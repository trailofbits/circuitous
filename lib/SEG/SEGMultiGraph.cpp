#include "circuitous/Decoder/DecoderPrinter.hpp"
#include "circuitous/Decoder/GenerationHelpers.hpp"

#include <circuitous/SEG/SEGMultiGraph.hpp>
#include <memory>
#include <sstream>

namespace circ::decoder{

std::vector< std::shared_ptr<SEGNode> > SEGNode::children()
{
    return _nodes;
}

std::string SEGNode::get_hash() const
{
    std::stringstream ss;
    ss << std::to_string(_nodes.size());
    ss << "|";
    for( std::shared_ptr<SEGNode> n : _nodes )
        ss << n->get_hash();

    return ss.str();
}

SEGNode::SEGNode( const std::string &id ) : id( id ) { }
std::vector< std::shared_ptr<SEGNode> > SEGNode::parents() { return _parents; }


void SEGNode::add_child( SEGNode::node_pointer child )
{
    this->_nodes.push_back(child);
    child->_parents.push_back(std::make_shared<SEGNode>(*this));
}

void SEGNode::replace_all_nodes_by_id(std::shared_ptr<SEGNode> new_target, std::string target_id)
{

    std::transform(std::begin(_nodes), std::end(_nodes), std::begin(_nodes),
                    [&](auto& node_ptr) { if(node_ptr != nullptr && node_ptr->id == target_id ) return new_target; else return node_ptr; });


}


void specialize( std::map< std::string, Operation * > &specs, std::shared_ptr< SEGNode > node,
                 Operation *op )
{
    auto res_pair = specs.try_emplace(node->id, op);
    if(!res_pair.second) // not inserted so already exists
        node->specializeble = false;

    std::size_t c =0 ;
    for(auto & child : op->operands())
    {
        if(c == node->children().size())
        {
            std::cerr << "not isomorphic" << std::endl;
            return;


        }
        specialize(specs, node->children()[c], child);
        c++;
    }
}


void SEGGraphPrinter::print_semantics_emitter()
{
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(seg_graph)) {
        auto func = func_decls.find(*node);
        if(func != func_decls.end())
        {
            auto fd= *func;
            std::cout << "// called externally: " << fd.first.isRoot << " hash: " << fd.first.get_hash() << std::endl;
            ep.print(fd.second);
            std::cout << std::endl;
        }
    }

}

void SEGGraph::prepare()
{
    extract_all_seg_nodes_from_circuit();
    dedup(*this);
    calculate_costs();
}

decoder::FunctionCall print_SEGNode_tree( SEGNode &node, std::string stack_name, Operation *op )
{

    // TODO(Sebas) convert this to a double iterator walk
    std::size_t c = 0;
    std::vector<decoder::FunctionCall> fcs;
    for ( auto &child : op->operands() )
    {
        if(node.children().size() > c)
            circ::unreachable() << "fucked up ";

        std::shared_ptr<SEGNode> chi= node.children()[c];
        fcs.push_back(print_SEGNode_tree(*chi.get(), stack_name, child));
        c++;
    }
    std::vector<decoder::Expr> args;
    args.push_back(decoder::Var(stack_name));
    args.insert(args.end(), fcs.begin(), fcs.end());
    decoder::FunctionCall fc("apply_operation", args);
    return fc;
}

/*
 * This function converts a SEGNode into c++.
 *
 * For a given SEGNode n it will return two things:
 *      the variable name of an lvalue containing the result of the execution of n together with it's subtrees
 *      the "setup"/preable code that needs to be executed before the lvalue can be accessed
 *
 * If a node is costly enough that re-use is warrented we declare the preamble as a function decl
 *      and add the function to `func_decls`, the lvalue returned will call this newly generated function.
 *      this function checks whether `func_decls` already include such a function or not.
 */
//TODO(Sebas): change this to a register function and add an api that returns void
std::pair< decoder::Var, decoder::StatementBlock >
expression_for_seg_node( std::unordered_map< SEGNode, FunctionDeclaration,
                                   segnode_hash_on_get_hash, segnode_comp_on_hash >
                   &func_decls,
               UniqueNameStorage &unique_names_storage, const SEGNode &node, std::vector<decoder::Var> arg_names )
{
    std::vector<decoder::Expr> args;
    std::vector<decoder::Expr> local_vars;
    decoder::StatementBlock setup;

    decoder::Var operation = arg_names[0];
    auto last_index = 0;
    local_vars.push_back(operation); // requires to be first in the list when calling the visitor
    for(auto &c : node._nodes)
    {
        auto start = arg_names.begin() + last_index + 1;
        auto end = start + c->subtree_count;
        auto child_arg_names = std::vector< decoder::Var >( start, end );
        last_index = last_index + c->subtree_count;
        auto [ lval, set ] = expression_for_seg_node( func_decls, unique_names_storage, *c, child_arg_names );
        local_vars.push_back(lval);
        setup.push_back(set);
    }

    decoder::VarDecl visitor_call_var(unique_names_storage.get_unique_var_name());
    auto visitor_call = decoder::FunctionCall("visitor.call", local_vars);
    auto visitor_assign = decoder::Statement(decoder::Assign(visitor_call_var, visitor_call));
    setup.push_back(visitor_assign);

    /*
     * we either return a list of statements through a func decl, or just a single one for the current node
     */
    if(!node.fd)
        return {visitor_call_var.value(), setup};

    // has node fd intro
    if(!func_decls.contains(node))
    {
        decoder::FunctionDeclarationBuilder fdb;
        fdb.retType( decoder::Type( "VisRetType" ) )
            .name(unique_names_storage.get_unique_var_name().name)
            .arg_insert(decoder::VarDecl(decoder::Var("visitor", "const VisitorType& ")));
        for(auto arg_name : arg_names)
            fdb.arg_insert(decoder::VarDecl(arg_name));
        fdb.body_insert(setup);
        fdb.body_insert(decoder::Return(visitor_call_var.value()));
        func_decls.insert( { node, fdb.make() } );
    }

    /*
     * if we created a function than all code required to create the lvalue will be added
     * inside the newly created function, and the caller of the function should not duplicate that
     */
    setup.clear();

    auto prev_declared_func = func_decls.find( node );
    decoder::VarDecl prev_func_call_var( unique_names_storage.get_unique_var_name() );
    std::vector< decoder::Expr > call_args = { decoder::Id( "visitor" ) };
    call_args.insert( call_args.end(), arg_names.begin(), arg_names.end() );
    auto prev_func_call
        = decoder::FunctionCall( prev_declared_func->second.function_name, call_args );
    auto prev_func_assign
        = decoder::Statement( decoder::Assign( prev_func_call_var, prev_func_call ) );
    setup.push_back( prev_func_assign );
    return { prev_func_call_var.value(), setup };
}

struct advice_value_visitor : Visitor< advice_value_visitor >
{
    Advice *target;
    Operation *result = nullptr;
    bool result_is_left_side = false;

    explicit advice_value_visitor( Advice *target ) : target( target ) { }
    void visit( AdviceConstraint *ac )
    {
        check( ac->operands_size() == 2 )
            << "advice constraint does not contain 2 children";
        check( ac->operand( 0 ) != ac->operand( 1 ) )
            << "advice constraint points to same child twice, left id:"
            << ac->operand( 0 )->id() << " id right:" << ac->operand( 1 )->id();
        if ( ac->operand( 0 ) == target )
        {
            result_is_left_side = false;
            result = ac->operand( 1 );
        }

        if ( ac->operand( 1 ) == target )
        {
            result_is_left_side = true;
            result = ac->operand( 0 );
        }
    }

    void visit( Operation *op ) { op->traverse( *this ); }
};

Operation *get_op_attached_to_advice_in_vi( Advice *advice, VerifyInstruction *vi)
{
    advice_value_visitor vis(advice);
    vi->traverse(vis);
    check(vis.result != nullptr) << "could not find value";
    check(vis.result != advice) << "returned advice to itself";
    check(isa<Advice>(vis.result) == false) << "transitive advice found";
    return vis.result;
}

std::vector< std::shared_ptr<SEGNode> > SEGGraph::nodes() const
{
    return _nodes;
}

gap::generator< SEGGraph::edge_type > SEGGraph::edges() const {

        for (auto node : _nodes) {
            for (auto child : node->children()) {
                co_yield edge_type{ node, child };
            }
        }

}

void SEGGraph::remove_node( const SEGGraph::node_pointer& node )
{
    auto target_id = node->id;
    auto ids_match = [&](const std::shared_ptr<SEGNode>& node) { return node->id == target_id;};
    std::erase_if( _nodes, ids_match);
}

std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > >>
SEGGraph::get_nodes_by_vi( VerifyInstruction *vi )
{
    //TODO(sebas): build this once instead of building this per call
    std::vector< std::pair< InstructionProjection , std::shared_ptr< SEGNode > >> m;
    for(auto& n : nodes())
    {
        //TODO(sebas): convert this to find_if
        for(auto root : n->valid_for_contexts )
        {
            if(root.vi == vi && n->isRoot) // this root shares the correct vi, so we should copy it
                m.push_back( { root, n }  );
        }
    }

    circ::check(m.size() != 0) << "shit is empty? wut";
    return m;
}

void SEGGraph::calculate_costs()
{
    // calc inline cost and declare fd if possible
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*this))
    {
        node->inline_cost
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< SEGNode > n ) {
                                   if(n->fd == false)
                                       return current + n->inline_cost;
                                   else
                                       return current + 1;
                               } );
        if(node->inline_cost >= 2 || node->isRoot)
            node->fd = true;
    }

    // count nodes in subtree for every node
    // TODO(sebas): combine this with above
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>( *this ))
    {
        node->subtree_count
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< SEGNode > n )
                               { return current + n->subtree_count; } );
    }
}

int SEGGraph::get_maximum_vi_size()
{
    // get maximum number of nodes used for a single VI. This determines how large the stack should be
    std::vector<int> vals;
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        auto root_nodes_for_vi = this->get_nodes_by_vi(vi);
        int a = std::accumulate(root_nodes_for_vi.begin(), root_nodes_for_vi.end(), 0,
                                 [](auto left, auto &p) {
                                     return left + p.second->subtree_count;
                                 });
        vals.push_back(a);

    }
    // TODO(Sebas): safely return this element
    return *std::max_element(vals.begin(), vals.end());
}


struct ToExpressionVisitor : Visitor< ToExpressionVisitor >
{
    explicit ToExpressionVisitor( VerifyInstruction *vi, std::vector< decoder::Expr > &arguments,
                                  SelectStorage *select_storage,
                                  SimpleDecodeTimeCircToExpressionVisitor *decode_time_resolver,
                                  std::size_t argument_index = 0 ) :
        vi( vi ),
        arguments( arguments ), select_storage( select_storage ),
        decode_time_resolver( decode_time_resolver ), arg_index( argument_index )
    {
    }

    void visit( Advice *advice )
    {
        auto attached_op = get_op_attached_to_advice_in_vi( advice, vi );
        dispatch(attached_op);
    }

    void visit( Operation *op )
    {
        add_assignment_to_next_argument(op->name());
        op->traverse( *this );
    }

    void visit( Select *op )
    {
        /*
         * Currently select helpers is only allowed for selects which have only leafs.
         * Mainly due to simplicity of its implementation.
         */
//        selecty.get_function(op, vi);

        auto can_use_select_helper_function = true;
        for(std::size_t i = 1; i < op->operands_size() && can_use_select_helper_function; i++)
            can_use_select_helper_function = can_use_select_helper_function && op->operand(i)->operands_size() == 0;

        if(can_use_select_helper_function)
        {
            auto indx = decode_time_resolver->dispatch(op->selector());
            auto helper_call = select_storage->get_specialization(op, indx);
            add_assignment_to_next_argument(helper_call);
        }
        else
        {
            std::size_t first_taken_args = 0;
            bool is_first = true;
            for ( std::size_t choice = 1; choice < op->operands_size(); choice++ )
            {
                auto indx = decoder::Var( "select_id_" + std::to_string( op->id() ) );
                auto c_val = decoder::Int( static_cast< int64_t >( choice - 1 ) );
                auto eq = decoder::Equal( indx, c_val );

                ToExpressionVisitor choice_child_vis( vi, arguments, select_storage,
                                                      decode_time_resolver, arg_index );
                choice_child_vis.visit( op->operand( choice ) );
                if ( is_first )
                {
                    first_taken_args = choice_child_vis.taken_args;
                    is_first = false;
                }
                else
                {
                    check( first_taken_args == choice_child_vis.taken_args )
                        << "non-isomorphic selects are not allowed for independent emission";
                }
                decoder::If ifs( eq, choice_child_vis.output_expressions );
                output_expressions.push_back( ifs );
            }
            arg_index = arg_index + first_taken_args;
        }
    }

    decoder::StatementBlock output_expressions;

private:
    VerifyInstruction *vi;
    std::vector<decoder::Expr> arguments;
    SelectStorage* select_storage;
    SimpleDecodeTimeCircToExpressionVisitor* decode_time_resolver;
    std::size_t arg_index;
    std::size_t taken_args = 0;


    void add_assignment_to_next_argument(decoder::Expr value)
    {
        auto lhs = arguments[ arg_index++ ];
        taken_args++;
        output_expressions.push_back( decoder::Statement( decoder::Assign( lhs, value ) ) );
        arguments.push_back( lhs );
    }
};

/*
 * Generates one function per verify instruction that accepts instruction bytes.
 *
 * Preconditions:
 *      prepare has been called
 *
 * Postconditions:
 *      prints out the decoder functions into expression printer,
 *      probably you want to have called print_semantics beforehand
 */
decoder::FunctionDeclaration SEGGraphPrinter::print_decoder( VerifyInstruction *vi )
{
    /*
     * lets go, move all of the body inside dig
     */
    auto decoder_name = "decoder_for_vi" + std::to_string( vi->id() );
    circ::decoder::FunctionDeclarationBuilder fdb;
    fdb.name(decoder_name).retType( decoder::Type("void") );

    return fdb.make();
}

void DecodedInstrGen::get_expression_for_projection_with_indepenent_choices(
    std::multimap< Operation *, seg_projection > &proj_groups, Operation *key )
{
    auto p = (*proj_groups.find(key)).second;
    auto instr_proj = p.first;
    auto node = p.second;
    std::vector<decoder::Expr> func_args;
    std::vector<decoder::Expr> argument_names;

    for(auto i = 0 ; i < node->subtree_count; i++)
        argument_names.push_back(get_next_free_data_slot());
//    auto argument_names = name_storage.get_n_var_names(node->subtree_count, "const VisInputType &");
    func_args.push_back(decoder::Id("visitor"));
    func_args.insert(func_args.end(), argument_names.begin(), argument_names.end());

    ToExpressionVisitor vis( vi, argument_names, &seg_graph_printer->select_storage,
                             &decode_time_expression_creator );
    vis.visit( key );
    fdb_setup.body_insert( vis.output_expressions );

    auto func_decl = seg_graph_printer->get_func_decl( node );
    auto funcCall = decoder::FunctionCall( func_decl.function_name, func_args );
    fdb_visit.body_insert(decoder::Statement( funcCall ));
}

/*
 * Converts an assignment/projection of circIR into an Expression.
 * As there might have been select choices made during decode time inside the subtree of the circIR root for this projection.
 *
 *
 * Input: an Operation* acting as a root
 *  A set of select choices which could be possibly made
 *  A VerifyInstruction* denoting the context
 *  A SEGNode for which we can retrieve which function to call into
 *
 *  Output:
 *  If there have been choices made during decode time:
 *  if( <expression checking for choice_1> )
 *     ... entire sub_tree for choice 1
 *  if( <expression checking for choice_2> )
 *      ... entire sub_tree for choice 2
 *  if( <expression checking for choice_<last>> )
 *      ... entire sub_tree for choice <last>
 *
 *  If no choices have been made
 *  <just emit expressions>
 *
 *  This should be called if the decode select choices cannot be made independent.
 *  As this is made as a fall-back for instances which have decode-select choices
 *  which would result in non-isomorphic trees. The variable names are always freshly created.
 *  Resulting in a lot more variable names created then probably needed.
 */
void DecodedInstrGen::get_expression_for_projection(
    const std::pair< InstructionProjection, std::shared_ptr< SEGNode > >& instr_node_pair)
{
    auto instr_proj = instr_node_pair.first;
    auto node = instr_node_pair.second;
    auto start_op = instr_proj.root_in_vi;
    auto start_op_ptr = std::make_shared< nodeWrapper >( start_op );
    auto op_gen = non_unique_dfs_with_choices< gap::graph::yield_node::on_open >(
        start_op_ptr, instr_proj.select_choices, vi );
    auto node_gen = non_unique_dfs< gap::graph::yield_node::on_open >( node );

    bool has_choices = instr_proj.select_choices.size() > 0;

    decoder::StatementBlock block;
    std::vector<decoder::Expr> arguments;
    arguments.push_back(decoder::Id("visitor"));
    if ( !has_choices )
    {
        auto func_decl = seg_graph_printer->get_func_decl(node);
        for ( auto &[ op, nod ] : tuple_generators( op_gen, node_gen ) )
        {
            auto lhs = get_next_free_data_slot();
            auto rhs = decoder::Id( op->op->name());

            member_initializations.push_back( decoder::Assign( lhs, rhs ) );
            arguments.push_back(lhs);
        }

        check( arguments.size() == func_decl.args.size() )
            << "calls function with incorrect number of arguments. given arguments"
            << arguments.size() << " expected: " << func_decl.args.size();
        auto func_call = decoder::FunctionCall( func_decl.function_name, arguments );
        fdb_visit.body_insert( func_call );
        return;
    }

    decoder::StatementBlock guard_conditions;

    for ( auto c : instr_proj.select_choices )
    {
        auto indx = decode_time_expression_creator.visit(c.sel->selector());
        auto choice = decoder::Int( static_cast< int64_t >( c.chosen_idx ) );
        auto eq = decoder::Equal( indx, choice );
        if ( !guard_conditions.empty() )
        {
            auto first = guard_conditions.back();
            guard_conditions.pop_back();
            guard_conditions.push_back( decoder::And( first, eq ) );
        }
        else
            guard_conditions.push_back( eq );
    }

    for ( auto op : op_gen )
    {
        auto lhs = get_next_free_data_slot();
        auto rhs = decoder::Id( op->op->name() );
        member_initializations.push_back( decoder::Statement ( decoder::Assign( lhs, rhs ) ) );
        arguments.push_back(lhs);
    }

    auto func_decl = seg_graph_printer->get_func_decl( node );
    check( arguments.size() == func_decl.args.size() )
        << "calls function with incorrect number of arguments";
    auto func_call = decoder::FunctionCall( func_decl.function_name, arguments );


//    decoder::If guarded_setup( guard_conditions, block );
    decoder::If guarded_visit( guard_conditions, func_call );
    \

//    fdb_setup.body_insert_statement( guarded_setup );
    fdb_visit.body_insert_statement( guarded_visit );
}

void SEGGraphPrinter::print_helper_functions()
{
    os << "// Extract a value from `a:b`.\n"
          "template <uint64_t kOffsetBits, uint64_t kSizeBits>\n"
          "static uint64_t LoadFromPair(uint64_t a, uint64_t b) {\n"
          "  static_assert((kOffsetBits + kSizeBits) <= 128u);\n"
          "\n"
          "  static constexpr uint64_t kBitMask =\n"
          "      kSizeBits == 64 ? ~0ull : (1ull << kSizeBits) - 1ull;\n"
          "\n"
          "  // Extracting from `b`.\n"
          "  if constexpr (kOffsetBits >= 64u) {\n"
          "    return LoadFromPair<kOffsetBits - 64u, kSizeBits>(b, 0);\n"
          "\n"
          "  // Extracting from `a`.\n"
          "  } else if constexpr ((kOffsetBits + kSizeBits) <= 64u) {\n"
          "    return (a >> kOffsetBits) & kBitMask;\n"
          "\n"
          "  // Extract `b:a`, i.e. high bits from `a` to form the low bits of our\n"
          "  // extracted value, combined with the low bits from `b` to form the high\n"
          "  // bits of our extracted value.\n"
          "  } else {\n"
          "    static constexpr uint64_t kNumABits = 64u - kOffsetBits;\n"
          "    static constexpr uint64_t kNumBBits = kOffsetBits + kSizeBits - 64u;\n"
          "    static constexpr uint64_t kLowBitMask = (1ull << kNumABits) - 1ull;\n"
          "    static constexpr uint64_t kHighBitMask = (1ull << kNumBBits) - 1ull;\n"
          "\n"
          "    const uint64_t low_bits = (a >> kOffsetBits) & kLowBitMask;\n"
          "    const uint64_t high_bits = b & kHighBitMask;\n"
          "    const uint64_t val = low_bits | (high_bits << kNumABits);\n"
          "    return val;\n"
          "  }\n"
          "}" << std::endl;
}

void SEGGraphPrinter::generate_function_definitions()
{
    // this does too much somehow
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        for(auto & [op, node ] : seg_graph.get_nodes_by_vi(vi) )
        {
            auto argument_names = name_storage.get_n_var_names(node->subtree_count, "const VisInputType &") ;
            expression_for_seg_node( func_decls, name_storage, *node, argument_names );
        }
    }
}
void SEGGraphPrinter::print_select_storage_helper_functions()
{
    for(auto fdb : select_storage.get_functions_for_select())
        ep.print(fdb);
}

/*
 * Check for if the select choices are made independent, because if so we can
 * emit code in for each choice separately.
 * i.e transform
 * if(cond_1 == 1 && cond_2 == 1)
 * .... <insert combinatiorial version of (cond_1 == x && cond_2 == y)
 * to
 * if (cond_1 == 1 )
 * ...
 * if( cond2 == 2 )
 * ...
 *
 * if( cond2 == 1 )
 * ...
 * if( cond2 == 1 )
 * ...
 *
 * We have two requirements:
 *  1. All choices for a select need to be isomorphic
 *      If this is not the case than the next variable we write to will be dependent on a decode
 * time value
 *  2. The emission of a select choice may not be dependent on another.
 *      For instance lets say we have 3 select choices A, B and C. With A being a parent to both and B and C have 2 children that are also leafs
 *      This example should produce one field/variable but the value it gets is dependent on A regardless.
 *
 * For this it suffices to just check 2 things:
 *  1. No select is a child of another select
 *  2. All nodes use the same SEG Node
 */
bool DecodedInstrGen::selects_emission_locations_are_constant(const std::multimap< Operation* , seg_projection>& proj_groups, Operation* key)
{
    std::shared_ptr< SEGNode > comparison = nullptr;
    auto equal_range = proj_groups.equal_range( key );
    for ( auto it = equal_range.first; it != equal_range.second; it++ )
    {

        if ( comparison == nullptr )
        {
            comparison = it->second.second;
            // we just need to check this once as this works on an operation level
            if( operation_has_nested_select( it->second.first ))
                return false;
        }
        else
        {
            if ( comparison->get_hash() != it->second.second->get_hash() )
                return false;
        }
    }

    return true;
}

decoder::FunctionDeclaration
SEGGraphPrinter::get_func_decl( std::shared_ptr< SEGNode > node )
{
    auto sem_entry = func_decls.find( *node );
    if ( sem_entry == func_decls.end() )
        circ::unreachable() << "Trying to emit for a function which wasn't registered";
    return sem_entry->second;
}

void SEGGraph::extract_all_seg_nodes_from_circuit()
{
    circ::inspect::LeafToVISubPathCollector subPathCollector;
    std::vector< std::shared_ptr< SEGNode > > nodes;
    int vi_counter = 0;

    for ( VerifyInstruction *vi : circuit->attr< circ::VerifyInstruction >() )
    {
        auto ltt_paths = collect::DownTree< constraint_opts_ts >().run( vi );
        int path_counter = 0;

        for ( auto &path : ltt_paths.collected )
        {
            // these constraints are useless by themselves as others will use them instead
            if ( isa< AdviceConstraint >( path ) )
                continue;

            auto prefix = "vi_" + std::to_string( vi_counter ) + "_path"
                          + std::to_string( path_counter ) + "_node";
            UnfinishedProjection up( prefix, vi, path );
            up.fully_extend();
            save_nodes_from_projection(nodes, up);
            path_counter++;
        }
        vi_counter++;
    }
    _nodes = nodes;
}

void SEGGraph::save_nodes_from_projection( std::vector< std::shared_ptr< SEGNode > > &nodes,
                                           UnfinishedProjection &up ) const
{
    auto node_gen = gap::graph::dfs< gap::graph::yield_node::on_open >(
        up.projection.get()->node );
    for ( auto node : node_gen )
        nodes.push_back( node );

    for ( auto copy : up.created_projections )
    {
        auto node_gen_proj = gap::graph::dfs< gap::graph::yield_node::on_open >(
            copy->projection->node );
        for ( auto node : node_gen_proj )
            nodes.push_back( node );
    }
}

decoder::Var UniqueNameStorage::get_unique_var_name(decoder::Id type_name)
{
    counter ++;
    return decoder::Var( "generated_name_" + std::to_string(counter), type_name);
}

std::vector<decoder::Var> UniqueNameStorage::get_n_var_names(int amount_of_names, decoder::Id type_name )
{
    std::vector<decoder::Var> vars;
    for(auto i = 0; i < amount_of_names; i++)
    {
        counter ++;
        vars.push_back(
            decoder::Var( "generated_name_" + std::to_string( counter ), type_name ) );
    }
    return vars;
}


Operation *select_index( Select *op ) { return op->operand(0); }

std::size_t SelectStorage::hash_select_targets( Select *sel )
{
    print::PrettyPrinter pp;
    return std::hash< std::string > {}( pp.Hash( select_values( sel ) ) );
}

bool are_trees_isomorphic( const std::vector< Operation * > &ops )
{
    if ( ops.size() < 2 )
        return true;

    print::CompactPrinter cp;
    std::string first_hash = cp.Hash( ops[ 0 ] );
    return std::all_of( ops.begin() + 1, ops.end(),
                        [ & ]( Operation *op ) {
                            std::cout << "comparing: " << cp.Hash(op) << " = " << first_hash << std::endl;
                            return cp.Hash( op ) == first_hash; } );
}




void SelectStorage::register_select( Select *sel )
{
//    decoder::FunctionDeclarationBuilder fdb;
//    print::PrettyPrinter pp;
//
//    if ( !are_trees_isomorphic( select_values(sel) ) )
//        circ::unreachable() << "adding select helper for non-isomorphic select";
//
//    auto values_hash = hash_select(sel);
//
//
//    fdb.name("select_" + std::to_string(values_hash));
//    //TODO get type of
//    decoder::Var select_index = decoder::Var("index", "int:" + std::to_string(sel->bits));
//    fdb.arg_insert(decoder::VarDecl(select_index));
//
//
//    for(std::size_t i = 0; i < select_values( sel ).size(); i++ )
//    {
//        std::vector< decoder::Expr > block;
//        auto start_op_ptr = std::make_shared< nodeWrapper >( sel->operand(i+1) );
//        for(auto x : gap::graph::dfs<gap::graph::yield_node::on_open>(start_op_ptr))
//        {
//            block.push_back(decoder::Id(x->op->name()));
//        }
//
//        decoder::Tuple tuple( block.size() );
//
//        //TODO turn if else into switch
//        auto if_expr = decoder::If(
//            decoder::Equal( select_index, decoder::Int( static_cast< int64_t >( i ) ) ),
//            decoder::Return( tuple.Construct( block ) ) );
//        fdb.body_insert( if_expr );
//        fdb.retType( tuple.get_type() );
//    }
//
//    selects.insert( std::make_pair( values_hash, fdb.make() ) );
}

std::vector< decoder::FunctionDeclaration > SelectStorage::get_functions_for_select()
{
    std::vector< decoder::FunctionDeclaration > funcs;

    for(auto & k : selects)
        funcs.push_back(k.second);

    return funcs;
}

decoder::FunctionCall SelectStorage::get_specialization( circ::Select *sel, decoder::Expr index )
{
    auto func = selects.at( hash_select(sel));
    return decoder::FunctionCall( func.function_name, { index } );
}

std::size_t hash_select( Select *op )
{
    print::PrettyPrinter pp;
    return std::hash<std::string>{}( pp.Hash(select_values(op)) );
}

std::vector< Operation * > select_values( Select *op )
{
    std::vector< Operation * > results;
    auto first = false;
    //TODO(sebas): idiomize this
    for (auto o : op->operands() )
    {
        if(!first)
            first = true;
        else
            results.push_back(o);
    }
    return results;
}

struct HasSelectInProjectionVisitor : AdviceResolvingVisitor< HasSelectInProjectionVisitor >
{
    using AdviceResolvingVisitor::AdviceResolvingVisitor;
    void visit( Select *op ) { found_select = true; }
    void visit( Operation *op ) { op->traverse( *this ); }

    bool found_select = false;
};


struct HasSubSelectInProjectionVisitor : AdviceResolvingVisitor< HasSubSelectInProjectionVisitor >
{
    using AdviceResolvingVisitor::AdviceResolvingVisitor;
    void visit( Select *op ) {
        HasSelectInProjectionVisitor vis( vi );
        op->traverse( vis );
        if(vis.found_select)
            found_sub_select = true;
    }
    void visit( Operation *op ) { op->traverse( *this ); }

    bool found_sub_select = false;
};

bool operation_has_nested_select( const InstructionProjection &projection )
{
    HasSubSelectInProjectionVisitor vis( projection.vi );
    vis.dispatch( projection.root_in_vi );
    return vis.found_sub_select;
}

void DecodedInstrGen::create()
{
    /*
     * There are multiple operations which are considered we would need to emit semantics for
     * However some of them might require a decode-time choice.
     * We keep track of the roots in both a set and multimap since multimap
     * does not expose a nice interface to just loop over the keys
     */
    std::multimap< Operation* , seg_projection> proj_groups;
    std::set< Operation* > proj_keys;
    for(auto p : seg_graph_printer->seg_graph.get_nodes_by_vi(vi))
    {
        proj_groups.insert( { p.first.root_in_vi, p } );
        proj_keys.insert({p.first.root_in_vi});
    }

    for ( auto key : proj_keys )
    {
        // checks for there exists multiple different execution trees for this given root node
        // If not than we up the emit the subtree with a trivial walk
        if ( proj_groups.count( key ) == 1 )
        {
            get_expression_for_projection(( *proj_groups.find( key ) ).second);
            continue ;
        }

        // Here we have execution path that depend on decode time choices
        if ( selects_emission_locations_are_constant( proj_groups, key ) )
        {
            get_expression_for_complete_seg_isomorphisms( proj_groups, key );
            continue;
        }

        // We have to emit all execution paths guarded by all required choices
        auto projections_for_same_root = proj_groups.equal_range( key );
        for ( auto proj_it = projections_for_same_root.first;
              proj_it != projections_for_same_root.second; proj_it++ )
        {
            auto t = ( *proj_it ).second;
            get_expression_for_projection( ( *proj_groups.find( key ) ).second );
        }


    }
}

decoder::IndexVar DecodedInstrGen::get_instr_data( std::size_t at_index )
{
    return decoder::IndexVar( data_array, decoder::Int( static_cast< int64_t >( at_index ) ) );
}

decoder::VarDecl DecodedInstrGen::get_next_free_data_slot()
{
    auto index = std::to_string(size++);
    auto var = decoder::Var(member_variable_prefix + index);
    return decoder::VarDecl(var);
}
}
