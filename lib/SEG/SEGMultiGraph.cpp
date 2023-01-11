#include <circuitous/SEG/SEGMultiGraph.hpp>
#include <memory>
#include <sstream>

namespace circ{

std::vector< std::shared_ptr<circ::SEGNode> > circ::SEGNode::children()
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


std::unique_ptr< SEGGraph > circ_to_segg( CircuitPtr circuit )
{
    circ::inspect::LeafToVISubPathCollector subPathCollector;
    std::vector<std::shared_ptr<SEGNode>> nodes;
    int vi_counter = 0;

    for(VerifyInstruction* vi : circuit->attr< circ::VerifyInstruction >())
    {
        circ::pretty_print(vi);
        auto ltt_paths = collect::DownTree<constraint_opts_ts>();
        ltt_paths.Run(vi);
        int path_counter = 0;

        for(auto & path: ltt_paths.collected)
        {
            if(isa<AdviceConstraint>(path)) // these constraints are useless by themselves as others will obtain their values
                continue;
            auto prefix = "vi_" + std::to_string(vi_counter) + "_path" + std::to_string(path_counter) + "_node";
            UnfinishedProjection up(prefix, vi, path);
            up.fully_extend();

            auto node_gen = gap::graph::dfs< gap::graph::yield_node::on_open >( up.projection.get()->node );
            for(auto node : node_gen)
                nodes.push_back(node);

            for(auto copy : up.created_projections)
            {
                auto node_gen_proj = gap::graph::dfs< gap::graph::yield_node::on_open >( copy->projection->node );
                for(auto node : node_gen_proj)
                    nodes.push_back(node);
            }
            path_counter++;
        }
        vi_counter++;
    }
    auto g = std::make_unique<SEGGraph>(circuit);
    g->_nodes = nodes;
    return g;
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

SEGGraph::SEGGraph( Circuit::circuit_ptr_t &circuit ) : circuit( std::move(circuit) ) { }

void SEGGraph::print_semantics_emitter(decoder::ExpressionPrinter& ep)
{
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*this)) {
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
    calculate_costs();
    auto max_size_var = circ::decoder::Var("MAX_SIZE_INSTR");
    // this does too much somehow
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        int counter = 0;
        for(auto & [op, node ] : this->get_nodes_by_vi(vi) )
        {
            circ::expr_for_node( func_decls, name_storage, *node, stack,  &counter, max_size_var );
        }
    }

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
 * This function converts a SEGNode into a c++.
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
expr_for_node( std::unordered_map< circ::SEGNode, circ::decoder::FunctionDeclaration,
                                   circ::segnode_hash_on_get_hash, circ::segnode_comp_on_hash >
                   &func_decls,
               UniqueNameStorage &unique_names_storage, const SEGNode &node, decoder::Var stack,
               int* initial_stack_offset, decoder::Var max_size_stack )
{
    std::vector<decoder::Expr> args;
    std::vector<decoder::Expr> local_vars;
    decoder::StatementBlock setup;

    decoder::VarDecl pop_var(unique_names_storage.get_unique_var_name());
    local_vars.push_back(pop_var.value()); // requires to be first in the list when calling the visitor
    for(auto &c : node._nodes)
    {
        auto [ lval, set ] = expr_for_node( func_decls,
                                            unique_names_storage, *c, stack,
                                            initial_stack_offset, max_size_stack );
        local_vars.push_back(lval);
        setup.push_back(set);
    }
    auto stack_offset_var = decoder::Var("stack_offset", "int*");
    auto stack_offset_deref = decoder::Dereference(stack_offset_var);
    (*initial_stack_offset)++;
    auto pop_call = decoder::IndexVar(stack, stack_offset_deref);
    auto pop_assign = decoder::Statement(decoder::Assign(pop_var, pop_call));
    setup.push_back(pop_assign);
    setup.push_back(decoder::Statement(
        decoder::Assign(stack_offset_deref, decoder::Plus(stack_offset_deref, decoder::Int(1)))));

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
        fdb.retType("VisRetType")
            .name(unique_names_storage.get_unique_var_name().name)
            .arg_insert(decoder::VarDecl(decoder::Var("visitor", "const VisitorType& ")))
            .arg_insert(decoder::VarDecl(decoder::Var("stack", "const std::array<" + max_size_stack.name + "> &")))
           .arg_insert(decoder::VarDecl(stack_offset_var));
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
    decoder::VarDecl prev_func_call_var(unique_names_storage.get_unique_var_name());
    auto prev_func_call
        = decoder::FunctionCall( prev_declared_func->second.function_name,
                                 { decoder::Id( "visitor" ), stack, stack_offset_var } );
    auto prev_func_assign = decoder::Statement(decoder::Assign(prev_func_call_var, prev_func_call));
    setup.push_back(prev_func_assign);
    return {prev_func_call_var.value(), setup};
}

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

void SEGGraph:: calculate_costs()
{
    // calc inline cost and declare fd if possible
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*this))
    {
        node->inline_cost
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< circ::SEGNode > n ) {
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
                               []( int current, std::shared_ptr< circ::SEGNode > n )
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

using seg_projection = std::pair< InstructionProjection, std::shared_ptr< SEGNode > >;
struct ToExpressionVisitor : Visitor< ToExpressionVisitor >
{
    decoder::StatementBlock block;
    VerifyInstruction *vi;
    decoder::Id stack_counter;
    explicit ToExpressionVisitor( VerifyInstruction *vi, const decoder::Id &stackCounter ) :
        vi(vi), stack_counter( stackCounter )
    {
    }

    void visit( Advice *advice )
    {
        auto attached_op = get_op_attached_to_advice_in_vi( advice, vi );
        dispatch(attached_op);
    }

    void visit( Operation *op )
    {
        if(isa<Select>(op))
            std::cout << "but how? " << std::endl;
        auto lhs = decoder::Id( "stack[" + stack_counter + "++]" );
        block.push_back(
            decoder::Statement( decoder::Assign( lhs, decoder::Id( op->name() ) ) ) );
        op->traverse( *this );
    }

    void visit( Select *op )
    {
        for ( std::size_t choice = 1; choice < op->operands_size(); choice++ )
        {
            auto indx = decoder::Var( "select_id_" + std::to_string( op->id() ) );
            auto c_val = decoder::Int( static_cast< int64_t >( choice -1 ) );
            auto eq = decoder::Equal( indx, c_val );

            ToExpressionVisitor choice_child_vis( vi, stack_counter );
            choice_child_vis.visit( op->operand( choice ) );
            decoder::If ifs( eq, choice_child_vis.block );
            block.push_back( ifs );
        }
    }
};

void SEGGraph::print_decoder( decoder::ExpressionPrinter &ep )
{
    // print decoder
    for(circ::VerifyInstruction* vi : circuit->attr<circ::VerifyInstruction>())
    {
        circ::decoder::FunctionDeclarationBuilder fdb;
        fdb.name("decoder_for_vi" + std::to_string(vi->id()))
            .retType("void");
        decoder::Var stack_counter("stack_counter");
        decoder::Var stack_begin_counter("stack_counter_begin");
        fdb.body_insert_statement(decoder::Assign(decoder::VarDecl(stack_counter), decoder::Int(0)));
        fdb.body_insert_statement(decoder::Assign(decoder::VarDecl(stack_begin_counter), decoder::Int(0)));
        auto paths = this->get_nodes_by_vi(vi);


        std::multimap< Operation* , seg_projection> proj_groups;
        std::set< Operation* > proj_keys;
        for(auto p : this->get_nodes_by_vi(vi))
        {
            proj_groups.insert( { p.first.root_in_vi, p } );
            proj_keys.insert({p.first.root_in_vi});
        }

        for(auto key : proj_keys)
        {
            auto equal_range = proj_groups.equal_range(key);
            auto start_counter = stack_counter;
            fdb.body_insert_statement(decoder::Assign(stack_begin_counter, stack_counter));
            if( proj_groups.count(key) == 1)
            {
                auto p = (*proj_groups.find(key)).second;
                auto instr_proj = p.first;
                auto node = p.second;
                auto expr = get_expression_for_projection( vi, start_counter, stack_begin_counter, instr_proj, node );
                fdb.body_insert(expr);
            }
            else{
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
                 * if( cond2 == 1)
                 * ...
                 * if( cond2 == 1
                 * ...
                 *
                 * Our current approach is to check for if for all selects which have a choice
                 * if they make a choice for every possible combination of select choices made.
                 * The easiest way to do so is just count how many options we have for this emitted node.
                 */

                std::set<Select*> s;
                for(auto it = equal_range.first; it != equal_range.second; it++)
                {
                    for(auto x :it->second.first.select_choices)
                        s.insert(x.sel);
                }

                std::size_t target_count = 1;
                for(auto x : s)
                    target_count = target_count * (1 << x->bits);

                //TODO(sebas): and all segnodes need to be the same
                bool independent = proj_groups.count(key) == target_count;
                if(independent)
                {
                    auto p = (*proj_groups.find(key)).second;
                    auto instr_proj = p.first;
                    auto node = p.second;

                    ToExpressionVisitor vis(vi, "stack_counter");
                    vis.visit(key);
                    fdb.body_insert(vis.block);

                    auto sem_entry = func_decls.find( *node );
                    if ( sem_entry == func_decls.end() )
                        circ::unreachable() << "Trying to emit for a function which wasn't registered";

                    auto funcCall = decoder::FunctionCall( sem_entry->second.function_name,
                                                           { decoder::Id( "stack" ), stack_counter } );
                    fdb.body_insert( decoder::Statement( funcCall ) );

                }
                else
                {
                    for ( auto it = equal_range.first; it != equal_range.second; it++ )
                    {
                        // TODO(sebas): we should ideally have an uninitialized expr, so we can
                        // replace its value later.
                        decoder::Expr expr = decoder::Id( "" );
                        auto [ root, p ] = *it;
                        auto instr_proj = p.first;
                        auto node = p.second;
                        expr = get_expression_for_projection( vi, start_counter, stack_begin_counter, instr_proj,
                                                              node );

                        fdb.body_insert( expr );
                    }
                }
            }
        }

        ep.print(fdb.make());
    }
}

decoder::Expr SEGGraph::get_expression_for_projection( VerifyInstruction *vi,
                                                       decoder::Var stack_counter,
                                                       decoder::Var stack_begin_counter,
                                                       InstructionProjection &instr_proj,
                                                       std::shared_ptr< SEGNode > &node )
{
    auto stack_counter_for_call = stack_counter.name;
    auto start_op = instr_proj.root_in_vi;
    auto start_op_ptr = std::make_shared< nodeWrapper >( start_op );
    auto op_gen = non_unique_dfs_with_choices< gap::graph::yield_node::on_open >(
        start_op_ptr, instr_proj.select_choices, vi );
    auto node_gen = non_unique_dfs< gap::graph::yield_node::on_open >( node );

    bool has_choices = instr_proj.select_choices.size() > 0;

    decoder::StatementBlock block;
    if ( !has_choices )
    {
        for ( auto &[ op, nod ] : tuple_generators( op_gen, node_gen ) )
        {
            auto lhs = decoder::Id( "stack[" + stack_counter_for_call + "++]" );
            block.push_back(
                decoder::Statement( decoder::Assign( lhs, decoder::Id( op->op->name() ) ) ) );
        }
        auto sem_entry = func_decls.find( *node );
        if ( sem_entry == func_decls.end() )
            circ::unreachable() << "Trying to emit for a function which wasn't registered";

        auto funcCall
            = decoder::FunctionCall( sem_entry->second.function_name, { stack_begin_counter } );
        block.push_back( decoder::Statement( funcCall ) );
        return block;
    }

    decoder::StatementBlock b;

    for ( auto c : instr_proj.select_choices )
    {
        auto indx = decoder::Var( "select_id_" + std::to_string( c.sel->id() ) );
        auto choice = decoder::Int( static_cast< int64_t >( c.chosen_idx ) );
        auto eq = decoder::Equal( indx, choice );
        if ( !b.empty() )
        {
            auto first = b.back();
            b.pop_back();
            b.push_back( decoder::And( first, eq ) );
        }
        else
            b.push_back( eq );
    }

    for ( auto op : op_gen )
    {
        auto lhs = decoder::Id( "stack[" + stack_counter_for_call + "++]" );
        block.push_back(
            decoder::Statement( decoder::Assign( lhs, decoder::Id( op->op->name() ) ) ) );
    }
    auto sem_entry = func_decls.find( *node );
    if ( sem_entry == func_decls.end() )
        circ::unreachable() << "Trying to emit for a function which wasn't registered";

    auto funcCall
        = decoder::FunctionCall( sem_entry->second.function_name, { stack_begin_counter } );
    block.push_back( decoder::Statement( funcCall ) );

    decoder::If ifs( b, block );
    return ifs;
}

decoder::Var UniqueNameStorage::get_unique_var_name()
{
    counter ++;
    return decoder::Var( "generated_name_" + std::to_string(counter));
}

}


