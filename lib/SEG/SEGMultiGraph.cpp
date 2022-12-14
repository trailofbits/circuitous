//
// Created by sabastiaan on 06-10-22.
//
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

//    this->post_hash = ss.str();
    return ss.str();
}

std::string SEGNode::print_hash()
{
    std::stringstream ss;
    ss << std::to_string(_nodes.size());
    if(specializeble)
        ss << "*";
    ss << "|";
    for( std::shared_ptr<SEGNode> n : _nodes )
        ss << n->get_hash();

    this->post_hash = ss.str();
    return ss.str();
}
SEGNode::SEGNode( const std::string &id ) : id( id ) { }
std::vector< std::shared_ptr<SEGNode> > SEGNode::parents() { return _parents; }


void SEGNode::add_parent( SEGNode::node_pointer parent )
{
    this->_parents.push_back(parent);
    parent->_nodes.push_back(std::make_shared<SEGNode>(*this));
}


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


void print_nodes_graph( const SEGGraph &g)
{
    for(auto n : gap::graph::dfs<gap::graph::yield_node::on_close>(g))
    {
        std::cout << n->id  << ": " << n->post_hash << std::endl;
        std::cout << "parents:";
        for(auto x : n->parents())
            std:: cout << " ," << x->id;
        std::cout << std::endl;
    }

}
std::unique_ptr< SEGGraph > circ_to_segg( circ::Circuit *circuit )
{
    circ::inspect::LeafToVISubPathCollector subPathCollector;
    std::vector<std::shared_ptr<SEGNode>> nodes;
    int vi_counter = 0;
//    for(auto & vi : circuit->attr<circ::VerifyInstruction>(circuit))
    for(VerifyInstruction* vi : circuit->attr< circ::VerifyInstruction >())
    {
        circ::pretty_print(vi);
        auto ltt_paths = collect::DownTree<constraint_opts_ts>();
        ltt_paths.Run(vi);
        int path_counter = 0;

        for(auto & path: ltt_paths.collected)
        {
            graph_constructor_visitor convertor("vi_" + std::to_string(vi_counter) + "_path" + std::to_string(path_counter) + "_node", vi);
            convertor.visit(path);
            nodes.insert(nodes.end(), convertor.result_set.begin(), convertor.result_set.end());
            path_counter++;
        }
        vi_counter++;
    }
    auto g = std::make_unique<SEGGraph>();
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

decoder::FunctionCall print_SEGNode_tree( SEGNode &node, std::string stack_name, Operation *op )
{

    /* convert this to a double itterator walk */
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
std::pair< decoder::Var, decoder::StatementBlock > expr_for_node(
//    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration> &func_decls,
    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration, circ::segnode_hash_on_get_hash, circ::segnode_comp_on_hash> &func_decls,
    UniqueNameStorage &unique_names_storage,
    const SEGNode &node,
    decoder::Var stack )
{
    std::vector<decoder::Expr> args;
    std::vector<decoder::Expr> local_vars;
    decoder::StatementBlock setup;

    decoder::VarDecl pop_var(unique_names_storage.get_unique_var_name());
    local_vars.push_back(pop_var.value()); // requires to be first in the list when calling the visitor
    for(auto &c : node._nodes)
    {
        auto [lval, set] = expr_for_node( func_decls, unique_names_storage, *c, stack );
        local_vars.push_back(lval);
        setup.push_back(set);
    }

    auto pop_call = decoder::FunctionCall(stack.name + ".pop", {});
    auto pop_assign = decoder::Statement(decoder::Assign(pop_var, pop_call));
    setup.push_back(pop_assign);

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
            .arg_insert(decoder::VarDecl(decoder::Var("stack", "const std::stack &")))
            .arg_insert(decoder::VarDecl(decoder::Var("visitor", "const VisitorType& ")));
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
    auto prev_func_call = decoder::FunctionCall(prev_declared_func->second.function_name, { stack, decoder::Id("visitor") } );
    auto prev_func_assign = decoder::Statement(decoder::Assign(prev_func_call_var, prev_func_call));
    setup.push_back(prev_func_assign);
    return {prev_func_call_var.value(), setup};
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

std::multimap< VerifyInstruction *, std::shared_ptr< SEGNode > >
SEGGraph::get_nodes_by_vi( VerifyInstruction *vi )
{
    //TODO(sebas): build this once instead of building this per call
    std::multimap< VerifyInstruction *, std::shared_ptr< SEGNode > > m;
    for(auto& n : nodes())
        if(n->isRoot && n->roots.contains(vi))
            m.insert( { vi, n } );
    return m;
}

decoder::Var UniqueNameStorage::get_unique_var_name()
{
    counter ++;
    return decoder::Var( "generated_name_" + std::to_string(counter));
}

}


