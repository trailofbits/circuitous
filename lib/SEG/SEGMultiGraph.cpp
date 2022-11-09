//
// Created by sabastiaan on 06-10-22.
//
#include <circuitous/SEG/SEGMultiGraph.hpp>
#include <memory>
#include <sstream>

namespace circ{



int printmeeeeeee(){
    return 2;
}
std::vector< std::shared_ptr<circ::SEGNode> > circ::SEGNode::children()
{
    return _nodes;
//    for (auto node : _nodes) {
//        co_yield node;
////        for (auto child : node->children()) {
////            co_yield edge_type{ node, child };
////        }
//    }
}
std::string SEGNode::get_hash()
{
    std::stringstream ss;
    ss << std::to_string(_nodes.size());
    ss << "|";
    for( std::shared_ptr<SEGNode> n : _nodes )
        ss << n->get_hash();

    this->post_hash = ss.str();
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


void dfs_graph( const SEGGraph &g )
{
    for(auto n : gap::graph::dfs<gap::graph::yield_node::on_close>(g))
    {
        std::cout << n->get_hash() << std::endl;
    }

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
    for(Operation* vi : circuit->attr< circ::VerifyInstruction >())
    {
        circ::pretty_print(vi);
        auto ltt_paths = collect::DownTree<constraint_opts_ts>();
        ltt_paths.Run(vi);
        int path_counter = 0;

        for(auto & path: ltt_paths.collected)
        {
            graph_constructor_visitor convertor("vi_" + std::to_string(vi_counter) + "_path" + std::to_string(path_counter) + "_node");
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

}
