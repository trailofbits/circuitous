#pragma once
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>
#include <circuitous/Decoder/DecodeAST.hpp>

namespace circ {
    //TODO(sebas): move this to gap
    template <typename S, typename T>
    gap::recursive_generator< std::pair< S, T > >
    tuple_generators( gap::recursive_generator< S > &s,
                      gap::recursive_generator< T > &t )
    {
        auto gen_s = s.begin();
        auto gen_t = t.begin();

        while(true){
            auto is_end_s = gen_s == s.end();
            auto is_end_t = gen_t == t.end();

            if(is_end_s != is_end_t)
                unreachable() << "non-isomorphic generators left:" << is_end_s <<  " right " << is_end_t;

            if(is_end_s || is_end_t)
                break;

            co_yield { *gen_s, *gen_t };

            gen_s++;
            gen_t++;
        }
    }


    template< gap::graph::yield_node when, typename node_pointer >
        requires gap::graph::node_like< typename node_pointer::element_type >
    gap::recursive_generator< node_pointer > non_unique_dfs(node_pointer root) {

        if constexpr (when == gap::graph::yield_node::on_open) {
            co_yield root;
        }

        for (auto child : root->children()) {
            co_yield non_unique_dfs< when >(child);
        }

        if constexpr (when == gap::graph::yield_node::on_close) {
            co_yield root;
        };
    }



enum class EdgeType{
    TBD,
    Inline,
    FunctionCall,
    FunctionPointerCall
};

template<typename NodeType>
struct Edge{
    NodeType* source;
    NodeType* target;

    EdgeType type;

};


struct UniqueNameStorage
{
    decoder::Var get_unique_var_name();
    std::vector<decoder::Var> names;
    int counter =0 ;
};


struct SEGNode
{
    using node_pointer   = std::shared_ptr<SEGNode>;
    using child_type   = node_pointer;
    SEGNode( const std::string &id );
    std::vector< child_type > children();
    std::vector< child_type > parents();
    std::vector< child_type > _nodes;
    std::vector< child_type > _parents;


    /*
     * should this add for both? I Think so
     */
    void add_parent(node_pointer parent);
    void add_child(node_pointer child);

    void replace_all_nodes_by_id(node_pointer new_target, std::string target_id);

    bool isRoot = false;
    bool specializeble = true;

    // vi is a "tag" that indicates which projection of a circir tree was used when linking it to the given Op*
    std::multimap<VerifyInstruction*, Operation*> roots;
//    std::vector<Operation*> root_operations;
    std::string id;
    //TODO(seb): figure out if this can be done with overriding a hash function from elsewhere.
    std::string get_hash() const;  // no idea if overriding hash here is a good idea yet.

    int inline_cost = 0;
    int subtree_count = 0; // count of all it's children and their children, used to calculate the size of pre-allocation needed
    bool fd = false;

};


struct segnode_hash_on_get_hash
{
    std::size_t operator()(const SEGNode & node) const noexcept
    {
        auto ret_val = std::hash<std::string>{}(node.get_hash());
        return ret_val;
    }
};


struct segnode_comp_on_hash
{
    bool operator()( const SEGNode & left, const SEGNode & right ) const
    {
        return left.get_hash() == right.get_hash();
    }
};


template <typename T>
struct SEGEdge {
    using source_type = T;
    using target_type = T;

    T _source;
    T _target;

    source_type source() { return _source; }
    target_type target() { return _target; }
};


struct SEGGraph
{

    using node_type = SEGNode;
    using node_pointer = node_type::node_pointer;
    using edge_type = SEGEdge<node_pointer>;
    using CircuitPtr = Circuit::circuit_ptr_t;

    explicit SEGGraph( CircuitPtr &circuit );
    std::vector<node_pointer> nodes() const;
    gap::generator<edge_type> edges() const;

    std::vector<node_pointer> _nodes;

    void remove_node(const node_pointer& node);

    std::vector<std::pair<Operation*, std::shared_ptr<SEGNode>>> get_nodes_by_vi(VerifyInstruction* vi);

    SEGGraph copy();


    void prepare();
    void print_semantics_emitter(decoder::ExpressionPrinter& ep);
    void print_decoder(decoder::ExpressionPrinter& ep);
    int get_maximum_vi_size();

private:
    // calculate costs of the nodes for emission
    void calculate_costs();

    CircuitPtr circuit;
    std::unordered_map< SEGNode, decoder::FunctionDeclaration,
                        segnode_hash_on_get_hash, segnode_comp_on_hash >
        func_decls;
    UniqueNameStorage name_storage;
    decoder::Var stack = decoder::Var( "stack" );
};




void specialize(std::map<std::string, Operation*>& specs, std::shared_ptr<SEGNode> node , Operation* op );


class graph_constructor_visitor: UniqueVisitor<graph_constructor_visitor>
{
    bool isRoot = true;
    int node_counter = 0;
    std::string vi_start; // from root?
    VerifyInstruction* vi;

public:
    std::vector<std::shared_ptr<SEGNode>> result_set;
    graph_constructor_visitor( const std::string &viStart, VerifyInstruction *vi ) : vi_start( viStart ), vi( vi )
    { }


    std::shared_ptr<SEGNode> visit(Operation * op)
    {
        std::string s = vi_start + std::to_string(node_counter);
        auto new_node = std::make_shared<SEGNode>(s);

        if(isRoot){
            isRoot = false;
            new_node->isRoot = true;
            new_node->roots.insert({vi, op});
        }

        node_counter++;
        result_set.push_back(new_node);
        for(auto & c : op->operands())
            new_node->add_child(visit(c));
        return new_node;
    }
};


std::unique_ptr<SEGGraph> circ_to_segg( CircuitPtr circuit);

template < gap::graph::graph_like g >
void absorb_node_into_an_existing_root( g &graph, const SEGGraph::node_pointer &to_hash );

template<gap::graph::graph_like g>
void dedup(g& graph)
{
    std::map<std::string, std::shared_ptr<SEGNode>> seen_hash;
    for(auto& to_hash : gap::graph::dfs<gap::graph::yield_node::on_close>(graph))
    {
        auto pair_iter_inserted = seen_hash.try_emplace(to_hash->get_hash(), to_hash);
        if(pair_iter_inserted.second == false ){ // item already existed hash replace others now
            auto pre_existinging_hashed_node = (*pair_iter_inserted.first).second;

            bool foundEdge = false;
            // find parent of target that was already hashed and replace it
            for(auto &e : graph.edges())
            {
                // we want to replace it, and we don't replace the one with the one want to replace with
                if(e.target()->id == to_hash->id && e.target()->id != pre_existinging_hashed_node->id){
                    foundEdge = true;
                    e.source()->replace_all_nodes_by_id( pre_existinging_hashed_node, to_hash->id);
                    graph.remove_node(to_hash);
                    break;
                }
            }
            // must be parent of a subtree, now replace those
            if(!foundEdge){
                if(!to_hash->isRoot)
                    unreachable() << "all nodes require an incoming edge or be marked as root.";
                absorb_node_into_an_existing_root( graph, to_hash );
            }
        }
    }
}
template < gap::graph::graph_like g >
void absorb_node_into_an_existing_root( g &graph, const SEGGraph::node_pointer &to_hash )
{ // two nodes which are both roots, and have same hashes can be merged
    for(auto & to_merge_with : graph.nodes())
    {
        if ( to_hash->id != to_merge_with->id && to_merge_with->isRoot
             && to_hash->isRoot
             && to_hash->get_hash() == to_merge_with->get_hash() )
        {
            to_merge_with->roots.merge(to_hash->roots);
            graph.remove_node(to_hash);
            break;
        }
    }
}

decoder::FunctionCall print_SEGNode_tree( SEGNode& node, std::string stack_name , Operation* op);

struct nodeWrapper
{
    using node_pointer = std::shared_ptr< nodeWrapper >;
    using child_type   = node_pointer;
    Operation* op;

    nodeWrapper( Operation *op ) : op( op ) { }
    gap::generator< child_type > children()
    {
        for(auto & n : op->operands())
            co_yield std::make_shared<nodeWrapper>(nodeWrapper(n));
    }

    gap::generator< child_type > children() const
    {
        for(auto & n : op->operands())
            co_yield std::make_shared<nodeWrapper>(nodeWrapper(n));
    }
};

template< gap::graph::node_like node_t_ >
struct edge_t {
    using node_type    = node_t_;
    using node_pointer = typename node_type::node_pointer;

    using source_type  = node_pointer;
    using target_type  = node_pointer;

    source_type source() const { return _src; }
    target_type target() const { return _dst; }

    node_pointer _src, _dst;
};



std::pair< decoder::Var, decoder::StatementBlock >
expr_for_node( std::unordered_map< SEGNode, decoder::FunctionDeclaration,
                                   segnode_hash_on_get_hash, segnode_comp_on_hash >
                   &func_decls,
               UniqueNameStorage &unique_names_storage, const SEGNode &node, decoder::Var stack,
               int* initial_stack_offset, decoder::Var max_size_stack );

}
