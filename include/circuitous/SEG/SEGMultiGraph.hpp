#pragma once
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>
#include <circuitous/Decoder/DecodeAST.hpp>



namespace circ {


class UniqueMultiGraph{
    /*
     * create in two steps:
     * 1. multi graph
     *      for all parents: change nodes with same hash into same with named edges
     * 2. unique
     *      all hashes which are equal are merged
     *
     *  Does merging matter if we do it from leafs or from deepest level ?
     *
     * Probably most efficient to do unique
     * feels like we can apply cata fusion here to merge the calculation in one
     */


    /*
     * Start out taking a circuit and copying
     */

};

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

    std::multimap<VerifyInstruction*, Operation*> roots;
//    std::vector<Operation*> root_operations;
    std::string post_hash;
    std::string id;
    //TODO(seb): figure out if this can be done with overriding a hash function from elsewhere.
    std::string get_hash() const;  // no idea if overriding hash here is a good idea yet.
    std::string print_hash();

    int inline_cost = 0;
    int subtree_count = 0; // count of all it's children and their children, used to calculate the size of pre-allocation needed
    bool fd = false;

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

    std::vector<node_pointer> nodes() const;
    gap::generator<edge_type> edges() const;

    std::vector<node_pointer> _nodes;

    void remove_node(const node_pointer& node);

    std::multimap<VerifyInstruction*, std::shared_ptr<SEGNode>> get_nodes_by_vi(VerifyInstruction* vi);

    SEGGraph copy();
};


struct segnode_hash_on_get_hash
{
    std::size_t operator()(const circ::SEGNode & node) const noexcept
    {
        auto ret_val = std::hash<std::string>{}(node.get_hash());
        return ret_val;
    }
};


struct segnode_comp_on_hash
{
    bool operator()( const circ::SEGNode & left, const circ::SEGNode & right ) const
    {
        return left.get_hash() == right.get_hash();
    }
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


std::unique_ptr<SEGGraph> circ_to_segg(circ::Circuit* circuit);


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
                    std::cout << "ERROR THIS SHIT " << std::endl;

//                std::cout << "merging roots" << std::endl;
                // two nodes which are both roots, and have same hashes can be merged
                for(auto & to_merge_with : graph.nodes())
                {

                    if(to_hash->id != to_merge_with->id && to_merge_with->isRoot && to_hash->isRoot && to_hash->get_hash() == to_merge_with->get_hash())
                    {
                        to_merge_with->roots.merge(to_hash->roots);
                        graph.remove_node(to_hash);
                        break;
                    }
                }
            }
        }
    }
}

decoder::FunctionCall print_SEGNode_tree( SEGNode& node, std::string stack_name , Operation* op);

struct nodeWrapper
{
    using node_pointer = std::shared_ptr< nodeWrapper >;
    using child_type   = node_pointer;
    circ::Operation* op;

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



struct tree_t {
    using node_type = nodeWrapper;
    using edge_type = edge_t< node_type >;

    using node_pointer = std::shared_ptr<nodeWrapper>;

    tree_t( const node_type &root ) : root( root ) { }

    gap::generator< node_pointer > nodes() const {
        for (auto& ch : root.children())
            co_yield std::make_shared<node_type>(ch->op);
    }

    gap::generator< edge_type > edges() const {

        for (auto node : nodes()) {
            co_yield edge_type{node, node};
//            for (auto child : node->children()) {
//                co_yield edge_type{ node, std::shared_ptr<node_type>(child) };
//            }
        }
    }

    node_type root;
};

//
//struct CombinedNode{
//    // vector of all operations associated with this node, if 1 we can pre-specialize during emission
//    // we can do that at the very end anyway without maintaining it
//    /*
//     * Do we store all operations on this node or not?
//     * we can store the kinds for pre speciliaziation, might not even hurt having the entire thing?
//     *  but how do we deal with deletions?
//     */
//
//    /*
//     * Nodes NEED to know parent to prevent memleaks
//     * if we dedup nodes we need to have
//     */
//
//
//    std::vector<CombinedNode*> parents;
//
//    std::vector<Edge*> in_edges;
//    std::vector<Edge*> out_edges;
//
//    bool deleted;
//    void replace_usage(const CombinedNode & old_parent, const CombinedNode & new_parent);
//    void replace_occurences_by(const CombinedNode & replacer)
//    {
//        /*
//         * for all in_edges
//         */
//        for(auto& parents: in_edges)
//            for(auto out_edges : parents.out_edges)
//                if(out_edges.target == &this)
//                    out_edges.target = replacer;
//
//        deleted = true;
//    }
//
//    template< typename Vis >
//    void traverse(Vis &vis)
//    {
//        for (auto op : _operands)
//            vis.dispatch(op);
//    }
//
//    template< typename Vis >
//    void traverse_upwards(Vis &vis)
//    {
//        for (auto op : users())
//            vis.dispatch(op);
//    }
//
//
//    // hash
//    // vector of (parent, placement-id, operation)
//    // vector of children (CN*, place-id, operation), must be shared over every parent to be mergable
//
//    //merge:: CN -> CN
//    // given node has all parent calls replaced with the current one,
//    // parent sets also merge
//
//};
//
//struct AdvicedPath {
//    // starting nodes from path
//    // LTT path it belongs to :: List
//    /*
//     * for every node we need to keep track which was related to it
//     * can be done through respecialization? -- need to update relations in UMG,
//     * finding umg nodes that way might be expensive :(
//     */
//};
//
//struct AdvicedTwinPath{
//    std::vector<CombinedNode*> orignals;
//    /*
//     * we first take a node, create a path from that node to the root of it's tree where all children of nodes that aren't in the path are marked away (we look at the hashes though)
//     * merging is done based on whether the parent have same hashes on different children nodes
//     *
//     * we have a set of linked/original nodes
//     * the highest node will have its orignal node shared with at least one other ;
//     *
//     */
//
//};
//
//
//CombinedNode convert_op_to_cn(Operation* op){
//    CombinedNode new_node;
//    for( auto &ref : op.operands)
//    {
//        Edge e;
//        auto child = convert_op_to_cn(ref);
//        e.source = new_node;
//        e.target = child;
//        new_node.out_edges.push_back(&e);
//        child.in_edges(new_node);
//    }
//    return new_node;
//}
//
//
//void hash_cn(const CombinedNode & node)
//{
//    std::stringstream ss;
//    ss << node.out_edges.size() << "|";
//    for(auto & ref : node.out_edges)
//        if(!has_hash(ref))
//            hash_cn(ref);
//        ss << hash_cn(ref.target);
//
//    node.hash = ss.to_str();
//}
//
//std::vector<Operation*> get_starting_nodes_for_all_ltt(Circuit* circuit);
//
//
//std::vector<CombinedNode*> get_leafs_from_cn(const CombinedNode &cn)
//{
//    std::vector<CombinedNode*> results;
//}
//
///*
// * assumes all nodes already have hash
// */
//
//class dedup_visitor {
//
//    std::map<std::string, CombinedNode*> known_hashes;
//    void visit(CombinedNode* cn)
//    {
//        for(auto & c: cn.out_edges){
//            /*
//             * for all children
//             *      if child.hash not in known hashes
//             *           add to known hashes
//             *      if child.hash in known hashes
//             *           in all parents of child, replace child with that in known hashes
//             *           delete old child
//             *           should be operation? on child
//             *
//             *
//             */
//
//        }
//    }
//};
//
//
//class uniqize {
//
//    void visit(CombinedNode* cn)
//    {
//        for(auto & c1: cn.out_edges){
//            for(auto & c2: cn.out_edges){
//                if(c1 != c2 && c1.hash() == c2.hash())
//                    c2.replace_all_occurences_by(c1);
//            }
//        }
//        traverse(&this);
//    }
//};
//void dedup(const std::vector<CombinedNode>& cn)
//{
//
//
//
//}
//
//
//
//UniqueMultiGraph CircToUMG(Circuit* circuit){
//    auto ltts = get_starting_nodes_for_all_ltt(circuit);
//    for(auto &ltt : ltts)
//        convert_op_to_cn(ltt);
//    // map convert_op_to_cn over llts
//
//    // apply dedup
//
//    /*
//     * get all ctt from the circuit
//     * convert them into segtrees
//     * add these to the graph
//     * merge only the new set
//     *
//     *
//     *
//     *
//     * convert to unique
//     *      have global hash table with hash -> string?SubPathCo
//     *      if not in hash table insert
//     *          else replace users with it <---> have parents call this on children, ---- can invalidate pointers though as we need to update all parents, and what if a node has 2 parents?
//     *              i
//     *
//     *
//     *
//     */
//}
//
//
//
//struct snode
//{
//    std::string id;
//};
//
//struct sroot :snode{
//    Operation* op;
//};
//
//struct opnode : snode{};
//struct AdvicePathLine {
//    using source_node_t = snode;
//    using target_node_t = snode;
//    using src_target = std::pair<source_node_t, target_node_t>;
//    std::vector<src_target> entry_exits;
//};
//
//struct advice_line_node : snode
//{
//    AdvicePathLine apl;
//};


template <gap::graph::node_like node>
struct TopBottHash
{
    node* root;
    // can be just the pairs (out_edge_nmr, #children) from parent. Still need to figure out what we want to do with duplicates
    // Would say that the duplicates should be just separate paths as they can match to different trees else, and both save the same amount of nodes twice when unrolld
    // std::vector<id_type> hash_ids, we can keep these from root -> advice node as our comp algo can use reversed itterators
    // std;:vector<nodes_in_path>
    // root node -> needs to be different on a match
    //
};

struct UniqueNameStorage
{
    decoder::Var get_unique_var_name();
    std::vector<decoder::Var> names;
    int counter =0 ;
};

// next advising
// take most savings TopBotHash set
/*
 * Take list TopBotthash hash1
 *  hashCompset = setTopBothHash
 *  for i = 0 to |path| hash1
 *  for all in hashCompset
 *   if hash1[i] != hash2[i]
 *      removeFromhashCompset
 *   potential_savings.add(currentPath * |hashCompset|)
 *
 *
 */


//auto segnode_hash_on_get_hash = [](const circ::SEGNode & node) {return std::hash<std::string>{}(node.get_hash()); };
//auto segnode_comp_on_hash = [](const circ::SEGNode & left, const circ::SEGNode & right) {return left.post_hash == right.post_hash; };


std::pair< decoder::Var, decoder::StatementBlock > expr_for_node(
    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration, segnode_hash_on_get_hash, segnode_comp_on_hash> &func_decls,
    UniqueNameStorage &unique_names_storage,
    const SEGNode &node,
    decoder::Var stack );

}



//namespace std {
//    template <> struct hash<circ::SEGNode>
//    {
//        size_t operator()(const circ::SEGNode & x) const
//        {
//            return std::hash<std::string>{}(x.id);
//            /* your code here, e.g. "return hash<int>()(x.value);" */
//        }
//    };
//}
