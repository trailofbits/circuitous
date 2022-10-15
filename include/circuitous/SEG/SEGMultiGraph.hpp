#pragma once

/*
 * Not sure if when accessing parents we should include operation already?
 */

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

struct FPCEdge : Edge {
    NodeType* FPCTarget;
};

/*
 * CTT path with root of AST?
 *
 *
 */

struct CombinedNode{
    // vector of all operations associated with this node, if 1 we can pre-specialize during emission
    /*
     * Do we store all operations on this node or not?
     * we can store the kinds for pre speciliaziation, might not even hurt having the entire thing?
     *  but how do we deal with deletions?
     */

    /*
     * Nodes NEED to know parent to prevent memleaks
     * if we dedup nodes we need to have 
     */


    std::vector<CombinedNode*> parents;

    std::vector<Edge*> in_edges;
    std::vector<Edge*> out_edges;

    void replace_usage(const CombinedNode & old_parent, const CombinedNode & new_parent);


    // hash
    // vector of (parent, placement-id, operation)
    // vector of children (CN*, place-id, operation), must be shared over every parent to be mergable

    //merge:: CN -> CN
    // given node has all parent calls replaced with the current one,
    // parent sets also merge

};

struct AdvicedPath {
    // starting nodes from path
    // LTT path it belongs to :: List
    /*
     * for every node we need to keep track which was related to it
     * can be done through respecialization? -- need to update relations in UMG,
     * finding umg nodes that way might be expensive :(
     */
};

struct AdvicedTwinPath{
    std::vector<CombinedNode*> orignals;
    /*
     * we first take a node, create a path from that node to the root of it's tree where all children of nodes that aren't in the path are marked away (we look at the hashes though)
     * merging is done based on whether the parent have same hashes on different children nodes
     *
     * we have a set of linked/original nodes
     * the highest node will have its orignal node shared with at least one other ;
     *
     */

};


CombinedNode convert_op_to_cn(Operation* op){
    CombinedNode new_node;
    for( auto &ref : op.operands)
    {
        Edge e;
        auto child = convert_op_to_cn(ref);
        e.source = new_node;
        e.target = child;
        new_node.out_edges.push_back(&e);
        child.in_edges(new_node);
    }
    return new_node;
}


void hash_cn(const CombinedNode & node)
{
    std::stringstream ss;
    ss << node.out_edges.size() << "|";
    for(auto & ref : node.out_edges)
        if(!has_hash(ref))
            hash_cn(ref);
        ss << hash_cn(ref.target);

    node.hash = ss.to_str();
}

UniqueMultiGraph CircToUMG(Circuit* circuit){



    /*
     * get all ctt from the circuit
     * convert them into segtrees
     * add these to the graph
     * merge only the new set
     *
     *
     *
     *
     * convert to unique
     *      have global hash table with hash -> string?SubPathCo
     *      if not in hash table insert
     *          else replace users with it <---> have parents call this on children, ---- can invalidate pointers though as we need to update all parents, and what if a node has 2 parents?
     *              i
     *
     *
     *
     */
}




