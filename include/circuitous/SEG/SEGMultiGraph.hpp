#pragma once
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>
#include <circuitous/Decoder/DecodeAST.hpp>

namespace circ {

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



// condition is equal to selector of Select* == chosen_idx
// equality for lhs, rhs: lhs.sel == rhs.sel && lhs.chosen_idx == rhs.chosen_idx
// as we always just copy the resulting_tree?

struct SelectChoice
{
    Select* sel;
    std::size_t chosen_idx;

    bool operator==(const SelectChoice &other) const noexcept
    {
        return (this->sel == other.sel && this->chosen_idx == other.chosen_idx);
    }
};

struct SelectChoiceHash
{
    std::size_t operator()(SelectChoice const& s) const noexcept
    {
        return std::hash<Select*>{}(s.sel);
    }
};


struct SelectChoiceCompare {
    bool operator() (const SelectChoice &lhs, const SelectChoice &rhs) const noexcept
    {
        return (lhs.sel == rhs.sel && lhs.chosen_idx == rhs.chosen_idx);
    }
};

using choice_set = std::unordered_set<SelectChoice, SelectChoiceHash, SelectChoiceCompare>;

//using choice_set = std::unordered_set<SelectChoice, SelectChoiceHash, SelectChoiceCompare>;
//
//struct GenerationUnit
//{
//    VerifyInstruction* vi; // it belongs to
//    Operation* root_operation;
////    using choice_set = std::unordered_set<SelectChoice, SelectChoiceHash, SelectChoiceCompare>;
//    std::shared_ptr<choice_set> choices = std::make_shared<choice_set>(); // might be empty
//
//};


//struct GUHash {
//    bool operator() (const GenerationUnit &lhs) const noexcept{
//        return std::hash<VerifyInstruction*>{}(lhs.vi);
//        //        return (lhs.vi == rhs.vi && std::equal( lhs.choices.begin(), lhs.choices.end(), rhs.choices.begin()));
//    }
//};
//
//
//struct GUcompare {
//    bool operator() (const GenerationUnit &lhs, const GenerationUnit &rhs) const{
//
//        for(auto lchoice : *(lhs.choices))
//        {
//            bool has_equal = false;
//            for(auto rchoice : *(rhs.choices))
//            {
//                // transform this into ==
//                if(lchoice.sel == rchoice.sel && lchoice.chosen_idx == rchoice.chosen_idx)
//                    has_equal = true;
//            }
//            if(!has_equal)
//                return false;
//        }
//        return (lhs.vi == rhs.vi && lhs.root_operation == rhs.root_operation); // TODO(sebas): implement choices
////        return (lhs.vi == rhs.vi && std::equal( lhs.choices.begin(), lhs.choices.end(), rhs.choices.begin()));
//    }
//};
//

//TODO(sebas): add tree to gap?
struct SEGNode;

struct Tree{
    std::vector<std::shared_ptr<Tree>> children = {};

    Operation* original;
    std::shared_ptr<SEGNode> node;
    void add_child(std::shared_ptr<Tree> child){
        children.push_back(child);
    }

    bool is_leaf() { return children.empty(); }
};



struct InstructionProjection
{

    VerifyInstruction* vi;
    Operation* root_in_vi;
    choice_set select_choices; // should be an unordered set, but implicitly there is only one order.

    /*
     * if they are inside the same context, with the same starting points and same choices for the select
     * then all SEGNodes should be isomorphic and thus the same.
     * TODO(Sebas): I'm worried that segnodes with different roots might cause trouble, since they should eventually all have the same roots
     */
    bool operator==(const InstructionProjection& other) const noexcept
    {
        return vi == other.vi && root_in_vi == other.root_in_vi
            && select_choices == other.select_choices;
    }
};
//
struct InstructionProjectionHash
{
    bool operator() (const InstructionProjection &hash) const noexcept
    {
        return std::hash<Operation*>{}(hash.root_in_vi);
    }
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
//    std::unordered_multimap<GenerationUnit, Operation*, GUHash, GUcompare> roots;
    std::unordered_set<InstructionProjection, InstructionProjectionHash> valid_for_contexts;

//    void insert_root();
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

    gap::generator< child_type > child(std::size_t idx)
    {
        co_yield std::make_shared<nodeWrapper>(nodeWrapper(op->operand(idx)));
    }

    gap::generator< child_type > child(std::size_t idx) const
    {
        co_yield std::make_shared<nodeWrapper>(nodeWrapper(op->operand(idx)));
    }
};


template< gap::graph::yield_node when >
    requires gap::graph::node_like< nodeWrapper >
gap::recursive_generator< std::shared_ptr<nodeWrapper> > non_unique_dfs_with_choices( std::shared_ptr<nodeWrapper> root, const choice_set & choices) {

//    auto yield_node = [&](const nodeWrapper& node)
//    {
//        if(!isa<Select>(root->op))
//            co_yield root;
//        else
//        {
//            auto choice = std::find_if(choices->begin(), choices->end(), [&](const SelectChoice& choice) { return choice.sel == root->op; });
//            if(choice != choices->end())
//            {
//                co_yield root->child(*choice->chosen_idx);
//            }
//        }
//
//    };
    if constexpr (when == gap::graph::yield_node::on_open) {
        if(!isa<Select>(root->op))
            co_yield root;
    }

    if(!isa<Select>(root->op))
    {
        for (auto child : root->children()) {
            co_yield non_unique_dfs_with_choices< when >(child, choices);
        }
    }
    else
    {
        auto choice = std::find_if(choices.begin(), choices.end(), [&](const SelectChoice& choice) { return choice.sel == root->op; });
        if(choice != choices.end())
        {
            std::size_t idx = (*choice).chosen_idx;
            for (auto child : root->child(idx)) {
                co_yield non_unique_dfs_with_choices< when >(child, choices);
            }
        }
    }


    if constexpr (when == gap::graph::yield_node::on_close) {
        if(!isa<Select>(root->op))
            co_yield root;
    };
}


//TODO(sebas): move this to gap
template < class S, class T >
gap::recursive_generator< std::pair< S, T > >
tuple_generators( gap::recursive_generator< S > &s,
                  gap::recursive_generator< T > &t)
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
    std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > >> get_nodes_by_vi(VerifyInstruction* vi);
//    std::vector<std::pair<GenerationUnit, std::shared_ptr<SEGNode>>> get_nodes_by_vi(VerifyInstruction* vi);
//    std::vector<std::pair<GenerationUnit, std::shared_ptr<SEGNode>>> get_nodes_by_gu(const GenerationUnit& gu);

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

/*
 * For a projection we need to save every possible combination of choices which might be made
 * in a single subtree for an emitted node.
 * If we take all possible choices over a VI this might balloon way to fast, so we need to isolate this
 *
 * The way we do this is by having projections have a VI to know which emission group it belongs to
 * together with the actual emitted node/Operation* which contains all information for operations
 *
 * So we will have emission groups/VI for an instruction
 *      A struct of emissions/ ALL projects (pure?):
 *          Might be shared across the entire VI
 *      VI projections conditional
 *          the condition list/expr
 *          Expression node for stack, this must have as subtree pre-patched selects
 *          pair<PPOperation*, SEGnode*, Condlist/Expr>
 *
 *
 * a projection of circIR
 *  a VI
 *  a choice for all select nodes on decode time <Decode Condition/Extract>
 *      From this we generate Expr conditions
 *  Start node, always independent of the choices above I think>
 * ????/
 */

/*
 * Projection of a VI.
 * Unfinsihed projection of VI
 */

struct advice_value_visitor : Visitor<advice_value_visitor>
{
    Advice* target;
    Operation* result = nullptr;
    bool result_is_left_side = false;

    explicit advice_value_visitor( Advice *target ) : target( target ) { }
    void visit(AdviceConstraint* ac)
    {
        check(ac->operands_size() == 2) << "advice constraint does not contain 2 children";
        check(ac->operand(0) != ac->operand(1)) << "advice constraint points to same child twice, left id:" << ac->operand(0)->id() << " id right:" << ac->operand(1)->id() ;
        if(ac->operand(0) == target)
        {
            result_is_left_side = false;
            result = ac->operand( 1 );
        }

        if(ac->operand(1) == target)
        {
            result_is_left_side = true;
            result = ac->operand( 0 );
        }
    }

    void visit(Operation* op)
    {
        op->traverse(*this);
    }
};

enum class AdviceDirection
{
    Starting,
    Left,
    Right
};

Operation* get_op_attached_to_advice_in_vi(Advice* advice, VerifyInstruction* vi, AdviceDirection dir = AdviceDirection::Starting);


struct Select_choice_maker_visitor
{

};
/*
 * Copy the structure of SEGnodes from a given projection
 * output has fresh SEGNodes (with fresh id's) inside pairs with their corresponding Operations
 *
 */
struct UnfinishedProjection : InstructionProjection
{
    std::unordered_set<std::shared_ptr<Tree>> to_continue_from; // all those nodes who's operation is a leaf
    std::unordered_set<std::shared_ptr<Tree>> selects_to_continue_from; // all those nodes who's operation is a leaf

    std::map<std::size_t, std::shared_ptr<Tree>> seen_nodes;
    std::string segnode_prefix = "";

    std::vector<std::shared_ptr<UnfinishedProjection>> created_projections;
    std::shared_ptr<Tree> projection;

    /*
     * This constructor is meant to be used when copying an unfinished projection
     * As this does not add itself to any of the to_continue_from sets.
     * As in the original it might have deleted itself from this already, and should be externally set.
     */
    explicit UnfinishedProjection( const std::string &segnodePrefix, VerifyInstruction* vi, std::shared_ptr<Tree> root, const choice_set& choices):
    segnode_prefix(segnodePrefix)
    {
        this->vi = vi;
        this->projection = root;
        this->select_choices = choices;
        init_root_tree();
    }

    explicit UnfinishedProjection( const std::string &segnodePrefix, VerifyInstruction* vi, Operation* root) :
        segnode_prefix( segnodePrefix )
    {
        this->vi = vi;
        projection = std::make_shared<Tree>();
        projection->node = std::make_shared<SEGNode>(segnodePrefix + "_root");
        projection->original = root;

        if( isa<Select>(projection->original) )
            selects_to_continue_from.insert(projection);
        else
            to_continue_from.insert(projection);

        init_root_tree();
    }

    void fully_extend()
    {
        while(!to_continue_from.empty() || !selects_to_continue_from.empty())
        {
            finish_up_to_decode_select();
            auto copies = project_select();
            for(auto & copy : copies)
            {
                assert(select_choices.size() > 0);
                copy->fully_extend();
                check(copy->select_choices.size() > 0) << "lol";
            }

            created_projections.insert(created_projections.end(), copies.begin(), copies.end());
            for(auto & c : created_projections)
                check(c->select_choices.size() > 0) << "lol" << c->select_choices.size();
        }
        InstructionProjection valid_for = {vi, projection->original, select_choices};
        projection->node->valid_for_contexts.insert(valid_for);

        for(auto & c : created_projections)
            check(c->projection->node->valid_for_contexts.begin()->select_choices.size() > 0) << "lol" << c->projection->node->valid_for_contexts.begin()->select_choices.size();

    }

    void finish_up_to_decode_select()
    {
        while(!to_continue_from.empty()) // ??? ??? ??
        {
            auto first_item = to_continue_from.begin();
            extend_except_select( *first_item );
            to_continue_from.erase(first_item);
        }
    }

    //TODO(sebas): make optional?
    std::vector<std::shared_ptr<UnfinishedProjection>> project_select()
    {
        std::vector<std::shared_ptr<UnfinishedProjection>> copies;
        if(selects_to_continue_from.empty())
            return copies;


        auto first_it = selects_to_continue_from.begin();
        auto node = *first_it;
        if( is_decode_select(node->original) )
        {
            Select* sel = static_cast<Select*>( node->original );
            circ::check(sel->operands_size() >= 3) << "not enough operands for select";
            // minus 1 for selector bit
            for(std::size_t i = 2; i < sel->operands_size(); i++)
            {
                auto select_prefix = "select" + std::to_string(sel->id()) + "copy" + std::to_string(i);
                auto copy = deep_copy(select_prefix);
                auto to_update = std::find_if(copy->selects_to_continue_from.begin(), copy->selects_to_continue_from.end(),[&](std::shared_ptr<Tree> tree)
                              {
                                  return tree->original == sel;
                              });
                if(to_update == copy->selects_to_continue_from.end())
                    std::cout << "were BAD" << std::endl;
                circ::check(to_update != copy->selects_to_continue_from.end()) << "select could not be found in copied tree";
                to_update->get()->original = sel->operand(i);
                to_update->get()->node = std::make_shared<SEGNode>(segnode_prefix + "copy_prefix_select");
                // does not get removed just updated so we should be good here?

                copy->select_choices.insert(SelectChoice{sel, i});
//                check(copy->projection->node == (*copy->projection->node->valid_for_contexts.begin())->projection->node) << "not equal??";
//                copy->select_choices->insert(SelectChoice{sel, i});
                copies.push_back(std::make_shared<UnfinishedProjection>(*copy));
            }
            // update last inplace
            select_choices.insert(SelectChoice{sel, 1});
            node->original = sel->operand(1);
            node->node = std::make_shared<SEGNode>(segnode_prefix + "copy_prefix_select");
//            for(auto copy : copies)
//            {
//                auto direct_node = this->projection->node;
//                auto indirect = (*this->projection->node->valid_for_contexts.begin());
//                auto indirect_node = indirect->projection->node;
//                check(direct_node == indirect_node) << "not equal??";
//                check(this->select_choices.size() == 1 ) << "not direct 1??";
//                check(indirect->select_choices.size() == 1 ) << "not 1??";
//            }
        }
        selects_to_continue_from.erase(first_it);
        return copies;
    }

    void extend_except_select(std::shared_ptr<Tree> tree)
    {
        if( is_decode_select(tree->original) )
        {
            auto s = std::find_if(selects_to_continue_from.begin(), selects_to_continue_from.end(),
                                   [&](std::shared_ptr<Tree> t) { return t->original == tree->original; });
            if(s == selects_to_continue_from.end())
            {
                selects_to_continue_from.insert( tree );
            }
            return;
        }


        if(tree->original->operands_size() == 0)
            return;

        if(!tree->children.empty()) // already extended
            return;

        for(auto op_child : tree->original->operands() )
        {
            if(seen_nodes.contains(op_child->id()))
            {
                tree->node->add_child(seen_nodes[op_child->id()]->node);
                tree->add_child( seen_nodes[ op_child->id() ] );
            }
            else
            {
                // assumes no transitive advices
//                if( isa<Advice>(op_child))
//                {
//                    auto advice = static_cast<Advice*>(op_child);
//                    auto advice_value = get_op_attached_to_advice_in_vi(advice, vi);
//
//                    op_child = advice_value;
//                }

                auto new_seg_node = std::make_shared<SEGNode>(segnode_prefix + "_" + std::to_string(op_child->id()));
                auto new_child = std::make_shared<Tree>();
                new_child->original = op_child;
                new_child->node = new_seg_node;
                tree->node->add_child(new_seg_node);
                tree->add_child(new_child);
                to_continue_from.insert(new_child);
                seen_nodes.insert( { op_child->id(), new_child } );
            }
        }
    }

    bool is_finished();

    std::unique_ptr<UnfinishedProjection> deep_copy(const std::string& id_postfix)
    {
        std::map<std::size_t, std::shared_ptr<Tree>> copied_nodes;
        auto old_root = projection;

        auto new_root_proj = std::make_shared<Tree>();
        new_root_proj->original = old_root->original;
        new_root_proj->node = std::make_shared<SEGNode>(segnode_prefix + id_postfix + "root");
        UnfinishedProjection copy(segnode_prefix + id_postfix, vi, new_root_proj, select_choices);

        for(auto & c: old_root->children)
        {
            auto child = deep_copy_child( id_postfix, *c, copied_nodes, copy ) ;
            new_root_proj->node->add_child(child->node);
            new_root_proj->add_child(child);
        }

        if( std::find_if( to_continue_from.begin(), to_continue_from.end(),
                           [&](const std::shared_ptr<Tree>& t) { return projection->node->id == t.get()->node->id; }
                           ) != to_continue_from.end())
            copy.to_continue_from.insert(copy.projection);


        if( std::find_if( selects_to_continue_from.begin(), selects_to_continue_from.end(),
                           [&](const std::shared_ptr<Tree>& t) { return projection->node->id == t.get()->node->id; }
                           ) != selects_to_continue_from.end())
            copy.selects_to_continue_from.insert(copy.projection);

        return std::make_unique<UnfinishedProjection>( copy );
    };

private:
    void init_root_tree()
    {
        projection->node->isRoot = true;
//        select_choices.insert(SelectChoice{static_cast<Select*>(projection->original), 1});
//        auto nested = (*projection->node->valid_for_contexts.begin());
//        check(nested->id == 3) << "WTF id:" << nested->id;
//        check(nested->select_choices.size() == 1) << "WTF selects" << nested->select_choices.size() ;
    }

    bool is_decode_select(Operation* op)
    {
        //TODO(sebas): check meta data to see if it tainted to be decode time
        return isa<Select>(op);
    }

    std::shared_ptr<Tree> deep_copy_child(const std::string& id_postfix, const Tree& copy_from, std::map<std::size_t, std::shared_ptr<Tree>>& copied_nodes, UnfinishedProjection& copy_to)
    {
        auto old_seg = copy_from.node;
        auto old_op = copy_from.original;
        std::shared_ptr<SEGNode> copy_of_value;
        auto new_tree = std::make_shared<Tree>();

        // make value
        if( copied_nodes.contains(old_op->id()))
            return copied_nodes[old_op->id()];

        copy_of_value = std::make_shared< SEGNode >(old_seg->id + id_postfix);
        new_tree->original = old_op;
        new_tree->node = copy_of_value;

        // if we should extend the node in the current project, then we should do that in the new projection as well
        if ( std::find_if( to_continue_from.begin(), to_continue_from.end(),
                           [ & ]( const std::shared_ptr< Tree > t )
                           { return copy_from.node->id == t->node->id; } )
             != to_continue_from.end() )
            copy_to.to_continue_from.insert(new_tree);

        // if we should extend the node in the current project, then we should do that in the new projection as well
        if ( std::find_if( selects_to_continue_from.begin(), selects_to_continue_from.end(),
                           [ & ]( const std::shared_ptr< Tree > t )
                           { return copy_from.node->id == t->node->id; } )
             != selects_to_continue_from.end() )
            copy_to.selects_to_continue_from.insert(new_tree);


        /*
         * Each projection keeps track of a list of nodes which it already has created
         * In the new projection we have different segnodes and we need to update which
         * nodes they link to
         */
        if ( std::find_if( seen_nodes.begin(), seen_nodes.end(),
                           [ & ]( auto &t ) { return copy_from.original->id() == t.first; } )
             != seen_nodes.end() )
            seen_nodes.insert({new_tree->original->id(), new_tree});

        for ( auto child : copy_from.children )
        {
            auto new_child = deep_copy_child(id_postfix, *child, copied_nodes, copy_to);
            new_tree->node->add_child(new_child->node);
            new_tree->add_child(new_child);
        }

        copied_nodes.insert({old_op->id(), new_tree });
        return new_tree;
    };
};


struct ProjectionExtender
{
    std::vector<InstructionProjection> extend(Operation* op);
private:
    void project_select();
    std::vector<UnfinishedProjection> unfinished_projections;
};


/*
 * This creates a copy of the entire tree that the given node is in.
 * This assumes that there is only a single root inside the entire tree.
 * It returns both the root, as-well as node from the point which copying started inside the copied tree.
 *
 * This was created so that we can walk over select nodes, and then create multiple trees for all possible ways we could go into.
 * Note that all
 */
struct BidirectionalCopier
{
    using node_ptr = std::shared_ptr<SEGNode>;
    node_ptr root;
    node_ptr starting_node;

    void copy(node_ptr node);

private:
    /*
     * Will go upwards until a root has been hit,
     * It takes a parent node which from which it will try to
     */
    void visit_upwards(node_ptr from_parent, node_ptr from_child, node_ptr to_child); // visit upwards

    // returns a copied sub tree starting from from_child
    node_ptr copy_downwards(node_ptr from_child); // visit downwards
};


void specialize(std::map<std::string, Operation*>& specs, std::shared_ptr<SEGNode> node , Operation* op );

//
class graph_constructor_visitor: UniqueVisitor<graph_constructor_visitor>
{
    using segnode_ptr = std::shared_ptr<SEGNode>;

    bool isRoot = true;
    int node_counter = 0;
    std::string vi_start; // from root?
//    VerifyInstruction* vi;

public:
    std::vector<std::shared_ptr<SEGNode>> result_set;
    graph_constructor_visitor( const std::string &viStart, VerifyInstruction *vi ) : vi_start( viStart )
    { }


    segnode_ptr visit(Operation * op)
    {
        std::string s = vi_start + std::to_string(node_counter);
        auto new_node = std::make_shared<SEGNode>(s);

        if(isRoot)
        {
            isRoot = false;
//            new_node->isRoot = true;
//            GenerationUnit gu;
//            gu.vi = vi;
//            gu.root_operation = op;
//            std::cout << "root inserted " << std::endl;
//            new_node->roots.insert(std::make_shared<InstructionProjection>(*this));
        }

        //TODO(sebas): delete this function
        circ::unreachable() << "old code";
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
    for(std::shared_ptr<SEGNode> & to_merge_with : graph.nodes())
    {
        if ( to_hash->id != to_merge_with->id && to_merge_with->isRoot
             && to_hash->isRoot
             && to_hash->get_hash() == to_merge_with->get_hash() )
        {
            to_merge_with->valid_for_contexts.merge(to_hash->valid_for_contexts);
//            std::cout << "merging " << to_merge_with->id << " new size: " << to_merge_with->valid_for_contexts.size() << std::endl;
//            for(auto lhs : to_merge_with->valid_for_contexts )
//            {
//                for(auto rhs : to_hash->valid_for_contexts )
//                {
//                    bool found = false;
//                    if(lhs == rhs)
//                        found = true;
//                    if(!found)
//                    {
//                        std::cout << "merged roots, with choice size " << lhs.select_choices.size() <<  " " << rhs.select_choices.size() << std::endl;
//                        to_merge_with->valid_for_contexts.insert( rhs );
////                        std::cout << "op id: " << to_merge_with->id << "new size " << to_merge_with->valid_for_contexts.size();
//                    }
//                }
//            }

            graph.remove_node(to_hash);
            break;
        }
    }
}

decoder::FunctionCall print_SEGNode_tree( SEGNode& node, std::string stack_name , Operation* op);



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
