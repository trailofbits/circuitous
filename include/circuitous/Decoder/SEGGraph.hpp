#pragma once

#include "gap/core/graph.hpp"
#include "circuitous/IR/Visitors.hpp"

namespace circ::decoder
{

    Operation *get_op_attached_to_advice_in_vi( Advice *advice, VerifyInstruction *vi);

    // Represents a choice made for a select node
    struct SelectChoice
    {
        Select *sel;
        std::size_t chosen_idx;

        bool operator==( const SelectChoice &other ) const noexcept
        {
            return ( this->sel == other.sel && this->chosen_idx == other.chosen_idx );
        }
    };

    struct SelectChoiceHash
    {
        std::size_t operator()( SelectChoice const &s ) const noexcept
        {
            return std::hash< Select * > {}( s.sel );
        }
    };

    struct SelectChoiceCompare
    {
        bool operator()( const SelectChoice &lhs, const SelectChoice &rhs ) const noexcept
        {
            return ( lhs.sel == rhs.sel && lhs.chosen_idx == rhs.chosen_idx );
        }
    };

    using choice_set
        = std::unordered_set< SelectChoice, SelectChoiceHash, SelectChoiceCompare >;

    struct SEGNode;

    struct Tree
    {
        std::vector< std::shared_ptr< Tree > > children = {};

        Operation *original;
        std::shared_ptr< SEGNode > node;
        void add_child( std::shared_ptr< Tree > child ) { children.push_back( child ); }

        bool is_leaf() { return children.empty(); }
    };

    /*
     * An Instruction Projection represents a subtree for a fully specialized context/vi
     * Lets say the context we want to represent consists of two assignments:
     * 1. Update a register value
     * 2. Update instruction pointer
     *
     * Both of these would get their own InstructionProjection
     *
     * Notice that Instruction Projects are unique up a set of select choices.
     * So even if we have the same root note for a given context,
     * the select choices can be different inside the subtree resulting in different semantics.
     * However, this is _only_ true for decode time selects, as selects can also be part of the
     * semantics that are expressed.
     */
    struct InstructionProjection
    {
        VerifyInstruction *vi;
        Operation *root_in_vi;
        choice_set select_choices; // should be an unordered set, but implicitly there is only
                                   // one order.

        /*
         * if they are inside the same context, with the same starting points and same choices
         * for the select then all SEGNodes should be isomorphic and thus the same.
         */
        bool operator==( const InstructionProjection &other ) const noexcept
        {
            return vi == other.vi && root_in_vi == other.root_in_vi
                   && select_choices == other.select_choices;
        }
    };

    struct InstructionProjectionHash
    {
        bool operator()( const InstructionProjection &hash ) const noexcept
        {
            return std::hash< Operation * > {}( hash.root_in_vi );
        }
    };

    /*
     * Semantics Emitter Generator Node (SEGNode) is one of the central data types for the
     * semantics emitter. These nodes do not have a kind and represent "operation-less" circIR
     * nodes.
     *
     *
     */
    struct SEGNode
    {
        using node_pointer = std::shared_ptr< SEGNode >;
        using child_type = node_pointer;
        SEGNode( const std::string &id );
        std::vector< child_type > children();
        std::vector< child_type > parents();
        std::vector< child_type > _nodes;
        std::vector< child_type > _parents;

        /*
         * should this add for both? I Think so
         */
        void add_parent( node_pointer parent );
        void add_child( node_pointer child );

        void replace_all_nodes_by_id( node_pointer new_target, std::string target_id );

        bool isRoot = false;
        /*
         * A specialized SEGNode is one for which all circIR nodes have the same kind
         * Or in case of constants, represent the same constant value.
         * Currently unused
         */
        bool specializeble = true;

        /*
         * Once SEGnodes get converted to a multi graph we need to keep track of the original
         * owners The easiest solution was to keep track of the parents/context for a node and
         * update on merges
         */
        std::unordered_set< InstructionProjection, InstructionProjectionHash >
            valid_for_contexts;

        // TODO(sebas): update to a more efficient representation
        std::string id; // Required to be unique

        // The hash function should give a hash of the shape of the sub-tree
        std::string get_hash() const;

        /*
         * In-line cost is computed by looking at the number of nodes that are children
         * of this node and do not have the fd flag set (not counting children of fd nodes)
         */
        int inline_cost = 0;
        int subtree_count = 0; // count of all it's children and their children, used to
                               // calculate the size of pre-allocation needed
        // Remembers whether the node is costly enough to warrant a function introduction for
        // the semantics emitter
        bool fd = false;
    };

    struct segnode_hash_on_get_hash
    {
        std::size_t operator()( const SEGNode &node ) const noexcept
        {
            auto ret_val = std::hash< std::string > {}( node.get_hash() );
            return ret_val;
        }
    };

    struct segnode_comp_on_hash
    {
        bool operator()( const SEGNode &left, const SEGNode &right ) const
        {
            return left.get_hash() == right.get_hash();
        }
    };

    struct nodeWrapper
    {
        using node_pointer = std::shared_ptr< nodeWrapper >;
        using child_type = node_pointer;
        circ::Operation *op;

        nodeWrapper( Operation *op ) : op( op ) { }
        gap::generator< child_type > children()
        {
            for ( auto &n : op->operands() )
                co_yield std::make_shared< nodeWrapper >( nodeWrapper( n ) );
        }

        gap::generator< child_type > children() const
        {
            for ( auto &n : op->operands() )
                co_yield std::make_shared< nodeWrapper >( nodeWrapper( n ) );
        }

        gap::generator< child_type > child( std::size_t idx )
        {
            co_yield std::make_shared< nodeWrapper >( nodeWrapper( op->operand( idx ) ) );
        }

        gap::generator< child_type > child( std::size_t idx ) const
        {
            co_yield std::make_shared< nodeWrapper >( nodeWrapper( op->operand( idx ) ) );
        }
    };

    template < gap::graph::yield_node when >
        requires gap::graph::node_like< nodeWrapper >
    gap::recursive_generator< std::shared_ptr< nodeWrapper > >
    non_unique_dfs_with_choices( std::shared_ptr< nodeWrapper > root, const choice_set &choices,
                                 VerifyInstruction *vi )
    {
        if ( isa< Advice >( root->op ) )
        {
            auto advice = static_cast< Advice * >( root->op );
            auto advice_value = get_op_attached_to_advice_in_vi( advice, vi );
            root = std::make_shared< nodeWrapper >( advice_value );
        }
        if constexpr ( when == gap::graph::yield_node::on_open )
        {
            if ( !isa< Select >( root->op ) )
                co_yield root;
        }

        if ( !isa< Select >( root->op ) )
        {
            for ( auto child : root->children() )
            {
                co_yield non_unique_dfs_with_choices< when >( child, choices, vi );
            }
        }
        else
        {
            auto choice = std::find_if( choices.begin(), choices.end(),
                                        [ & ]( const SelectChoice &choice )
                                        { return choice.sel == root->op; } );
            if ( choice != choices.end() )
            {
                std::size_t idx = ( *choice ).chosen_idx;
                for ( auto child : root->child( idx ) )
                {
                    co_yield non_unique_dfs_with_choices< when >( child, choices, vi );
                }
            }
        }

        if constexpr ( when == gap::graph::yield_node::on_close )
        {
            if ( !isa< Select >( root->op ) )
                co_yield root;
        };
    }

    // TODO(sebas): move this to gap
    template < class S, class T >
    gap::recursive_generator< std::pair< S, T > >
    tuple_generators( gap::recursive_generator< S > &s, gap::recursive_generator< T > &t )
    {
        auto gen_s = s.begin();
        auto gen_t = t.begin();

        while ( true )
        {
            auto is_end_s = gen_s == s.end();
            auto is_end_t = gen_t == t.end();

            if ( is_end_s != is_end_t )
                unreachable() << "non-isomorphic generators left:" << is_end_s << " right "
                              << is_end_t;

            if ( is_end_s || is_end_t )
                break;

            co_yield { *gen_s, *gen_t };

            gen_s++;
            gen_t++;
        }
    }

    template < typename T >
    struct SEGEdge
    {
        using source_type = T;
        using target_type = T;

        T _source;
        T _target;

        source_type source() { return _source; }
        target_type target() { return _target; }
    };

    struct UnfinishedProjection;
    struct SEGGraph
    {
        using node_type = SEGNode;
        using node_pointer = node_type::node_pointer;
        using edge_type = SEGEdge< node_pointer >;

        explicit SEGGraph( circuit_ref_t circ ) : circuit( circ ) { }
        std::vector< node_pointer > nodes() const;
        gap::generator< edge_type > edges() const;

        std::vector< node_pointer > _nodes;

        void remove_node( const node_pointer &node );
        std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > > >
        get_nodes_by_vi( VerifyInstruction *vi );

        void prepare();

    private:
        // calculate costs of the nodes for emission
        void calculate_costs();
        void extract_all_seg_nodes_from_circuit();

        circuit_ref_t circuit;
        void save_nodes_from_projection( std::vector< std::shared_ptr< SEGNode > > &nodes,
                                         UnfinishedProjection &up ) const;
    };

    using seg_projection = std::pair< InstructionProjection, std::shared_ptr< SEGNode > >;


    template < typename Derived, bool IsConst = false >
    struct AdviceResolvingVisitor : Visitor< Derived, IsConst >
    {
        AdviceResolvingVisitor( VerifyInstruction *vi ) : vi( vi ) {};

        auto visit( Advice *op )
        {
            this->parent_t::dispatch( get_op_attached_to_advice_in_vi( op, vi ) );
        }

    protected:
        VerifyInstruction *vi;
    };
}
