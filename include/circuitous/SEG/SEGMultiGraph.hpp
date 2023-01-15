#pragma once
#include <circuitous/Decoder/DecodeAST.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>

namespace circ
{

    Operation *get_op_attached_to_advice_in_vi( Advice *advice, VerifyInstruction *vi );

    template < gap::graph::yield_node when, typename node_pointer >
        requires gap::graph::node_like< typename node_pointer::element_type >
    gap::recursive_generator< node_pointer > non_unique_dfs( node_pointer root )
    {
        if constexpr ( when == gap::graph::yield_node::on_open )
        {
            co_yield root;
        }

        for ( auto child : root->children() )
        {
            co_yield non_unique_dfs< when >( child );
        }

        if constexpr ( when == gap::graph::yield_node::on_close )
        {
            co_yield root;
        };
    }

    struct UniqueNameStorage
    {
        decoder::Var get_unique_var_name(decoder::Id type_name = "auto");
        std::vector<decoder::Var> get_n_var_names(int amount_of_names, decoder::Id type_name = "auto");
        std::vector< decoder::Var > names;
        int counter = 0;
    };

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

    // InstructionProjections represent a subtree for a fully specialized context/vi
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

    struct SEGGraph
    {
        using node_type = SEGNode;
        using node_pointer = node_type::node_pointer;
        using edge_type = SEGEdge< node_pointer >;
        using CircuitPtr = Circuit::circuit_ptr_t;

        explicit SEGGraph( CircuitPtr &circuit );
        std::vector< node_pointer > nodes() const;
        gap::generator< edge_type > edges() const;

        std::vector< node_pointer > _nodes;

        void remove_node( const node_pointer &node );
        std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > > >
        get_nodes_by_vi( VerifyInstruction *vi );
        //    std::vector<std::pair<GenerationUnit, std::shared_ptr<SEGNode>>>
        //    get_nodes_by_vi(VerifyInstruction* vi); std::vector<std::pair<GenerationUnit,
        //    std::shared_ptr<SEGNode>>> get_nodes_by_gu(const GenerationUnit& gu);

        SEGGraph copy();

        void prepare();
        void print_semantics_emitter( decoder::ExpressionPrinter &ep );
        void print_decoder( decoder::ExpressionPrinter &ep );
        int get_maximum_vi_size();

    private:
        // calculate costs of the nodes for emission
        void calculate_costs();

        CircuitPtr circuit;
        std::unordered_map< SEGNode, decoder::FunctionDeclaration, segnode_hash_on_get_hash,
                            segnode_comp_on_hash >
            func_decls;
        UniqueNameStorage name_storage;
        decoder::Var stack = decoder::Var( "stack" );
        decoder::Expr get_expression_for_projection( VerifyInstruction *vi,
                                                     InstructionProjection &instr_proj,
                                                     std::shared_ptr< SEGNode > &node );
    };

    /*
     * For a projection we need to save every possible combination of choices which might be
     * made in a single subtree for an emitted node. If we take all possible choices over a VI
     * this might balloon way to fast, so we need to isolate this
     *
     * The way we do this is by having projections have a VI to know which emission group it
     * belongs to together with the actual emitted node/Operation* which contains all
     * information for operations
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

    /*
     * This structure is an intermediate represntation of a circIR subtree being converted to an
     * InstructionProjection More-over it generates the actual SEGNodes incrementally, and
     * splits itself off for every decode-time select
     */
    struct UnfinishedProjection : InstructionProjection
    {
        std::unordered_set< std::shared_ptr< Tree > > to_continue_from;
        std::unordered_set< std::shared_ptr< Tree > > selects_to_continue_from;

        std::map< std::size_t, std::shared_ptr< Tree > > seen_nodes;
        std::string segnode_prefix = "";

        std::vector< std::shared_ptr< UnfinishedProjection > > created_projections;
        std::shared_ptr< Tree > projection;

        /*
         * This constructor is meant to be used when copying an unfinished projection
         * As this does not add itself to any of the to_continue_from sets.
         * As in the original it might have deleted itself from this already, and should be
         * externally set.
         */
        explicit UnfinishedProjection( const std::string &segnodePrefix, VerifyInstruction *vi,
                                       std::shared_ptr< Tree > root,
                                       const choice_set &choices ) :
            segnode_prefix( segnodePrefix )
        {
            this->vi = vi;
            this->projection = root;
            this->select_choices = choices;
            init_root_tree();
        }

        explicit UnfinishedProjection( const std::string &segnodePrefix, VerifyInstruction *vi,
                                       Operation *root ) :
            segnode_prefix( segnodePrefix )
        {
            this->vi = vi;
            projection = std::make_shared< Tree >();
            projection->node = std::make_shared< SEGNode >( segnodePrefix + "_root" );
            projection->original = root;

            if ( isa< Select >( projection->original ) )
                selects_to_continue_from.insert( projection );
            else
                to_continue_from.insert( projection );

            init_root_tree();
        }

        void fully_extend()
        {
            while ( !to_continue_from.empty() || !selects_to_continue_from.empty() )
            {
                finish_up_to_decode_select();
                auto copies = project_select();
                for ( auto &copy : copies )
                {
                    assert( select_choices.size() > 0 );
                    copy->fully_extend();

                    // This should succeed with arbitrary nesting, but should verify this.
                    if ( !copy->created_projections.empty() )
                        created_projections.insert( created_projections.end(),
                                                    copy->created_projections.begin(),
                                                    copy->created_projections.end() );
                }

                created_projections.insert( created_projections.end(), copies.begin(),
                                            copies.end() );
            }
            InstructionProjection valid_for = { vi, projection->original, select_choices };
            projection->node->valid_for_contexts.insert( valid_for );
        }

        void finish_up_to_decode_select()
        {
            while ( !to_continue_from.empty() )
            {
                auto first_item = to_continue_from.begin();
                extend_except_select( *first_item );
                to_continue_from.erase( first_item );
            }
        }

        // TODO(sebas): make output std::optional?
        std::vector< std::shared_ptr< UnfinishedProjection > > project_select()
        {
            std::vector< std::shared_ptr< UnfinishedProjection > > copies;
            if ( selects_to_continue_from.empty() )
                return copies;

            auto first_it = selects_to_continue_from.begin();
            auto node = *first_it;
            if ( is_decode_select( node->original ) )
            {
                Select *sel = static_cast< Select * >( node->original );
                circ::check( sel->operands_size() >= 3 ) << "not enough operands for select";
                // minus 1 for selector bit
                for ( std::size_t i = 2; i < sel->operands_size(); i++ )
                {
                    auto select_prefix
                        = "select" + std::to_string( sel->id() ) + "copy" + std::to_string( i );
                    auto copy = deep_copy( select_prefix );
                    auto to_update = std::find_if( copy->selects_to_continue_from.begin(),
                                                   copy->selects_to_continue_from.end(),
                                                   [ & ]( std::shared_ptr< Tree > tree )
                                                   { return tree->original == sel; } );
                    if ( to_update == copy->selects_to_continue_from.end() )
                        std::cout << "were BAD" << std::endl;
                    circ::check( to_update != copy->selects_to_continue_from.end() )
                        << "select could not be found in copied tree";
                    to_update->get()->original = sel->operand( i );
                    to_update->get()->node
                        = std::make_shared< SEGNode >( segnode_prefix + "copy_prefix_select" );
                    /*
                     * the value of to_update might can be changed into a select or a non-select
                     * node we need to update this. if it's a select it remains in the correct
                     * spot, so only update if it's not
                     */
                    if ( !is_decode_select( to_update->get()->original ) )
                    {
                        std::shared_ptr< Tree > t
                            = std::make_shared< Tree >( *to_update->get() );
                        check( t->node->id == ( *to_update )->node->id )
                            << "not updated properly";
                        auto old_id = t->node->id;
                        copy->to_continue_from.insert( t );
                        copy->selects_to_continue_from.erase( to_update );
                        check( t->node->id == old_id ) << "not updated properly";
                    }

                    copy->select_choices.insert( SelectChoice { sel, i } );
                    copy->fully_extend();
                    copies.push_back( std::make_shared< UnfinishedProjection >( *copy ) );
                }
                // update last inplace
                select_choices.insert( SelectChoice { sel, 1 } );
                node->original = sel->operand( 1 );
                node->node
                    = std::make_shared< SEGNode >( segnode_prefix + "copy_prefix_select" );
            }
            selects_to_continue_from.erase( first_it );
            return copies;
        }

        void extend_except_select( std::shared_ptr< Tree > tree )
        {
            if ( is_decode_select( tree->original ) )
            {
                auto s = std::find_if( selects_to_continue_from.begin(),
                                       selects_to_continue_from.end(),
                                       [ & ]( std::shared_ptr< Tree > t )
                                       { return t->original == tree->original; } );
                if ( s == selects_to_continue_from.end() )
                {
                    selects_to_continue_from.insert( tree );
                }
                return;
            }

            if ( tree->original->operands_size() == 0 )
                return;

            if ( !tree->children.empty() ) // already extended
                return;

            for ( auto op_child : tree->original->operands() )
            {
                // assumes no transitive advices
                if ( isa< Advice >( op_child ) )
                {
                    auto advice = static_cast< Advice * >( op_child );
                    auto advice_value = get_op_attached_to_advice_in_vi( advice, vi );
                    op_child = advice_value;
                }

                if ( seen_nodes.contains( op_child->id() ) )
                {
                    tree->node->add_child( seen_nodes[ op_child->id() ]->node );
                    tree->add_child( seen_nodes[ op_child->id() ] );
                }
                else
                {
                    auto new_seg_node = std::make_shared< SEGNode >(
                        segnode_prefix + "_" + std::to_string( op_child->id() ) );
                    auto new_child = std::make_shared< Tree >();
                    new_child->original = op_child;
                    new_child->node = new_seg_node;
                    tree->node->add_child( new_seg_node );
                    tree->add_child( new_child );
                    to_continue_from.insert( new_child );
                    seen_nodes.insert( { op_child->id(), new_child } );
                }
            }
        }

        bool is_finished();

        std::unique_ptr< UnfinishedProjection > deep_copy( const std::string &id_postfix )
        {
            std::map< std::size_t, std::shared_ptr< Tree > > copied_nodes;
            auto old_root = projection;

            auto new_root_proj = std::make_shared< Tree >();
            new_root_proj->original = old_root->original;
            new_root_proj->node
                = std::make_shared< SEGNode >( segnode_prefix + id_postfix + "root" );
            UnfinishedProjection copy( segnode_prefix + id_postfix, vi, new_root_proj,
                                       select_choices );

            for ( auto &c : old_root->children )
            {
                auto child = deep_copy_child( id_postfix, *c, copied_nodes, copy );
                new_root_proj->node->add_child( child->node );
                new_root_proj->add_child( child );
            }

            if ( std::find_if( to_continue_from.begin(), to_continue_from.end(),
                               [ & ]( const std::shared_ptr< Tree > &t )
                               { return projection->node->id == t.get()->node->id; } )
                 != to_continue_from.end() )
                copy.to_continue_from.insert( copy.projection );

            if ( std::find_if( selects_to_continue_from.begin(), selects_to_continue_from.end(),
                               [ & ]( const std::shared_ptr< Tree > &t )
                               { return projection->node->id == t.get()->node->id; } )
                 != selects_to_continue_from.end() )
                copy.selects_to_continue_from.insert( copy.projection );

            return std::make_unique< UnfinishedProjection >( copy );
        };

    private:
        void init_root_tree() { projection->node->isRoot = true; }

        bool is_decode_select( Operation *op )
        {
            // TODO(sebas): check meta data to see if it tainted to be decode time, should also
            // move this outside the class
            return isa< Select >( op );
        }

        std::shared_ptr< Tree >
        deep_copy_child( const std::string &id_postfix, const Tree &copy_from,
                         std::map< std::size_t, std::shared_ptr< Tree > > &copied_nodes,
                         UnfinishedProjection &copy_to )
        {
            auto old_seg = copy_from.node;
            auto old_op = copy_from.original;
            std::shared_ptr< SEGNode > copy_of_value;
            auto new_tree = std::make_shared< Tree >();

            // make value
            if ( copied_nodes.contains( old_op->id() ) )
                return copied_nodes[ old_op->id() ];

            copy_of_value = std::make_shared< SEGNode >( old_seg->id + id_postfix );
            new_tree->original = old_op;
            new_tree->node = copy_of_value;

            // if we should extend the node in the current project, then we should do that in
            // the new projection as well
            if ( std::find_if( to_continue_from.begin(), to_continue_from.end(),
                               [ & ]( const std::shared_ptr< Tree > t )
                               { return copy_from.node->id == t->node->id; } )
                 != to_continue_from.end() )
                copy_to.to_continue_from.insert( new_tree );

            // if we should extend the node in the current project, then we should do that in
            // the new projection as well
            if ( std::find_if( selects_to_continue_from.begin(), selects_to_continue_from.end(),
                               [ & ]( const std::shared_ptr< Tree > t )
                               { return copy_from.node->id == t->node->id; } )
                 != selects_to_continue_from.end() )
                copy_to.selects_to_continue_from.insert( new_tree );

            /*
             * Each projection keeps track of a list of nodes which it already has created
             * In the new projection we have different segnodes and we need to update which
             * nodes they link to
             */
            if ( std::find_if( seen_nodes.begin(), seen_nodes.end(),
                               [ & ]( auto &t )
                               { return copy_from.original->id() == t.first; } )
                 != seen_nodes.end() )
                seen_nodes.insert( { new_tree->original->id(), new_tree } );

            for ( auto child : copy_from.children )
            {
                auto new_child = deep_copy_child( id_postfix, *child, copied_nodes, copy_to );
                new_tree->node->add_child( new_child->node );
                new_tree->add_child( new_child );
            }

            copied_nodes.insert( { old_op->id(), new_tree } );
            return new_tree;
        };
    };

    void specialize( std::map< std::string, Operation * > &specs,
                     std::shared_ptr< SEGNode > node, Operation *op );

    std::unique_ptr< SEGGraph > circ_to_segg( CircuitPtr circuit );

    template < gap::graph::graph_like g >
    void absorb_node_into_an_existing_root( g &graph, const SEGGraph::node_pointer &to_hash );

    /*
     * Deduplicate nodes with the same hash. Effectively turning the graph into a multi-graph
     * where nodes have unique hashes/sub-trees
     */
    template < gap::graph::graph_like g >
    void dedup( g &graph )
    {
        std::map< std::string, std::shared_ptr< SEGNode > > seen_hash;
        for ( auto &to_hash : gap::graph::dfs< gap::graph::yield_node::on_close >( graph ) )
        {
            auto pair_iter_inserted = seen_hash.try_emplace( to_hash->get_hash(), to_hash );
            if ( pair_iter_inserted.second == false )
            { // item already existed hash replace others now
                auto pre_existinging_hashed_node = ( *pair_iter_inserted.first ).second;

                bool foundEdge = false;
                // find parent of target that was already hashed and replace it
                for ( auto &e : graph.edges() )
                {
                    // we want to replace it, and we don't replace the one with the one want to
                    // replace with
                    if ( e.target()->id == to_hash->id
                         && e.target()->id != pre_existinging_hashed_node->id )
                    {
                        foundEdge = true;
                        e.source()->replace_all_nodes_by_id( pre_existinging_hashed_node,
                                                             to_hash->id );
                        graph.remove_node( to_hash );
                        break;
                    }
                }
                // must be parent of a subtree, now replace those
                if ( !foundEdge )
                {
                    if ( !to_hash->isRoot )
                        unreachable()
                            << "all nodes require an incoming edge or be marked as root.";
                    absorb_node_into_an_existing_root( graph, to_hash );
                }
            }
        }
    }

    template < gap::graph::graph_like g >
    void absorb_node_into_an_existing_root( g &graph, const SEGGraph::node_pointer &to_hash )
    { // two nodes which are both roots, and have same hashes can be merged
        for ( std::shared_ptr< SEGNode > &to_merge_with : graph.nodes() )
        {
            if ( to_hash->id != to_merge_with->id && to_merge_with->isRoot && to_hash->isRoot
                 && to_hash->get_hash() == to_merge_with->get_hash() )
            {
                to_merge_with->valid_for_contexts.merge( to_hash->valid_for_contexts );
                graph.remove_node( to_hash );
                break;
            }
        }
    }

    decoder::FunctionCall print_SEGNode_tree( SEGNode &node, std::string stack_name,
                                              Operation *op );

    std::pair< decoder::Var, decoder::StatementBlock > expr_for_node(
        std::unordered_map< SEGNode, decoder::FunctionDeclaration, segnode_hash_on_get_hash,
                            segnode_comp_on_hash > &func_decls,
        UniqueNameStorage &unique_names_storage, const SEGNode &node, std::vector<decoder::Var> arg_names );

}
