#pragma once

#include "circuitous/Decoder/DecoderPrinter.hpp"
#include "circuitous/Decoder/GenerationHelpers.hpp"
#include "circuitous/Decoder/SEGGraph.hpp"

#include <circuitous/Decoder/DecodeAST.hpp>
#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>

namespace circ::decoder
{
    //TODO(sebas): Move to gap?
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
        decoder::Var get_unique_var_name();
        decoder::Var get_unique_var_name( Type t );
        std::vector< decoder::Var > get_n_var_names( int amount_of_names,
                                                     Type type_name );
        std::vector< decoder::Var > names;
        int counter = 0;
    };

    struct SEGGraphPrinter
    {
        explicit SEGGraphPrinter( circuit_ref_t circuit, std::ostream &os ) :
            seg_graph(circuit), circuit(circuit) , os( os ), ep( os )
        {
            seg_graph.prepare();
            generate_function_definitions();
        };

        void print_semantics_emitter();
        void print_helper_functions();
        static constexpr const auto extract_helper_function = "extraction_helper";

        std::unordered_map< SEGNode, decoder::FunctionDeclaration, segnode_hash_on_get_hash,
                            segnode_comp_on_hash >
            func_decls;

        SEGGraph seg_graph;

        decoder::FunctionDeclaration get_func_decl(std::shared_ptr<SEGNode> node);

    private:
        circuit_ref_t circuit;
        std::ostream &os;
        decoder::ExpressionPrinter ep;

        UniqueNameStorage name_storage;

        void generate_function_definitions();
    };

    struct DecodedInstrGen
    {
        DecodedInstrGen( SEGGraphPrinter *seg_graph_printer,
                         IndependentSelectEmissionHelper &select_emission_helper,
                         VerifyInstruction *vi, const std::string &name ) :
            vi( vi ),
            name( name ), seg_graph_printer( seg_graph_printer ),
            select_emission_helper( select_emission_helper ),
            main_to_tuple_call( FunctionCall( name, {} ) ),
            decode_time_expression_creator( SimpleDecodeTimeCircToExpressionVisitor(
                vi, decoder::inner_func_arg1, decoder::inner_func_arg2,
                decoder::extract_helper_function_name ) )
        {
        }

        Struct create_struct();

        VerifyInstruction *vi;
        std::string name;

        SEGGraphPrinter *seg_graph_printer;
        IndependentSelectEmissionHelper &select_emission_helper;

        ConstructorDeclarationBuilder tuple_constructor;
        ConstructorDeclarationBuilder main_constructor;
        FunctionCall main_to_tuple_call;

        decoder::FunctionDeclarationBuilder fdb_visit;

        std::vector< decoder::VarDecl > member_declarations;
        std::vector< decoder::Assign > member_initializations;

        std::size_t size = 0;
        decoder::VarDecl get_next_free_data_slot();

    private:
        SimpleDecodeTimeCircToExpressionVisitor decode_time_expression_creator;
        using projection_maps =  std::multimap< Operation *, seg_projection >;

        void create_from_seg_printer();

        bool selects_emission_locations_are_constant(
            const projection_maps &proj_groups, Operation *key );

        void expr_for_proj(
            const std::pair< InstructionProjection, std::shared_ptr< SEGNode > >
                &instr_node_pair );

        void expr_for_proj_with_const_sel_loc( projection_maps &proj_groups,
                                                           Operation *key );

        const std::string member_variable_prefix = "node_";
    };

    /*
     * This structure is an intermediate representation of a circIR subtree being converted to an
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

    std::pair< decoder::Var, decoder::StatementBlock > expression_for_seg_node(
        std::unordered_map< SEGNode, decoder::FunctionDeclaration, segnode_hash_on_get_hash,
                            segnode_comp_on_hash > &func_decls,
        UniqueNameStorage &unique_names_storage, const SEGNode &node,
        std::vector< decoder::Var > arg_names );

    bool operation_has_nested_select( const InstructionProjection &projection );
}
