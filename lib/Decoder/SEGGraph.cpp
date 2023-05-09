/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Decoder/SEGGraph.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <circuitous/IR/Shapes.hpp>

namespace circ::decoder
{
    std::vector< std::shared_ptr< SEGNode > > SEGNode::children()
    {
        return _nodes;
    }

    std::string SEGNode::get_hash() const
    {
        std::stringstream ss;
        ss << std::to_string( _nodes.size() );
        ss << "|";
        for ( std::shared_ptr< SEGNode > n : _nodes )
            ss << n->get_hash();

        return ss.str();
    }

    SEGNode::SEGNode( const std::string &id ) : id( id ) { }
    std::vector< std::shared_ptr< SEGNode > > SEGNode::parents()
    {
        return _parents;
    }

    void SEGGraph::prepare()
    {
        extract_all_seg_nodes_from_circuit();
        dedup( *this );
        calculate_costs();
    }

    void SEGNode::add_child( SEGNode::node_pointer child )
    {
        this->_nodes.push_back( child );
        child->_parents.push_back( std::make_shared< SEGNode >( *this ) );
    }

    void SEGNode::replace_all_nodes_by_id( std::shared_ptr< SEGNode > new_target,
                                           std::string target_id )
    {
        std::transform( std::begin( _nodes ), std::end( _nodes ), std::begin( _nodes ),
                        [ & ]( auto &node_ptr )
                        {
                            if ( node_ptr != nullptr && node_ptr->id == target_id )
                                return new_target;
                            else
                                return node_ptr;
                        } );
    }

    std::vector< std::shared_ptr< SEGNode > > SEGGraph::nodes() const
    {
        return _nodes;
    }

    gap::generator< SEGGraph::edge_type > SEGGraph::edges() const
    {
        for ( auto node : _nodes )
        {
            for ( auto child : node->children() )
            {
                co_yield edge_type { node, child };
            }
        }
    }

    void SEGGraph::remove_node( const SEGGraph::node_pointer &node )
    {
        auto target_id = node->id;
        auto ids_match = [ & ]( const std::shared_ptr< SEGNode > &node )
        {
            return node->id == target_id;
        };
        std::erase_if( _nodes, ids_match );
    }

    std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > > >
    SEGGraph::get_nodes_by_vi( VerifyInstruction *vi )
    {
        // TODO(sebas): build this once instead of building this per call
        std::vector< std::pair< InstructionProjection, std::shared_ptr< SEGNode > > > m;
        for ( auto &n : nodes() )
        {
            for ( auto &root : n->valid_for_contexts )
            {
                if ( root.vi == vi
                     && n->isRoot ) // this root shares the correct vi, so we should copy it
                    m.push_back( { root, n } );
            }
        }

        circ::check( m.size() != 0 ) << "Could not retrieve SEGNode for VI";
        return m;
    }

    void SEGGraph::calculate_costs()
    {
        // calc inline cost and declare fd if possible
        for ( auto &node : gap::graph::dfs< gap::graph::yield_node::on_close >( *this ) )
        {
            node->inline_cost = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                                                 []( int current, std::shared_ptr< SEGNode > n )
                                                 {
                                                     if ( n->fd == false )
                                                         return current + n->inline_cost;
                                                     else
                                                         return current + 1;
                                                 } );

            if ( node->inline_cost >= 2 || node->isRoot )
                node->fd = true;

            node->subtree_count
                = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                                   []( int current, std::shared_ptr< SEGNode > n )
                                   { return current + n->subtree_count; } );
        }
    }

    void SEGGraph::extract_all_seg_nodes_from_circuit()
    {
        inspect::LeafToVISubPathCollector subPathCollector;
        std::vector< std::shared_ptr< SEGNode > > nodes;
        int vi_counter = 0;

        for ( VerifyInstruction *vi : circuit->attr< circ::VerifyInstruction >() )
        {
            auto ltt_paths = collect::DownTree< constraint_opts_ts >().run( vi );
            int path_counter = 0;

            for ( auto &path : ltt_paths.collected )
            {
                // these constraints are useless by themselves as others will use them instead
                if ( isa< AdviceConstraint >( path ) )
                    continue;

                auto prefix = "vi_" + std::to_string( vi_counter ) + "_path"
                              + std::to_string( path_counter ) + "_node";
                UnfinishedProjection up( prefix, vi, path );
                up.fully_extend();
                save_nodes_from_projection( nodes, up );
                path_counter++;
            }
            vi_counter++;
        }
        _nodes = nodes;
    }

    void SEGGraph::save_nodes_from_projection( std::vector< std::shared_ptr< SEGNode > > &nodes,
                                               UnfinishedProjection &up ) const
    {
        auto node_gen
            = gap::graph::dfs< gap::graph::yield_node::on_open >( up.projection.get()->node );
        for ( auto node : node_gen )
            nodes.push_back( node );

        for ( auto copy : up.created_projections )
        {
            auto node_gen_proj
                = gap::graph::dfs< gap::graph::yield_node::on_open >( copy->projection->node );
            for ( auto node : node_gen_proj )
                nodes.push_back( node );
        }
    }

    UnfinishedProjection::UnfinishedProjection( const std::string &segnodePrefix,
                                                VerifyInstruction *vi,
                                                std::shared_ptr< Tree > root,
                                                const choice_set &choices ) :
        segnode_prefix( segnodePrefix )
    {
        this->vi = vi;
        this->projection = root;
        this->select_choices = choices;
        init_root_tree();
    }

    UnfinishedProjection::UnfinishedProjection( const std::string &segnodePrefix,
                                                VerifyInstruction *vi, Operation *root ) :
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

    void UnfinishedProjection::fully_extend()
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

    void UnfinishedProjection::finish_up_to_decode_select()
    {
        while ( !to_continue_from.empty() )
        {
            auto first_item = to_continue_from.begin();
            extend_except_select( *first_item );
            to_continue_from.erase( first_item );
        }
    }

    std::vector< std::shared_ptr< UnfinishedProjection > >
    UnfinishedProjection::project_select()
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
                    std::shared_ptr< Tree > t = std::make_shared< Tree >( *to_update->get() );
                    check( t->node->id == ( *to_update )->node->id ) << "not updated properly";
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
            node->node = std::make_shared< SEGNode >( segnode_prefix + "copy_prefix_select" );
        }
        selects_to_continue_from.erase( first_it );
        return copies;
    }

    void UnfinishedProjection::extend_except_select( std::shared_ptr< Tree > tree )
    {
        if ( is_decode_select( tree->original ) )
        {
            auto s = std::find_if(
                selects_to_continue_from.begin(), selects_to_continue_from.end(),
                [ & ]( std::shared_ptr< Tree > t ) { return t->original == tree->original; } );
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
                op_child = advice->value( vi );
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

    std::unique_ptr< UnfinishedProjection >
    UnfinishedProjection::deep_copy( const std::string &id_postfix )
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
    }

    bool UnfinishedProjection::is_decode_select( Operation *op )
    {
        // TODO(sebas): check meta data to see if it tainted to be decode time, should also
        // move this outside the class
        return isa< Select >( op );
    }

    std::shared_ptr< Tree > UnfinishedProjection::deep_copy_child(
        const std::string &id_postfix, const Tree &copy_from,
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
                           [ & ]( auto &t ) { return copy_from.original->id() == t.first; } )
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
    }
}
