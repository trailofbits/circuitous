/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Decoder/DecodeAST.hpp>
#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Decoder/GenerationHelpers.hpp>
#include <circuitous/Decoder/SEGGraph.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <gap/core/graph.hpp>
#include <iostream>
#include <map>
#include <memory>

namespace circ::decoder
{
    // TODO(sebas): Move to gap?
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
        std::vector< decoder::Var > get_n_var_names( int amount_of_names, Type type_name );
        std::vector< decoder::Var > names;
        int counter = 0;
    };

    struct SEGGraphPrinter
    {
        explicit SEGGraphPrinter( circuit_ref_t circuit, std::ostream &os ) :
            seg_graph( circuit ), circuit( circuit ), os( os ), ep( os )
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

        decoder::FunctionDeclaration get_func_decl( std::shared_ptr< SEGNode > node );

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
        using projection_maps = std::multimap< Operation *, seg_projection >;

        void create_from_seg_printer();

        bool selects_emission_locations_are_constant( const projection_maps &proj_groups,
                                                      Operation *key );

        void expr_for_proj( const std::pair< InstructionProjection, std::shared_ptr< SEGNode > >
                                &instr_node_pair );

        void expr_for_proj_with_const_sel_loc( projection_maps &proj_groups, Operation *key );

        const std::string member_variable_prefix = "node_";
    };

    std::pair< decoder::Var, decoder::StatementBlock > expression_for_seg_node(
        std::unordered_map< SEGNode, decoder::FunctionDeclaration, segnode_hash_on_get_hash,
                            segnode_comp_on_hash > &func_decls,
        UniqueNameStorage &unique_names_storage, const SEGNode &node,
        std::vector< decoder::Var > arg_names );

    bool operation_has_nested_select( const InstructionProjection &projection );
}
