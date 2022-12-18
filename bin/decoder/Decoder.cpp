/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */
#include <circuitous/IR/Verify.hpp>
#include <circuitous/Transforms.hpp>
#include <circuitous/IR/Cost.hpp>
#include <circuitous/IR/Serialize.hpp>

#include <circuitous/Printers/Verilog.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Util/CmdParser.hpp>

#include <circuitous/Support/Ciff.hpp>
#include <circuitous/Support/CLIArgs.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/Lifter/CircuitSmithy.hpp>

#include <circuitous/Decoder/DecoderPrinter.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <gflags/gflags.h>
#include <glog/logging.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <remill/OS/OS.h>

#include <iostream>
#include <unordered_map>

#include <ranges>

#include <circuitous/SEG/SEGMultiGraph.hpp>

// TODO(lukas): Clean this up once remill gets rid of gflags.
DEFINE_string(arch, "", "");
DEFINE_string(os, REMILL_OS, "");

DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");

DEFINE_string(dec_out, "", "Path to the output decoder file.");

DEFINE_string(bytes_in, "", "Hex representation of bytes to be lifted");
DEFINE_string(ciff_in, "", "Load input from circuitous-seed --dbg produced file");



namespace cli = circ::cli;

std::string_view as_string_view(std::vector< uint8_t > &buf)
{
    return std::string_view( reinterpret_cast<char *>(buf.data()), buf.size());
}

using input_options = circ::tl::TL<
    cli::CiffIn,
    cli::IRIn,
    cli::BytesIn
>;

using output_options = circ::tl::TL<
    circ::cli::DecoderOut
>;
using remill_config_options = circ::tl::TL<
    circ::cli::Arch,
    circ::cli::OS
>;

using other_options = circ::tl::TL<
    circ::cli::Help,
    circ::cli::Version
>;

using cmd_opts_list = circ::tl::merge<
    input_options,
    output_options,
    remill_config_options,
    other_options
>;

circ::CircuitPtr get_input_circuit(auto &cli)
{
    auto make_circuit = [&](auto buf) {
        circ::log_info() << "Going to make circuit";
        circ::Ctx ctx{ *cli.template get< cli::OS >(), *cli.template get< cli::Arch >() };
        return circ::CircuitSmithy(std::move(ctx)).smelt(buf).forge();
    };

    if (auto bytes = cli.template get< cli::BytesIn >())
        return make_circuit(as_string_view(*bytes));

    if (auto ir_file = cli.template get< cli::IRIn >())
        return circ::deserialize(*ir_file);

    if (auto cif = cli.template get< cli::CiffIn >())
        return make_circuit(circ::CIFFReader().read(*cif).take_bytes());
    return {};
}


template< typename OptsList >
auto parse_and_validate_cli(int argc, char *argv[]) -> std::optional< circ::ParsedCmd >
{
    using namespace circ;

    static const auto yield_err = [&](const auto &msg)
    {
        std::cerr << msg << std::endl;
    };

    auto parsed = circ::CmdParser< OptsList >::parse_argv(argc, argv);
    if (!parsed)
    {
        std::cerr << "Command line arguments were not parsed correctly, see "
                  << "stderr for more details.";
        return {};
    }

    auto v = Validator(parsed);

    if (v.check(is_singleton< cli::Help >())
         .check(is_singleton< cli::Version >())
         .process_errors(yield_err))
    {
        return {};
    }

    if (v.check(one_of< input_options >())
         .process_errors(yield_err))
    {
        return {};
    }

    if (v.validate_leaves( OptsList{} ).process_errors(yield_err))
        return {};

    return parsed;
}

using seg_node_ptr = std::shared_ptr< circ::SEGNode >;

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
            circ::unreachable() << "non-isomorphic generators left:" << is_end_s <<  " right " << is_end_t;

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


int main(int argc, char *argv[]) {
    auto maybe_parsed_cli = parse_and_validate_cli< cmd_opts_list >(argc, argv);
    if (!maybe_parsed_cli)
    {
        std::cerr << circ::help_str(cmd_opts_list());
        return 1;
    }

    auto parsed_cli = std::move(*maybe_parsed_cli);

    if (parsed_cli.template present< circ::cli::Help >())
    {
        std::cout << circ::help_str(cmd_opts_list());
        return 0;
    }

    if (parsed_cli.template present< circ::cli::Version >())
    {
        std::cerr << "TODO: Implement proper version message";
        return 1;
    }

    // NOTE(lukas): Support libraries still need to be initialized, since
    //              remill may be using/relying on them.
    google::ParseCommandLineFlags(&argc, &argv, true);
    google::InitGoogleLogging(argv[0]);

    auto circuit = get_input_circuit(parsed_cli);
    if (!circuit)
    {
        circ::unreachable() << "Not able to load circuit.";
        return 3;
    }

    VerifyCircuit("Verifying loaded circuit.", circuit.get(), "Circuit is valid.");

    auto seg = circ::circ_to_segg(circuit.get());


    std::cout << "Number of starting nodes: "  << seg->_nodes.size() << std::endl;
    circ::dedup(*seg.get());
    std::cout << "dedup nodes: "  << seg->_nodes.size() << std::endl;

    std::map<std::string, circ::Operation*> speciliazes;
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
        if(node->isRoot)
        {
            for(auto & op : node->roots)
                circ::specialize(speciliazes, node, op.second );
        }
    }

    // calc inline cost and declare fd if possible
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
        node->inline_cost
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< circ::SEGNode > n ) {
                                    if(n->fd == false)
                                        return current + n->inline_cost;
                                    else
                                        return current + 1;
                               } );
        if(node->inline_cost >= 2 || node->isRoot)
            node->fd = true;
    }

    // count nodes in subtree for every node
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
        node->subtree_count
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< circ::SEGNode > n )
                               { return current + n->subtree_count; } );
    }




    circ::decoder::ExpressionPrinter ep(std::cout);
    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration, circ::segnode_hash_on_get_hash, circ::segnode_comp_on_hash> func_decls;
    circ::UniqueNameStorage name_storage;
    circ::decoder::Var stack("stack");

    // get maximum number of nodes used for a single VI. This determines how large the stack should be
    std::vector<int> vals;
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        auto root_nodes_for_vi = seg->get_nodes_by_vi(vi);
        int a = std::accumulate(root_nodes_for_vi.begin(), root_nodes_for_vi.end(), 0,
                                 [](auto left, auto &p) {
                                     return left + p.second->subtree_count;
                                 });
        vals.push_back(a);

    }
    auto max_size = *std::max_element(vals.begin(), vals.end());

    auto max_size_var = circ::decoder::Var("MAX_SIZE_INSTR");
    ep.print(circ::decoder::Assign(circ::decoder::VarDecl(max_size_var), circ::decoder::Int(max_size)));
    // this does too much somehow
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        std::cout << std::endl << "For vi: " << vi->id() << std::endl;
        int counter = 0;
        for(auto & [op, node ] : seg->get_nodes_by_vi(vi) )
        {

            circ::expr_for_node( func_decls, name_storage, *node, stack,  &counter, max_size_var );
            std::cout << op->name() << "(" << node->inline_cost <<  (node->fd ? "*" : "") << "): " << node->print_hash() << std::endl;
        }
    }

    std::cout << "size of func_decls " << func_decls.size() << std::endl << std::endl;

    /*
     * Emission is two phased:
     *      Phase 1: emit the functions representing the semantics emitter which will get used by the decoder
     *      Phase 2: emit the functions for the decoder
     *
     * The first phase will be walking over the SEG graph and outputting its trees
     * while introducing a function for everything which had the function declartion set
     *
     * The second phase will be going over every VI in the system (the domain of the decoder)
     * then walk the the circIR and corresponding semantics emitter graph in tandem
     * so that the decoder can prefill the stack in a correct manner
     */
    // Print semantics emission functions
    // func decls orders based on hash buckets, need to traverse the graph to keep proper shape of the call graph
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get())) {
        auto func = func_decls.find(*node);
        if(func != func_decls.end())
        {
            auto fd= *func;
            std::cout << "// called externally: " << fd.first.isRoot << " hash: " << fd.first.get_hash() << std::endl;
            ep.print(fd.second);
            std::cout << std::endl;
        }
    }


    // print decoder
    for(circ::VerifyInstruction* vi : circuit->attr<circ::VerifyInstruction>())
    {
        circ::decoder::FunctionDeclarationBuilder fdb;
        int stack_counter = 0;
        for(auto [vis, node] : seg->get_nodes_by_vi(vi))
        {
            auto stack_counter_for_call = stack_counter;
            auto start_op = vis;
            auto start_op_ptr = std::make_shared< circ::nodeWrapper >( start_op );
            auto op_gen = non_unique_dfs< gap::graph::yield_node::on_open >( start_op_ptr );
            auto node_gen = non_unique_dfs< gap::graph::yield_node::on_open >( node );

            for ( auto &[ op, nod ] : tuple_generators( op_gen, node_gen ) )
            {
                auto lhs = circ::decoder::Id("stack[" + std::to_string(stack_counter) + "]");
                fdb.body_insert(circ::decoder::Assign( lhs , circ::decoder::Id(op->op->name()) ));
                stack_counter++;
            }
            auto sem_entry = func_decls.find(*node);
            fdb.body_insert(circ::decoder::FunctionCall(sem_entry->second.function_name,{circ::decoder::Id("stack"), circ::decoder::Int(stack_counter_for_call)}));
        }

        ep.print(fdb.make());
    }

//    if (auto dec_out = parsed_cli.template get< cli:: DecoderOut >()){
//        if ( *dec_out != "cout" ) {
//            auto o = std::ofstream ( *dec_out );
//            auto decGen = circ::decoder::DecoderPrinter( circuit, o );
//            decGen.print_file();
//        }
//        else {
//            auto decGen = circ::decoder::DecoderPrinter( circuit );
//            decGen.print_file();
//        }
//    }
//    else{
//        circ::unreachable() << "Decoder out was not specified";
//    }


    return 0;
}
