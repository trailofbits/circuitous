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

int main(int argc, char *argv[]) {
//    std::cout << printmeeeeeee() << std::endl;
//
//    circ::SEGGraph segg;
//
//    circ::SEGNode sg("root");
//    auto n = std::make_shared<circ::SEGNode>("n0");
//    auto n1 = std::make_shared<circ::SEGNode>("n1"); // contians 23
//    auto n2 = std::make_shared<circ::SEGNode>("n2");
//    auto n3 = std::make_shared<circ::SEGNode>("n3");
//    auto n4 = std::make_shared<circ::SEGNode>("n4"); // contains 56
//    auto n5 = std::make_shared<circ::SEGNode>("n5");
//    auto n6 = std::make_shared<circ::SEGNode>("n6");
//
//    sg.add_child(n);
//    n->add_child(n1);
//    n1->add_child(n2);
//    n1->add_child(n3);
//    sg.add_child(n4);
//    n4->add_child(n5);
//    n4->add_child(n6);
//
//    //
////    n->_nodes.push_back(n1);
////    n->_nodes.push_back(n4);
////    n1->_nodes.push_back(n2);
////    n1->_nodes.push_back(n3);
////
////    n4->_nodes.push_back(n5);
////    n4->_nodes.push_back(n6);
//    segg._nodes.push_back(std::make_shared<circ::SEGNode>(sg));
//    segg._nodes.push_back(n1);
//    segg._nodes.push_back(n2);
//    segg._nodes.push_back(n3);
//    segg._nodes.push_back(n4);
//    segg._nodes.push_back(n5);
//    segg._nodes.push_back(n6);
//
//
////    std::vector<int> vec = {1,2,3,4,5};
////    int tval  =2;
////    std::cout << "before " << vec.size() << std::endl;
////    std::remove_if(std::begin(vec), std::end(vec), [&](int val) { return tval == val; });
////    vec.erase(1);
////    std::cout << "after " << vec.size() << std::endl;
//
//    for(auto& to_hash : gap::graph::dfs<gap::graph::yield_node::on_close>(segg))
//        std::cout << to_hash->id << ": " << to_hash->get_hash() << std::endl;
//    std::cout << "---------------------" << std::endl;
//    circ::dedup(segg);
//    std::cout << "---------------------" << std::endl;
//    circ::dedup(segg);
//    circ::dfs_graph(segg);
//    circ::print_nodes_graph(segg);
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
    std::cout << "---------------------" << std::endl;
    for(auto& to_hash : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
        std::cout << to_hash->id << ": " << to_hash->get_hash() << std::endl;


    std::cout << "Number of functions: " << std::count_if(seg->_nodes.begin(), seg->_nodes.end(), [](auto node) { return node->isRoot;}) <<  " out of #paths: " << seg->_nodes.size() << std::endl;


    // pair(Node, Op*)

    std::map<std::string, circ::Operation*> speciliazes;
//    circ::specialize(speciliazes, )
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
        if(node->isRoot)
        {
            for(auto & op : node->root_operations)
                circ::specialize(speciliazes, node, op );
        }
    }


    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
        std::cout << node->id << "( " << node->inline_cost << "): " << node->print_hash() << std::endl;

    std::cout << "newwww" << std::endl;

    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {

        node->inline_cost = 1;
        for(auto &c : node->_nodes)
        {
            if(c->fd)
                node->inline_cost++;
            else
                node->inline_cost += c->inline_cost;
        }


//        node->inline_cost
//            = std::accumulate( node->children().begin(), node->children().end(), 0,
//                               []( int current, std::shared_ptr< circ::SEGNode > n ) {
//                                    if(n->fd == false)
//                                        return current + n->inline_cost;
//                                    else
//                                        return current + 1;
//                               } );
        if(node->inline_cost >= 5)
            node->fd = true;

        std::cout << node->id << "( " << node->inline_cost;
        if(node->fd)
            std::cout << "*" << "";
        std::cout << " ): " << node->print_hash()<< std::endl;
    }


    /*
     * Next up, specializations
     */

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
