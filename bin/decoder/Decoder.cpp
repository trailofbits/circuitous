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

//std::ranges::view tuple






//class CircIRGraph
//{
//public:
//
//    using node_type = circ::Operation*;
//    using edge_type = edge_t< node_type >;
//
//    using node_pointer = circ::Opernation*;
//
//    gap::generator< node_pointer > nodes() const {
//        for (auto ch : _nodes)
//            co_yield ch;
//    }
//
//    generator< edge_type > edges() const {
//        for (auto node : _nodes) {
//            for (auto child : node->children()) {
//                co_yield edge_type{ node, child };
//            }
//        }
//    }
//    CircIRGraph( circ::Operation *root ) : root( root ) { }
//    gap::recursive_generator
//private:
//    circ::Operation *root;
//};

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
//    circ::SEGGraph tg1;
//    auto n = std::make_shared<circ::SEGNode>("n0");
//    auto n1 = std::make_shared<circ::SEGNode>("n1"); // contians 23
//    auto n2 = std::make_shared<circ::SEGNode>("n2");
//    n->add_child(n1);
//    n1->add_child(n2);
//
//    tg1._nodes.push_back(n);
//    tg1._nodes.push_back(n1);
//    tg1._nodes.push_back(n2);
//
//    circ::SEGGraph tg2;
//    tg2._nodes.push_back(n);
//    tg2._nodes.push_back(n1);
//    tg2._nodes.push_back(n2);
//    auto dfs_tg2 = gap::graph::dfs<gap::graph::yield_node::on_close>(tg2);
//
//    auto dfs_tg1 = gap::graph::dfs<gap::graph::yield_node::on_open>(tg1);
////    std::cout << decltype(dfs_tg1) << std::endl;
//    using seg_node_t = std::shared_ptr<circ::SEGNode>;
//
//
//
//
//    for( auto [left, right] : tuple_generators<seg_node_t, seg_node_t>(dfs_tg1, dfs_tg2))
//        std::cout << left->id << ", " << right->id << std::endl;
//    return 2;



//    std::cout << printmeeeeeee() << std::endl;
//

//
//    circ::SEGNode sg("root");
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
    std::cout << "dedup nodes: "  << seg->_nodes.size() << std::endl;

    std::cout << "specialize" << std::endl;
    std::map<std::string, circ::Operation*> speciliazes;
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
        if(node->isRoot)
        {
            for(auto & op : node->roots)
                circ::specialize(speciliazes, node, op.second );
        }
    }

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

    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
    {
//        if(node->_nodes.empty())
//            node->subtree_count = 1;
//        else
            node->subtree_count
            = std::accumulate( node->_nodes.begin(), node->_nodes.end(), 1,
                               []( int current, std::shared_ptr< circ::SEGNode > n )
                               { return current + n->subtree_count; } );
    }




    std::cout << "newwww" << std::endl;

    /*
     * calc fd intro vs inline cost
     */


    /*
     * create segnode functions here
     */

    /*
     * should have have created the functions before here
     */

    circ::decoder::ExpressionPrinter ep(std::cout);
//    using seg_ptr = std::shared_ptr<circ::SEGNode>;
//    auto seg_ptr_compare = []( seg_ptr &left, seg_ptr &right )
//    {
//        return left->id < right->id;
//    };

//    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration> func_decls;
    std::unordered_map<circ::SEGNode, circ::decoder::FunctionDeclaration, circ::segnode_hash_on_get_hash, circ::segnode_comp_on_hash> func_decls;
    circ::UniqueNameStorage name_storage;
    circ::decoder::Var stack("stack");
    std::vector<int> vals;

    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        std::cout << std::endl << "For vi: " << vi->id() << std::endl;
        auto it_pair = seg->get_nodes_by_vi( vi );
        int counter = 0;
        for(auto & [_, node ] : seg->get_nodes_by_vi(vi) ) {
            counter += node->subtree_count;
        }
        vals.push_back(counter);
//        int a = std::accumulate(it_pair.begin(), it_pair.end(), 0,  [](auto &left, auto &p) {
//                 return p.second.subtree_count;
//        });
    }


    std::cout << "sizes of vi's: ";
    std::for_each(vals.begin(), vals.end(), [&](int val) { std::cout << val << " ";});
        std::cout << std::endl;
    for(auto vi : circuit->attr<circ::VerifyInstruction>())
    {
        std::cout << std::endl << "For vi: " << vi->id() << std::endl;
        for(auto & [_, node ] : seg->get_nodes_by_vi(vi) )
        {
            auto it = node->roots.equal_range(vi);
            for(auto i = it.first  ; i != it.second; ++i)
            {
                auto op = i->second;

                circ::expr_for_node(func_decls, name_storage, *node, stack );
                std::cout << op->name() << "(" << node->inline_cost <<  (node->fd ? "*" : "") << "): " << node->print_hash() << std::endl;
//                ep.print(circ::decoder::Statement( circ::expr_for_node( , *node, circ::decoder::Var( "stack_var" ) ) ));


//                auto start_op = i->second;
//                auto start_op_ptr = std::make_shared< circ::nodeWrapper >( start_op );
//                auto op_gen = non_unique_dfs< gap::graph::yield_node::on_open >( start_op_ptr );
//                auto node_gen = non_unique_dfs< gap::graph::yield_node::on_open >( node );
//
//                std::cout << "combined: " << std::endl;
//
//                for ( auto &[ op, nod ] : tuple_generators( op_gen, node_gen ) )
//                {
//                    if(nod->fd)
//                        std::cout << "pre:existing: ";

//                }
//                std::cout << "end" << std::endl << std::endl;
            }

        }
    }

    std::cout << "size of func_decls " << func_decls.size() << std::endl << std::endl;

    // func decls orders based on hash buckets, need to traverse the graph to keep proper shape of the call graph
    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get())) {
        auto func = func_decls.find(*node);
        if(func != func_decls.end())
        {
            auto fd= *func;
//            std::cout << "DECLARED FUNC:: node: "  << fd.first.id << " name: " << fd.second.function_name << " hash: " << fd.first.get_hash() << std::endl;
            std::cout << "// called externally: " << fd.first.isRoot << " hash: " << fd.first.get_hash() << std::endl;
            ep.print(fd.second);
            std::cout << std::endl;
        }
    }


    /*
     * Emission is two phased, walk over the node structure and these will emit the functions into which we will call
     * second walk will be over the structure again, but paired with nodes as well for structure?
     * Not neccesarily for structure as they are isomorphic, but they need to for the structure of functions / func decls
     * Can be cata-fused but probably won't take save a lot as we walk over the structure in the size of operations, not nodes
     */




//    seg->get_nodes_by_vi()

//    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_close>(*seg.get()))
//    {
//        if(node->isRoot)
//        {
//            for(auto vi_paths : node->roots)
//            {
//                std::cout << "VI:  " << vi_paths.first->id() << std::endl;
//
//                auto start_op = vi_paths.second;
//                auto start_op_ptr = std::make_shared< circ::nodeWrapper >( start_op );
//                auto op_gen = non_unique_dfs< gap::graph::yield_node::on_open >( start_op_ptr );
//                auto node_gen = non_unique_dfs< gap::graph::yield_node::on_open >( node );
//
//                std::cout << "combined: " << std::endl;
//
//                for ( auto &[ op, nod ] : tuple_generators( op_gen, node_gen ) )
//                    std::cout << op->op->name() << "(" << nod->inline_cost
//                              << "): " << nod->print_hash() << std::endl;
//
//                std::cout << "end" << std::endl;
//            }
//        }
//    }

//    int counter = 0;
//    circ::decoder::ExpressionPrinter ep(std::cout);
//    for(auto& node : gap::graph::dfs<gap::graph::yield_node::on_open>(*seg.get()))
//    {
//
//        if(node->isRoot){
//            ep.print(circ::print_SEGNode_tree(*node.get(), "stackky", node->root_operations[0]));
//            counter++;
//            if(counter > 3)
//                break;
//        }
//    }


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
