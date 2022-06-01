/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <circuitous/Transforms/Pattern.hpp>

#include <circuitous/Support/CLIArgs.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Util/CmdParser.hpp>
#include <circuitous/Util/Overloads.hpp>
#include <fstream>
#include <map>
#include <string>

namespace cli {

using namespace circ::cli;
struct PathArg : Arity<1>, As<std::string> {};

struct Input : circ::DefaultCmdOpt, PathArg {
    const static inline auto opt = circ::CmdOpt("-i", false);
};

struct Output : circ::DefaultCmdOpt, PathArg {
    const static inline auto opt = circ::CmdOpt("-o", false);
};

}  // namespace cli

using input_options = circ::tl::TL<cli::Input>;

using output_options = circ::tl::TL<cli::Output>;

using common_options = circ::tl::TL<cli::Help, cli::Version>;

using options = circ::tl::merge<input_options, output_options, common_options>;

template <typename OptsList>
auto parse_and_validate_cli(int argc, char *argv[])
    -> std::optional<circ::ParsedCmd> {
    using namespace circ;

    static const auto yield_err = [&](const auto &msg) {
        std::cerr << msg << std::endl;
    };

    auto parsed = circ::CmdParser<OptsList>::parse_argv(argc, argv);
    if (!parsed) {
        std::cerr << "Command line arguments were not parsed correctly, see "
                  << "stderr for more details.";
        return {};
    }

    auto v = Validator(parsed);

    if (v.check(is_singleton<circ::cli::Help>())
            .check(is_singleton<circ::cli::Version>())
            .process_errors(yield_err)) {
        return {};
    }

    if (v.validate_leaves(OptsList{}).process_errors(yield_err))
        return {};

    return parsed;
}

std::optional<std::ifstream> get_input_stream(auto &cli) {
    if (auto in = cli.template get<cli::Input>()) {
        return std::ifstream(*in);
    }

    std::cerr << "Missing input path\n";
    std::exit(1);
}

std::optional<std::ofstream> get_output_stream(auto &cli) {
    if (auto out = cli.template get<cli::Output>()) {
        return std::ofstream(*out);
    }

    std::cerr << "Missing output path\n";
    std::exit(1);
}

/* EGraph */


namespace circ::eqsat {

using StringNode = ENode<std::string>;

std::string full_name(const StringNode *node) {
    return std::visit(
        overloaded{[](const BondNode &n) { return n.name(); },
                   [](const StorageNode<std::string> &n) { return n.get(); }},
        node->get());
}

std::string name(const StringNode *node) {
    auto strip_bitwidth = [](const auto &name) {
        return name.substr(0, name.find(":"));
    };

    return strip_bitwidth(full_name(node));
}

std::optional<uint32_t> bitwidth(const StringNode *node) {
    const auto &name = full_name(node);

    if (auto from = name.find(":"); from != std::string::npos) {
        return std::stoi(name.substr(from + 1));
    }

    return std::nullopt;
}

bool is_context_node(const StringNode *node) {
    auto n = name(node);
    return std::string_view(n).starts_with("CTX");
}

std::optional<std::int64_t> extract_constant(const StringNode *node) {
    auto is_number = [](std::string_view s) {
        auto isdigit = [](auto c) { return std::isdigit(c); };
        return !s.empty() && std::all_of(s.begin(), s.end(), isdigit);
    };

    if (is_number(node->data()))
        return std::stoll(node->data());

    return std::nullopt;
}

struct StringEGraph : EGraph<StringNode> {
    using Base = EGraph<StringNode>;
    using ENode = Base::Node;

    auto make_leaf(std::string_view atom) {
        auto candidate = ENode(std::string(atom));
        auto con = extract_constant(&candidate);

        if (con)
            if (auto it = constants.find(*con); it != constants.end())
                return find(it->second);

        auto [id, node] = add(std::move(candidate));

        if (con)
            constants.emplace(*con, node);

        return id;
    }

    auto make_node(std::string_view atom, std::vector<Id> children) {
        ENode node((std::string(atom)));
        node.children() = std::move(children);
        auto [id, _] = add(std::move(node));
        return id;
    }

    StringNode singleton(Id id) {
        const auto &eclass = this->eclass(id);
        return eclass.nodes.at(0)->data();
    }

    void dump(const std::string &file) {
        std::ofstream out(file);
        to_dot(*this, out);
    }

    std::map<std::int64_t, StringNode *> constants;
};

using TestENode = StringEGraph::Node;

struct StringEGraphBuilder {
    using constant = eqsat::constant;
    using place = eqsat::place;
    using label = eqsat::label;
    using operation = eqsat::operation;

    StringEGraphBuilder(StringEGraph *graph) : _graph(graph) {}

    Id constrain(const Id node, const Id advice) const {
        return _graph->make_node("constraint", {node, advice});
    }

    Id synthesize(const eqsat::expr &e, const auto &subs, const auto &places,
                  const auto &subexprs) const {
        CHECK(subs.size() == places.size());
        auto synth = [&](const auto &sub) {
            return synthesize(sub, subs, places, subexprs);
        };

        std::vector<Id> args;
        for (const auto &child : children(e)) {
            args.push_back(synth(child));
        }

        auto node = std::visit(
            overloaded{
                [&](const constant &con) -> Id {
                    return _graph->make_leaf(std::to_string(con.ref()));
                },
                [&](const place &plc) -> Id { return subs.id(places.at(plc)); },
                [&](const operation &op) -> Id {
                    return _graph->make_node(op.name, args);
                },
                [&](const label &lab) -> Id {
                    return synth(subexprs.at(label_name(lab)));
                },
                [&](const auto &) -> Id {
                    unreachable() << "unsupported node";
                },
            },
            root(e));

        return node;
    }

    StringEGraph *_graph;
};

}  // namespace circ::eqsat


using Rule = circ::eqsat::Rule< circ::eqsat::StringEGraph >;

int main(int argc, char *argv[]) {
    auto maybe_parsed_cli = parse_and_validate_cli<options>(argc, argv);
    if (!maybe_parsed_cli) {
        std::cerr << circ::help_str(options());
        return 1;
    }

    auto cli = std::move(*maybe_parsed_cli);

    if (cli.template present<cli::Help>()) {
        std::cout << circ::help_str(options());
        return 0;
    }

    if (cli.template present<cli::Version>()) {
        std::cerr << "TODO: Implement proper version message";
        return 1;
    }

    auto in = get_input_stream(cli).value();

    auto out = get_output_stream(cli).value();
}
