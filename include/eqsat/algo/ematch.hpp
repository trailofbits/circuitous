/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/core/egraph.hpp>

#include <gap/core/generator.hpp>
#include <gap/core/dense_map.hpp>
#include <gap/core/overloads.hpp>

#include <spdlog/spdlog.h>
#include <fmt/ranges.h>
#include <iostream>
#include <span>

namespace eqsat
{
    struct maybe_node_handle {
        explicit maybe_node_handle() : _handle(0) {}
        explicit maybe_node_handle(graph::node_handle handle)
            : _handle(handle.id.ref() + 1)
        {}

        graph::node_handle handle() const { return graph::node_handle(id()); }
        node_id_t id() const { return node_id_t( _handle - 1 ); }

        constexpr auto operator<=>(const maybe_node_handle& other) const = default;

    private:
        node_id_t::underlying_t _handle;
    };


    //
    // match result
    //
    using matched_places_t = gap::dense_map< std::uint32_t, maybe_node_handle >;

    struct single_match_result {
        graph::node_handle root;
        matched_places_t matched_places;
    };

    struct multi_match_result {
        std::unordered_map< label_t, graph::node_handle > roots;
        matched_places_t matched_places;
    };

    using match_result = std::variant< single_match_result, multi_match_result >;

    static inline matched_places_t matched_places(const match_result &result) {
        return std::visit([] (const auto &m) { return m.matched_places; }, result);
    }

    template< typename stream >
    stream& operator<<(stream& os, const matched_places_t& places) {
        for (auto p : places) {
            os << fmt::format(" {} -> {}", p.first, p.second.id().ref());
        }
        return os;
    }

    template< typename stream >
    stream& operator<<(stream& os, const single_match_result& m) {
        os << "match: " << m.root.id << '\n';
        return os << '\n' << m.matched_places;
    }

    template< typename stream >
    stream& operator<<(stream& os, const multi_match_result& m) {
        os << "match:\n";
        for (const auto &[lab, root] : m.roots) {
            os << lab << " -> " << root.id.ref() << '\n';
        }
        return os << '\n' << m.matched_places;
    }

    template< typename stream >
    stream& operator<<(stream& os, const match_result& _m) {
        return std::visit([&] (const auto &m) -> stream& { return os << m; }, _m);
    }

    using match_generator = gap::recursive_generator< match_result >;

    using single_match_generator = gap::recursive_generator< single_match_result >;

} // namespace eqsat

namespace eqsat {
    //
    // match constant node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const constant_t &c
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        if (auto con = extract_constant(node)) {
            if (con.value() == c.ref()) {
                spdlog::debug("[eqsat] matched constant {} with {}", c, con.value());
                co_yield { graph.find(&node), matched };
            }
        }
    }

    //
    // match operation node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const operation_t &o
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        if (node_name(node) == o.ref()) {
            spdlog::debug("[eqsat] matched op {} with {}", o, node_name(node));
            co_yield { graph.find(&node), matched };
        }
    }

    //
    // match place node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const place_t &p
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        auto id = std::uint32_t(place_index(p, places));
        auto handle = graph.find(&node);
        if (auto it = matched.find(id); it != matched.end()) {
            if (it->second.handle() != handle) {
                co_return;
            }
        }

        spdlog::debug("[eqsat] matched place {} with {}", p, node_name(node));

        single_match_result result = { handle, matched };
        result.matched_places.emplace(id, maybe_node_handle(handle));
        co_yield result;
    }

    //
    // match label node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const label_t &p
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        co_yield match(
            get_expr_with_name(p, pattern).expr, node, pattern, graph, places, matched
        );
    }

    //
    // match atom node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const atom_t &atom
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        spdlog::debug("[eqsat] matching atom {} : {}", atom, atom.bitwidth().value_or(0));
        co_yield std::visit([&] (const auto &a) -> single_match_generator {
            co_yield match(a, node, pattern, graph, places, matched);
        }, atom);
    }

    static inline auto tail(const auto &vec) {
        return std::span(&(*std::next(vec.begin())), &(*vec.end()));
    }

    template< gap::graph::graph_like egraph >
    single_match_generator match_children(
          const auto &pattern_children
        , const auto &node_children
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        using eclass_type = typename egraph::eclass_type;

        auto match_child = [&] () -> single_match_generator {
            spdlog::debug("[eqsat] matching child {}", pattern_children.front());
            eclass_type child_class = graph.eclass(node_children.front());
            co_yield match(pattern_children.front(), child_class, pattern, graph, places, matched);
        };

        if (pattern_children.size() == 1) {
            co_yield match_child();
        } else {
            for (auto m : match_child()) {
                co_yield match_children(
                    tail(pattern_children), tail(node_children), pattern, graph, places, matched_places(m)
                );
            }
        }
    }

    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const expr_list &list
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        for (auto head : match(list.front(), node, pattern, graph, places, matched)) {
            auto pattern_children = tail(list);

            if (pattern_children.empty()) {
                co_yield head;
            } else {
                if (pattern_children.size() != node.num_of_children()) {
                    co_return;
                }

                std::vector< graph::node_handle > node_children;
                for (auto &&ch : node.children()) {
                    node_children.push_back(std::move(ch));
                }

                for (auto m : match_children(pattern_children, node_children, pattern, graph, places, matched_places(head))) {
                    co_yield { head.root, matched_places(m) };
                }
            }
        }
    }

    //
    // match expr with context
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const expr_with_context &expr
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        // TODO deal with context matching
        co_yield match(expr.expr, node, pattern, graph, places, matched);
    }

    //
    // match simple expr node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const simple_expr &expr
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        co_yield std::visit([&] (const auto &a) -> single_match_generator {
            co_yield match(a, node, pattern, graph, places, matched);
        }, expr);
    }

    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const simple_expr &expr
        , const typename egraph::eclass_type &eclass
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        spdlog::debug("[eqsat] matching simple expr {}", expr);
        for (const auto &node : eclass.nodes) {
            co_yield match(expr, *node, pattern, graph, places, matched);
        }
    }

    //
    // match match expr node
    //
    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const basic_match_expr &expr
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        // TODO: assert that labels differ
        // for (const auto &label : labels(expr)) {
        //     auto partial = match(get_expr_with_name(label, pattern), node, pattern, graph, places, matched);
        // }

        // 1. accumulate matches along all labels, forward matched places
        // match(get_expr_with_name(label, pattern), pattern, graph, places, matched)
        spdlog::error("not implemented simple match expr");
        __builtin_abort();
    }

    template< gap::graph::graph_like egraph >
    single_match_generator match(
          const commutative_match_expr &expr
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        // TODO: assert that labels differ
        spdlog::error("not implemented commutative match expr");
        __builtin_abort();
    }

    template< gap::graph::graph_like egraph >
    match_generator match(
          const match_expr &expr
        , const typename egraph::node_type &node
        , const match_pattern &pattern
        , const egraph &graph
        , const places_t &places
        , const matched_places_t &matched
    ) {
        // TODO: assert that labels differ
        co_yield std::visit([&] (const auto &e) -> match_generator {
            for (const auto &m : match(e, node, pattern, graph, places, matched) ) {
                co_yield m;
            }
        }, expr);
    }

    template< gap::graph::graph_like egraph >
    match_generator match(
          const match_pattern &pattern
        , const typename egraph::eclass_type &eclass
        , const egraph &graph
        , const places_t &places
    ) {
        auto match_by_action = [&] (const auto &node) -> match_generator {
            co_yield std::visit([&] (const auto &a) -> match_generator {
                matched_places_t matched;
                for (auto m : match(a, node, pattern, graph, places, matched) ) {
                    co_yield m;
                }
            }, pattern.action);
        };

        auto size = eclass.nodes.size();
        for (size_t idx = 0; idx < size; ++idx) {
            co_yield match_by_action(*eclass.nodes[idx]);
        }
    }

    template< gap::graph::graph_like egraph >
    match_generator match(const match_pattern &pattern, const egraph &graph) {
        auto places = gather_places(pattern);
        for (const auto &[_, eclass] : graph.eclasses()) {
            for (auto m : match(pattern, eclass, graph, places)) {
                if (matched_places(m).size() == places.size()) {
                    spdlog::debug("[eqsat] matched {}", m);
                    co_yield m;
                }
            }
        }
    }

    template< gap::graph::graph_like egraph >
    match_generator match(const rewrite_rule &rule, const egraph &graph) {
        spdlog::debug("[eqsat] matching rule {}", rule);
        co_yield match(rule.lhs, graph);
    }

} // namespace eqsat
