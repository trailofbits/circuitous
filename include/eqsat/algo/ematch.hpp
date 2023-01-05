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
    // TODO move to util
    static inline auto tail(const auto &vec) {
        return std::span(&(*std::next(vec.begin())), &(*vec.end()));
    }

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

    template< gap::graph::graph_like egraph >
    struct matcher {
        using node_type = typename egraph::node_type;
        using eclass_type = typename egraph::eclass_type;

        //
        // match constant node
        //
        auto match(const constant_t &c, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            if (auto con = extract_constant(n)) {
                if (con.value() == c.ref()) {
                    spdlog::debug("[eqsat] matched constant {} with {}", c, con.value());
                    co_yield { graph.find(&n), matched };
                }
            }
        }

        //
        // match operation node
        //
        auto match(const operation_t &o, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            if (node_name(n) == o.ref()) {
                spdlog::debug("[eqsat] matched op {} with {}", o, node_name(n));
                co_yield { graph.find(&n), matched };
            }
        }

        //
        // match place node
        //
        auto match(const place_t &p, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            auto id = std::uint32_t(place_index(p, places));
            auto handle = graph.find(&n);
            if (auto it = matched.find(id); it != matched.end()) {
                if (it->second.handle() != handle) {
                    co_return;
                }
            }

            spdlog::debug("[eqsat] matched place {} with {}", p, node_name(n));

            single_match_result result = { handle, matched };
            result.matched_places.emplace(id, maybe_node_handle(handle));
            co_yield result;
        }

        //
        // match label node
        //
        auto match(const label_t &p, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            co_yield match(get_expr_with_name(p, pattern).expr, n, matched);
        }

        //
        // match atom node
        //
        auto match(const atom_t &a, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            co_yield std::visit([&] (const auto &atom) -> single_match_generator {
                co_yield match(atom, n, matched);
            }, a);
        }

        auto match_children(const auto &pattern_children, const auto &node_children, const matched_places_t &matched)
            -> single_match_generator
        {
            auto front = graph.eclass(node_children.front());
            auto front_match = match(pattern_children.front(), front, matched);

            if (pattern_children.size() == 1) {
                co_yield front_match;
            } else {
                for (auto m : front_match) {
                    co_yield match_children(tail(pattern_children), tail(node_children), matched_places(m));
                }
            }
        }

        //
        // match list piecewise
        //
        auto match(const expr_list &list, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            for (auto head : match(list.front(), n, matched)) {
                auto pattern_children = tail(list);

                if (pattern_children.empty()) {
                    co_yield head;
                } else {
                    if (pattern_children.size() != n.num_of_children()) {
                        co_return;
                    }

                    std::vector< graph::node_handle > node_children;
                    for (auto ch : n.children()) {
                        node_children.push_back(ch);
                    }

                    auto head_matched = matched_places(head);
                    for (auto m : match_children(pattern_children, node_children, head_matched)) {
                        co_yield { head.root, matched_places(m) };
                    }
                }
            }
        }

        //
        // match expr with context
        //
        auto match(const expr_with_context &e, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            // TODO deal with context matching
            co_yield match(e.expr, n, matched);
        }

        //
        // match simple expr node
        //
        auto match(const simple_expr &e, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            co_yield std::visit([&] (const auto &a) -> single_match_generator {
                co_yield match(a, n, matched);
            }, e);
        }

        auto match(const simple_expr &e, const eclass_type &eclass, const matched_places_t &matched)
            -> single_match_generator
        {
            spdlog::debug("[eqsat] matching simple expr {}", e);
            for (const auto &n : eclass.nodes) {
                co_yield match(e, *n, matched);
            }
        }

        //
        // try to match expr rooting in the given eclass
        //
        match_generator match(const simple_expr &expr, const eclass_type &eclass) {
            for (const auto &node : eclass.nodes) {
                matched_places_t matched;
                for (const auto &m : match(expr, *node, matched)) {
                    co_yield m;
                }
            }
        }

        //
        // generate matches of expr for whole egraph
        //
        match_generator match(const simple_expr &expr) {
            for (const auto &[_, eclass] : graph.eclasses()) {
                for (auto m : match(expr, eclass)) {
                    if (matched_places(m).size() == places.size()) {
                        spdlog::debug("[eqsat] matched {}", m);
                        co_yield m;
                    }
                }
            }
        }

        match_generator match(const match_expr &expr) {
            spdlog::error("not implemented simple match expr");
            __builtin_abort();
        }

        match_generator match() {
            co_yield std::visit([&] (const auto &a) -> match_generator {
                co_yield match(a);
            }, pattern.action);
        }

        matcher(const match_pattern &pattern, const egraph &graph)
            : pattern(pattern), graph(graph), places(gather_places(pattern))
        {}

        matcher(const rewrite_rule &rule, const egraph &graph)
            : matcher(rule.lhs, graph)
        {
            spdlog::debug("[eqsat] matching rule {}", rule);
        }

        const match_pattern &pattern;
        const egraph &graph;
        places_t places;
    };

    template< gap::graph::graph_like egraph >
    match_generator match(const rewrite_rule &rule, const egraph &graph) {
        co_yield matcher(rule, graph).match();
    }

} // namespace eqsat
