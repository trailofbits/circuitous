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

    static inline void unify_inplace(matched_places_t &dst, const matched_places_t &src) {
        for (const auto &[k, v] : src) {
            assert(!dst.count(k) || dst.at(k) == v);
            dst.emplace(k, v);
        }
    }

    struct single_match_result {
        graph::node_handle root;
        matched_places_t matched_places;
    };

    struct multi_match_result {
        std::unordered_map< label_t, graph::node_handle > roots;
        matched_places_t matched_places;

        void insert(const label_t &label, single_match_result &&m) {
            assert(!roots.count(label));
            roots.emplace(label, m.root);
            unify_inplace(matched_places, m.matched_places);
        }
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
        auto match(const label_t &lab, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            co_yield match(get_expr_with_name(lab, pattern).expr, n, matched);
        }

        //
        // match atom node
        //
        auto match(const atom_t &a, const node_type &n, const matched_places_t &matched)
            -> single_match_generator
        {
            const atom_base &base = a;
            co_yield std::visit([&] (const auto &atom) -> single_match_generator {
                co_yield match(atom, n, matched);
            }, base);
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
            const simple_expr_base &base = e;
            co_yield std::visit([&] (const auto &a) -> single_match_generator {
                co_yield match(a, n, matched);
            }, base);
        }

        auto match(const simple_expr &e, const eclass_type &eclass, const matched_places_t &matched)
            -> single_match_generator
        {
            for (const auto &n : eclass.nodes) {
                co_yield match(e, *n, matched);
            }
        }

        //
        // generate matches of expr for whole egraph
        //
        single_match_generator match(const simple_expr &expr, const matched_places_t &matched) {
            for (const auto &[_, eclass] : graph.eclasses()) {
                for (auto m : match(expr, eclass, matched)) {
                    if (matched_places(m).size() == places.size()) {
                        spdlog::debug("[eqsat] matched {}", m);
                        co_yield m;
                    }
                }
            }
        }

        single_match_generator match(const simple_expr &expr) {
            matched_places_t matched;
            co_yield match(expr, matched);
        }

        single_match_generator match(const expr_with_context &e, const matched_places_t &matched)
        {
            // TODO deal with context matching
            co_yield match(e.expr, matched);
        }

        //
        // labeled multi-match
        //
        single_match_generator match(const label_t &lab, const matched_places_t &matched)
        {
            co_yield match(get_expr_with_name(lab, pattern).expr, matched);
        }


        match_generator match(std::vector< label_t > labels, multi_match_result partial_result) {
            auto label = labels.back();
            labels.pop_back();

            for (auto m : match(label, partial_result.matched_places)) {
                multi_match_result result = partial_result;
                result.insert(label, std::move(m));

                if (labels.empty()) {
                    co_yield result;
                } else {
                    co_yield match(labels, result);
                }
            }
        }

        match_generator match(const basic_match_expr &expr) {
            multi_match_result result;
            co_yield match(expr.labels, result);
        }

        match_generator match(const commutative_match_expr &expr) {
            spdlog::error("not implemented basic_match_expr expr");
            __builtin_abort();
        }

        match_generator match(const match_expr &expr) {
            co_yield std::visit([&] (const auto &a) -> match_generator {
                co_yield match(a);
            }, expr);
        }

        match_generator match() {
            co_yield std::visit([&] (const auto &a) -> match_generator {
                for (auto &&m : match(a)) {
                    co_yield std::forward< decltype(m) >(m);
                }
            }, pattern.action);
        }

        matcher(const match_pattern &pattern, const egraph &graph)
            : pattern(pattern), graph(graph), places(gather_places(pattern))
        {}

        matcher(const rewrite_rule &rule, const egraph &graph)
            : matcher(rule.lhs, graph)
        {}

        const match_pattern &pattern;
        const egraph &graph;
        places_t places;
    };

    template< gap::graph::graph_like egraph >
    match_generator match(const rewrite_rule &rule, const egraph &graph) {
        spdlog::debug("[eqsat] match rule {}", rule);
        co_yield matcher(rule, graph).match();
    }

} // namespace eqsat
