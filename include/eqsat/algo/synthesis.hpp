/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/core/egraph.hpp>
#include <eqsat/pattern/pattern.hpp>

namespace eqsat {

    using graph::node_handle;

    template< gap::graph::graph_like egraph >
    struct synthesizer {

        auto synthesize(const expr_with_context &e) -> node_handle {
            return synthesize(e.expr);
        }

        auto synthesize_atom(const constant_t &constant) -> node_handle {
            return graph.insert(constant);
        }

        auto synthesize_operation(const operation_t &operation, std::span< node_handle > children)
            -> node_handle
        {
            return graph.insert(operation, children);
        }

        auto synthesize_atom(const operation_t &operation) -> node_handle {
            return synthesize_operation(operation, {});
        }


        auto synthesize_atom(const place_t &place) -> node_handle {
            return matched_places(match).find(
                std::uint32_t(place_index(place, places))
            )->second.handle();
        }

        auto synthesize_atom(const label_t &label) -> node_handle {
            return synthesize(get_expr_with_name(label, pattern).expr);
        }

        auto synthesize(const atom_t &atom) -> node_handle {
            const atom_base &base = atom;
            return std::visit([&] (const auto &a) { return synthesize_atom(a); }, base);
        }

        auto synthesize(const expr_list &list) -> node_handle {
            if (is_nested_list(list)) {
                throw std::runtime_error("nested expression in rewrite pattern");
            }

            auto front = list.front();

            if (list.size() > 1) {
                graph::children_t children;
                for (const auto &child : tail(list)) {
                    children.push_back(synthesize(child));
                }

                auto op = std::get< operation_t >(std::get< atom_t >(front));
                return synthesize_operation(op, children);
            } else {
                return synthesize(front);
            }
        }

        auto synthesize(const simple_expr &expr) -> node_handle {
            const simple_expr_base &base = expr;
            return std::visit([&] (const auto &e) { return synthesize(e); }, base);
        }

        synthesizer(
              const apply_pattern &pat
            , const places_t &plc
            , const match_result &m
            , egraph &g
        )
            : pattern(pat), places(plc), match(m), graph(g)
        {}

        const apply_pattern &pattern;
        const places_t &places;
        const match_result &match;
        egraph &graph;
    };

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const simple_expr &expr,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &match,
        egraph &graph
    ) -> node_handle {
        return synthesizer(pattern, places, match, graph).synthesize(expr);
    }

} // namespace eqsat
