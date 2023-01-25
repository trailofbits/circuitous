/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/algo/ematch.hpp>
#include <eqsat/algo/apply.hpp>

#include <eqsat/core/egraph.hpp>
#include <eqsat/core/cost_graph.hpp>

#include <eqsat/pattern/rule_set.hpp>
#include <eqsat/pattern/rewrite_rule.hpp>


namespace eqsat
{
    using graph::node_handle;

    namespace action {

        struct rebuild {};

        struct match_and_apply {
            rewrite_rule rule;
        };

    } // namespace action

    template< gap::graph::graph_like egraph >
    struct saturable_egraph : egraph {

        using base = egraph;

        using handle_hash = typename base::handle_hash;

        explicit saturable_egraph(egraph &&graph)
            : egraph(std::forward< egraph >(graph))
        {}

        node_handle merge(node_handle lhs, node_handle rhs) {
            // TODO perform atomically
            auto lid = find(lhs.id);
            auto rid = find(rhs.id);

            if (lid == rid) {
                return node_handle(lid);
            }

            if (rank(lid) < rank(rid)) {
                std::swap(lid, rid);
            }

            // TODO maybe can be moved to rebuild?
            merge_eclasses(node_handle(lid), node_handle(rid));

            return { merge(lid, rid) };
        }

        using base::find;

        // Restores the egraph invariants, i.e, congruence equality and enode uniqueness
        void rebuild() {
            for (auto eclass : _pending) {
                this->canonicalize(eclass);
            }

            std::unordered_set< node_handle, handle_hash > changed_classes;
            for (auto eclass : _pending) {
                changed_classes.insert(find(eclass));
            }

            for (auto eclass : changed_classes) {
                this->repair(eclass);
            }

            _pending.clear();
        }

        void match_and_apply(const rewrite_rule &rule) {
            auto &graph = *this;

            std::vector< match_result > results;
            for (const auto &m : match(rule, graph)) {
                results.push_back(m);
            }

            for (const auto &m : results) {
                apply(rule, m, graph);
            }
        }

        auto apply_action( action::rebuild ) && {
            rebuild();
            return std::move( *this );
        }

        auto apply_action( action::match_and_apply act ) && {
            match_and_apply(act.rule);
            return std::move( *this );
        }

      private:

        void merge_eclasses(node_handle lhs, node_handle rhs) {
            auto eclass = this->_classes.extract(rhs).mapped();
            this->_classes[lhs].merge(std::move(eclass));
        }

        node_id_t find(node_id_t id) { return this->_unions.find(id); }
        node_id_t find(node_id_t id) const { return this->_unions.find(id); }

        gap::rank_type rank(node_id_t id) { return this->_unions.rank(id); }
        gap::rank_type rank(node_id_t id) const { return this->_unions.rank(id); }

        node_handle merge(node_id_t lhs, node_id_t rhs) {
            return _pending.emplace_back(node_handle{this->_unions.merge(lhs, rhs)});
        }

        // modified eclasses that needs to be rebuild
        std::vector< node_handle > _pending;
    };

    // return value of equality saturation
    enum class stop_reason
    {
        saturated, iteration_limit, node_limit, time_limit, unknown, none
    };

    std::string to_string(stop_reason reason);

    template< gap::graph::graph_like egraph >
    using saturation_result = std::pair< saturable_egraph< egraph >, stop_reason >;

    //
    // step of equality saturation
    //

    template< gap::graph::graph_like egraph >
    saturation_result< egraph > make_step(
        saturable_egraph< egraph > &&graph,
        std::span< rule_set > sets
    ) {
        spdlog::debug("[eqsat] saturation step");
        // TODO: paralelize

        for (const auto &set : sets) {
            spdlog::debug("[eqsat] applying sreule set {}", set.name);
            for (const auto &rule : set.rules) {
                graph = std::move(graph) | action::match_and_apply{ rule };
            }
        }

        return { std::move(graph), stop_reason::unknown };
    }

    //
    // generic saturation algorithm
    //
    template< gap::graph::graph_like egraph >
    saturation_result< egraph > saturate(
        saturable_egraph< egraph > &&graph,
        std::span< rule_set > rules
    ) {
        spdlog::debug("[eqsat] saturate start");
        // egraph.rebuild()

        stop_reason status = stop_reason::none;
        while (status == stop_reason::none) {
            auto [g, s] = make_step(std::move(graph), rules);
            graph = std::move(g);
            status = s;
        }

        spdlog::debug("[eqsat] saturate stop {}", to_string(status));
        return { std::move(graph), status };
    }

    template< gap::graph::graph_like egraph >
    saturation_result< egraph > saturate(egraph &&graph, std::span< rule_set > rules) {
        return saturate(saturable_egraph(std::forward< egraph >(graph)), rules);
    }

    template< gap::graph::graph_like egraph, typename action >
    constexpr auto operator|(egraph &&graph, action &&act) {
        return std::forward< egraph >(graph).apply_action(std::forward< action >(act));
    }
} // namespace eqsat
