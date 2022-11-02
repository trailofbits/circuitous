/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/egraph.hpp>
#include <eqsat/pattern/rewrite_rule.hpp>

namespace eqsat {

    using graph::node_handle;

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

} // namespace eqsat
