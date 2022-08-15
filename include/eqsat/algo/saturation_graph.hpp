/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    struct saturable_egraph : egraph {

        explicit saturable_egraph(egraph &&graph)
            : egraph(std::forward< egraph >(graph))
        {}

      private:
        // modified eclasses that needs to be rebuild
        std::vector< node_id_t > _pending;
    };

} // namespace eqsat
