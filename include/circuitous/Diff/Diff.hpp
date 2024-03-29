/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <functional>
#include <fstream>
#include <ostream>
#include <string>
#include <unordered_map>
#include <stack>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/IR/Shapes.hpp>
#include "SemanticsTainter.hpp"

/*
 * We want to get all paths that go from a constraint (target) to a config node within a VI
 * The fact that we want to only get paths belonging to one VI makes that we can't use a
 * bottom up approach, as recurring upwards requires that we check the parent also belongs
 * to the VI we are interested in.
 * We can exit early, making us check at every node (grows with #nodes, expensive), or if the path we found
 * traces back to the desired VI (grows with #paths, also expensive)
 *
 * I didn't want to do this on a copy of a VI either.
 *
 * We have the following approach
 * Keep track of all nodes visteded in DFS
 * once we hit a config/end node do:
 *     build path from config node to start node
 *     if last added node is constraint/print node
 *          print/save this path
 */

namespace circ::inspect {
    static const inline std::string meta_key = "diff_ctt";
    enum class DiffMarker
    {
        None,
        Left,
        Right,
        Overlapping
    };

    void diffmarker_write(Operation* op, DiffMarker dm);
    DiffMarker diffmarker_read(Operation* op);

    template <typename T>
    concept SubPathCol = std::derived_from<T, SubPathCollector<T>>;

    /*
     * Finds all paths starting from a constraint down to a config node
     */
    struct ConfigToTargetSubPathCollector : SubPathCollector<ConfigToTargetSubPathCollector>{
        bool top(Operation* op) { return isa<constraint_opts_ts>(op); }
        bool bottom(Operation* op) { return read_semantics(op) == sem_taint::Config; }
    };

    struct LeafToTargetSubPathCollector : SubPathCollector<LeafToTargetSubPathCollector>{
        bool top(Operation* op) { return isa<constraint_opts_ts>(op); }
        bool bottom(Operation* op) { return isa<leaf_values_ts>(op); }
    };

    struct LeafToVISubPathCollector : SubPathCollector<LeafToVISubPathCollector>{
        bool top(Operation* op) { return isa<VerifyInstruction>(op); }
        bool bottom(Operation* op) { return isa<leaf_values_ts>(op);}
    };

    struct InstrBitsToDRSubPathCollector : SubPathCollector<InstrBitsToDRSubPathCollector>{
        bool top(Operation* op) { return isa<DecoderResult>(op); }
        bool bottom(Operation* op) { return isa<leaf_values_ts>(op); }
    };

    void mark_operation(Operation* o, const DiffMarker& key_this, const DiffMarker& key_other);
    void clear_mark(Operation* op);

    template < typename F>
    void apply_on_subtree(const std::vector<std::vector<Operation*>> &subtrees, F&& f)
    {
        for ( auto &p : subtrees )
            for ( auto &o : p )
                f( o );
    }
};
