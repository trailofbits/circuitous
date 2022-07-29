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

namespace circ::inspect::config_differ {
    static const inline std::string key = "diff_ctt";
    enum class DiffMarker{
        None,
        Left,
        Right,
        Overlapping
    };

    void diffmarker_write(Operation* op, DiffMarker dm);
    DiffMarker diffmarker_read(Operation* op);

    /*
     * Finds all paths starting from a constraint down to a config node
     */
    struct CTTFinder : SubPathCollector<CTTFinder>{
        bool top(Operation* op);
        bool bottom(Operation* op);
    };

    struct LTTFinder : SubPathCollector<LTTFinder>{
        bool top(Operation* op);
        bool bottom(Operation* op);
    };

    struct LeafToVISubPathCollector : SubPathCollector<LeafToVISubPathCollector>{
        bool top(Operation* op);
        bool bottom(Operation* op);
    };

    struct InstrBitsToDRFinder : SubPathCollector<InstrBitsToDRFinder>{
        bool top(Operation* op);
        bool bottom(Operation* op);
    };


    void mark_operation(Operation* o, const DiffMarker& key_this, const DiffMarker& key_other);

    template<typename T>
    void diff_subtrees(Operation* tree1, Operation* tree2, T&& path_collector){
        path_collector.visit(tree1);
        auto collected = path_collector.paths_collect;
        for(auto& p : path_collector(tree1)){
            for(auto& o : p)
                mark_operation(o, DiffMarker::Left, DiffMarker::Right);
        }

        for(auto& p : path_collector(tree2)){
            for(auto& o : p)
                mark_operation(o, DiffMarker::Right, DiffMarker::Left);
        }
    }
};
