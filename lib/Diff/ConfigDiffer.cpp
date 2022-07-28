// Copyright (c) 2022 Trail of Bits, Inc.

#include <circuitous/Diff/ConfigDiffer.hpp>
#include <circuitous/Diff/SemanticsTainter.hpp>
#include <string>

namespace circ::inspect::config_differ{

//    void ConfigToTargetDifferPass::Execute(Operation *tree1, Operation* tree2) {
//        /*
//         * We first mark all paths that exist inside tree1 as left.
//         * Then we mark all paths in tree2 as right
//         * If during the walk for tree2 we see left we mark them as overlap
//         */
//        DiffTree( tree1, DiffMarker::Left, DiffMarker::Right );
//        DiffTree( tree2, DiffMarker::Right, DiffMarker::Left );
//    }
//
//    void ConfigToTargetDifferPass::DiffTree(Operation* tree, const DiffMarker& key_this, const DiffMarker& key_other){
//        CTTFinder ctt_collector;
//        ctt_collector.visit( tree );
//
//        for(auto& p : ctt_collector.paths_collect){
//            for(auto o : p) {
//                if(o->has_meta(key) && diffmarker_read(o) == key_other)
//                    diffmarker_write(o, DiffMarker::Overlapping);
//                // if something is already marked overlap don't remove it
//                else if(diffmarker_read(o) != DiffMarker::Overlapping)
//                    diffmarker_write(o, key_this);
//            }
//        }
//    }

    void diffmarker_write(Operation *op, DiffMarker dm) {
        auto value = [&](){
            switch(dm){
                case DiffMarker::None : return "none";
                case DiffMarker::Left : return "left";
                case DiffMarker::Right : return "right";
                case DiffMarker::Overlapping : return "overlapping";
            }
        }();

        op->set_meta<true>(key, value);
    }

    DiffMarker diffmarker_read(Operation *op) {
        if(!op->has_meta(key))
            return DiffMarker::None;
        else if(op->get_meta(key) == "left")
            return DiffMarker::Left;
        else if(op->get_meta(key) == "right")
            return DiffMarker::Right;
        else if(op->get_meta(key) == "overlapping")
            return DiffMarker::Overlapping;
        else
            circ::unreachable() << "could not decode DiffMarker";
    }

    void mark_operation(Operation *o, const DiffMarker &key_this, const DiffMarker &key_other) {
        if(o->has_meta(key) && diffmarker_read(o) == key_other)
            diffmarker_write(o, DiffMarker::Overlapping);
            // if something is already marked overlap don't remove it
        else if(diffmarker_read(o) != DiffMarker::Overlapping)
            diffmarker_write(o, key_this);
    }

    bool CTTFinder::top(Operation *op) {
        return isa<constraint_opts_ts>(op);
    }

    bool CTTFinder::bottom(Operation *op) {
        return semantics_tainter::read_semantics(op) == semantics_tainter::SemColoring::Config;
    }

    bool InstrBitsToDR::top(Operation *op) {
        return isa<DecoderResult>(op);
    }

    bool InstrBitsToDR::bottom(Operation *op) {
        return isa<leaf_values_ts>(op);
    }
}
