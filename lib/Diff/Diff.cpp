// Copyright (c) 2022 Trail of Bits, Inc.

#include <circuitous/Diff/Diff.hpp>
#include <circuitous/Diff/SemanticsTainter.hpp>
#include <string>

namespace circ::inspect
{
    void diffmarker_write(Operation *op, DiffMarker dm) {
        auto value = [&](){
            switch(dm){
                case DiffMarker::None : return "none";
                case DiffMarker::Left : return "left";
                case DiffMarker::Right : return "right";
                case DiffMarker::Overlapping : return "overlapping";
            }
        }();

        op->set_meta<true>( meta_key, value);
    }

    DiffMarker diffmarker_read(Operation *op) {
        if(!op->has_meta( meta_key))
            return DiffMarker::None;
        else if( op->get_meta( meta_key) == "left")
            return DiffMarker::Left;
        else if( op->get_meta( meta_key) == "right")
            return DiffMarker::Right;
        else if( op->get_meta( meta_key) == "overlapping")
            return DiffMarker::Overlapping;
        else
            circ::unreachable() << "could not decode DiffMarker";
    }

    void mark_operation(Operation *o, const DiffMarker &key_this, const DiffMarker &key_other) {
        if( o->has_meta( meta_key) && diffmarker_read( o) == key_other)
            diffmarker_write(o, DiffMarker::Overlapping);
            // if something is already marked overlap don't remove it
        else if(diffmarker_read(o) != DiffMarker::Overlapping)
            diffmarker_write(o, key_this);
    }



//    bool CTTFinder::bottom(Operation *op)
//
//    bool InstrBitsToDRFinder::top(Operation *op) {
//        return isa<DecoderResult>(op);
//    }
//
//    bool InstrBitsToDRFinder::bottom(Operation *op) {
//        return isa<leaf_values_ts>(op);
//    }
//
//    bool LTTFinder::top(Operation *op) {
//        return isa<constraint_opts_ts>(op);
//    }
//
//    bool LTTFinder::bottom(Operation *op) {
//        return isa<leaf_values_ts>(op);
//    }
//
//    bool LeafToVISubPathCollector::top(Operation *op) {
//        return isa<VerifyInstruction>(op);
//    }
//
//    bool LeafToVISubPathCollector::bottom(Operation *op) {
//        return isa<leaf_values_ts>(op);
//    }
}
