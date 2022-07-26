// Copyright (c) 2022 Trail of Bits, Inc.

#include <circuitous/Diff/ConfigDiffer.hpp>
#include <circuitous/Diff/SemanticsTainter.hpp>
#include <string>

namespace circ::inspect::config_differ{

    void ConfigToTargetDifferPass::Execute(Operation *tree1, Operation* tree2) {
        /*
         * We first mark all paths that exist inside tree1 as left.
         * Then we mark all paths in tree2 as right
         * If during the walk for tree2 we see left we mark them as overlap
         */
        DiffTree( tree1, DiffMarker::Left, DiffMarker::Right );
        DiffTree( tree2, DiffMarker::Right, DiffMarker::Left );
    }

    void ConfigToTargetDifferPass::DiffTree(Operation* tree, const DiffMarker& key_this, const DiffMarker& key_other){
        CollectPathsUpwardTillConstraint constraint_collector;
        constraint_collector.Run( tree );

        for(auto& p : constraint_collector.paths_collect){
            for(auto o : p) {
                if(o->has_meta(key) && diffmarker_read(o) == key_other)
                    diffmarker_write(o, DiffMarker::Overlapping);
                else
                    diffmarker_write(o, key_this);
            }
        }
    }

    void CollectPathsUpwardTillConstraint::Execute(Operation *op) {
        if( semantics_tainter::read_semantics(op) == semantics_tainter::SemColoring::Config){
            std::cout << "reading configs, size current path: " << current_path.size() << std::endl;
            std::vector<Operation*> path_to_save;
            for(auto it = current_path.rbegin(); it != current_path.rend(); ++it){
                path_to_save.emplace(path_to_save.end(), *it); // TODO expensive call, change to deque?
                std::cout << "traversed " <<  (*it)->name() << std::endl;
                if(isa<constraint_opts_ts>(*it)){
                    paths_collect.push_back(path_to_save); //copy
                    std::cout << "pushed with size " <<  path_to_save.size() << std::endl;
                }
            }
        }
    }

    void CollectPathsUpwardTillConstraint::Run(Operation *op) {
        Execute( op );

        for (auto o: op->operands) {
            current_path.push_back(o);
            Run( o );
            current_path.pop_back();
        }
    }

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
}