/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <functional>
#include <fstream>
#include <ostream>
#include <string>
#include <unordered_map>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <stack>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/IR/Shapes.hpp>

namespace circ::inspect {

    enum class sem_taint {
        None,
        Decode,
        State,
        Config,
        Semantics,
        Delete
    };

    std::string semantic_to_string(sem_taint sc);
    sem_taint read_semantics(Operation* op);


    /*
     * This taint is meant to identify what kind of semantics certain nodes represent.
     * The goal is to identify what are "core" semantics and what are operands to semantics.
     *
     * Take an `and rax, 1` we want to identify all nodes relating to the semantics of `and`
     * and which nodes represent the operands `rax` and `1`. Once we do so, we can replace
     * the operands with advice nodes.
     *
     * If we take the view of how we "assemble" an instruction, and we already have semantics
     * then we need to have bits from the encoding specify which state needs to be altered.
     * The point at which this happens is called "config", because this configures which state
     * the semantics should be altered.
     *
     * The key observation is that if a set of semantics accept multiple operands,
     * then the choice of which operand is acted upon must be encoded in the instruction bits.
     * Therefore, the point onto which the operands are introduced to the semantics is at
     * the point at which a subtree related purely to instruction bits (decode) meets with
     * a subtree relating to the state which will be altered. (state)
     * The point at which decode and state meet is called "config" because this configures
     * which operands/targets the semantics that will be executed.
     *
     *
     * Note that certain node types can have different meanings dependent on where they are located
     * For instance a constant can represent the value of the encoding bits.
     * But also the value `1` which eax will be incremented by for the instruction INC.
     *
     * The graph taints node in 1 of 4 different options: Semantics, Config, Decode and State
     * If a node has children only of the same type, then that type gets propagated.
     *
     * State represents machine state like registers.
     * Decode represents nodes relating to the decoding process.
     * Config represent those nodes that have a Decode and State child.
     * Semantics represent semantics or config nodes.
     *
     * The advantage of doing this on the graph compared to llvm is that any semantics which
     * might be implicit to an instruction are captured as well through this approach.
     */
    struct SemanticsTainterVisitor : Visitor< SemanticsTainterVisitor >
    {
        void write(Operation* op, sem_taint value);
        bool all_children_are_same(Operation* op);
        bool should_promote_to_semantics(Operation* op);
        void visit(Operation* op);
        void taint(Operation* op);
        static const inline std::string meta_key = "diff";
    };

    struct SemanticsTainterRemovalVisitor : Visitor< SemanticsTainterRemovalVisitor >
    {
        void visit(Operation* op);
    };

}  // namespace circ
