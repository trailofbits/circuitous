#pragma once

#include <circuitous/IR/Circuit.hpp>

namespace circ::decoder
{
    struct SpecializationManager
    {
        void extract_spec(Circuit* c);
        Expr emit_structures();

        std::vector<Enum> enums;
        /*
         * so we would have these enums,
         * we would emit them
         * in the graph all targets which we would specialize beforehand
         *
         * biggest question though visitor types
         *
         *
         * for each unique type in graph add one
         *      what if I don't want them?
         *
         * struct dummy_visitor{
         *      visit(Add *)
         *      visit(Concat *)
         *      visit(E....   *)
         *      ...
         * }
         *
         * loop over kind?
         *  emit structs definition
         *  Add(const_2, reg); // takes an arbtirary OP ????
         *  except for blacklist or get propper TL?
         *
         *
         * is it circIR kind cons and sizes opt?
         *
         * Registers needs to be converted to functions anyway so these can be enums thus
         * sem_add(RegisterEnum reg_out, Register reg_in)
         *  Assign(convert_to_reg(64, reg),  Add(64, const_2, reg_in))
         *
         *
         *  If we convert to enum lose type info though -> conversion functions
         *
         *  It is the subset of circIR used under VI
         *      each operation is generic struct with operator for (size, <L.operands>)
         *      functions for each VI, taking a generic visitor and possibily specializations
         *          Trees are created from this subset
         *          Each target in VI calls visitor with their own tree
         */
    }
}


