#include <circuitous/Decoder/DecodeAST.hpp>
#include <string>
#include <circuitous/Diff/SemanticsTainter.hpp>


namespace circ::inspect::semantics_tainter {

    void SemanticsTainterPass::taint(circ::Operation *op){
        /*
         * instruction bits are by definition related to decoding
         * Advice are meant to represent dependancy inversion which are chosen by decoding
         *      Honestly not 100% whether it should always be a decode value
         *
         * Constants are decodes until they are hit by a different value
         */
        if ( isa< Constant >( op ) || isa< InputInstructionBits >( op ) ||
             isa< Advice >( op )) {
            write( op, SemColoring::Decode );
        } else if ( isa< RegConstraint >( op ) || isa< WriteConstraint >( op ) ||
                    isa< ReadConstraint >( op )) {
            /*
             * Constraints are always semantics as they never represent state directly
             * Nor are config nor are decode
             */
            write( op, SemColoring::Semantics );
        } else if ( isa< DecodeCondition >( op )) {
            /*
             * Decode by definition
             */
            write( op, SemColoring::Decode );
            for (auto &o: op->operands) {
                /*
                 * No matter what constants are in the rest of the system,
                 * the value instruction bits are compared against to see which instruction
                 * we are decoding should always be decode
                 */
                if ( isa< Constant >( o )) {
                    write( o, SemColoring::Decode );
                }
            }
        } else if ( isa< Memory >( op )) {
            /*
             * Memory is always an operand to an instruction
             */
            write( op, SemColoring::Config );
        } else if ((isa< leaf_values_ts >( op ) && !isa< InputInstructionBits >( op ))) {
            /*
             * This should get all registers and other machine state related nodes
             */
            write( op, SemColoring::State );
        } else { // non terminals
            if ( op->operands.size() == 1 ) { // single child, should just pass on
                write( op, read_semantics( op->operands[ 0 ] ));
            } else if ( should_promote_to_semantics( op )) {
                write( op, SemColoring::Semantics );
            } else if ( !all_children_are_same( op )) {
                write( op, SemColoring::Config );
            } else { // we know we are not a leaf node and all nodes are the same
                write( op, read_semantics( op->operands[ 0 ] ));
            }
        }
    }

    bool SemanticsTainterPass::should_promote_to_semantics(Operation *op) {
        std::vector<Operation*> promote_to_config;

        for (auto& o: op->operands){
            auto o_sem = read_semantics( o);
            if( o_sem == SemColoring::Config || o_sem == SemColoring::Semantics)
                continue;
            /*
             * any child should at least be config or higher, or an decode
             * if there is a decode, it must not be "just" decode
             * for instance an `add rax 2` has 2 as a constant, but add is clearly semantics
             * more-over, this `2` now is an operand to semantics and hence should be considered config
             *
             * The check for !all_children_are_same prevents some weird cases in which a node
             * with only decode constants are suddenly promoted to semantics
             * This shouldn't be an issue for now as there shouldn't be semantics that are
             * of the form OP imm, imm
             *
             *
             */
            if(o_sem == SemColoring::Decode && !all_children_are_same(op)){
                promote_to_config.push_back(o);
                continue;
            }
            return false;
        }

        for(auto o : promote_to_config){
            write(o, SemColoring::Config);
        }
        return true;
    }

    bool SemanticsTainterPass::all_children_are_same(Operation *op) {
        if(op->operands.size() < 2)
            return true;

        auto first = read_semantics(op);
        for (auto& o: op->operands){
            if(first != read_semantics(o)){
                return false;
            }
        }
        return true;
    }

    void SemanticsTainterPass::write(Operation *op, SemColoring value) {
        op->set_meta<true>(key, semantic_to_string(value));
    };

    std::string semantic_to_string(SemColoring sc) {
        switch (sc){
            case SemColoring::None: return "None";
            case SemColoring::Decode: return "Decode";
            case SemColoring::State: return "State";
            case SemColoring::Config: return "Config";
            case SemColoring::Semantics: return "Semantics";
            case SemColoring::Delete: return "Delete";
        }
    }

    SemColoring read_semantics(Operation *op) {
        if (!op->has_meta(key))
            return SemColoring::None;
        else if (op->get_meta(key) == "None")
            return SemColoring::None;
        else if (op->get_meta(key) == "Decode")
            return SemColoring::Decode;
        else if (op->get_meta(key) == "State")
            return SemColoring::State;
        else if (op->get_meta(key) == "Config")
            return SemColoring::Config;
        else if (op->get_meta(key) == "Semantics")
            return SemColoring::Semantics;
        else if (op->get_meta(key) == "Delete")
            return SemColoring::Delete;
        else
            circ::unreachable() << "could not decode semantics";
    }


}