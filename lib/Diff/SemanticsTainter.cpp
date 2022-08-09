#include <circuitous/Decoder/DecodeAST.hpp>
#include <string>
#include <circuitous/Diff/SemanticsTainter.hpp>


namespace circ::inspect {

    void SemanticsTainterVisitor::taint(circ::Operation *op){
        /*
        * Instruction bits are by definition related to decoding.
        * Advice are meant to represent dependency inversion which are chosen by decoding
        *      Honestly not 100% whether it should always be a decode value
        *
        * Constants are decodes until they are hit by a different value
        */

        if ( is_one_of<Constant, InputInstructionBits>(op)) {
            return write( op, sem_taint::Decode );
        }
        if ( is_one_of<RegConstraint, WriteConstraint, ReadConstraint>(op)) {
            /*
             * Constraints are always semantics as they never represent state directly
             * Nor are config nor are decode. Note that this doesn't hold for advice constraints
             */
            return write( op, sem_taint::Semantics );
        }
        if ( isa< DecodeCondition >( op )) {
            /*
             * Decode by definition
             */
            write( op, sem_taint::Decode );
            for (auto &o: op->operands()) {
                /*
                 * No matter what constants are in the rest of the system,
                 * the value instruction bits are compared against to see which instruction
                 * we are decoding should always be decode
                 */
                if ( isa< Constant >( o )) {
                    write( o, sem_taint::Decode );
                }
            }
            return;
        }
        if ( is_one_of< Memory, Advice >( op )) {
            /*
             * Memory is always an operand to an instruction
             */
            return write( op, sem_taint::Config );
        }
        if ((isa< leaf_values_ts >( op ) && !isa< InputInstructionBits >( op ))) {
            /*
             * This should get all registers and other machine state related nodes
             */
            return write( op, sem_taint::State );
        }
        // non terminals
        if ( op->operands_size() == 1 ) { // single child, should just pass on
            return write( op, read_semantics( op->operand( 0 ) ));
        }
        if ( should_promote_to_semantics( op )) {
            return write( op, sem_taint::Semantics );
        }
        if ( !all_children_are_same( op )) {
            return write( op, sem_taint::Config );
        }
        // we know we are not a leaf node and all nodes are the same
        return write( op, read_semantics( op->operand( 0 ) ));
    }

    void SemanticsTainterVisitor::visit( circ::Operation *op )
    {
        taint( op );
        op->traverse_upwards( *this );
    }

    bool SemanticsTainterVisitor::should_promote_to_semantics(Operation *op) {
        std::vector<Operation*> promote_to_config;

        for ( auto &o: op->operands() )
        {
            auto o_sem = read_semantics( o );
            if( o_sem == sem_taint::Config || o_sem == sem_taint::Semantics)
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
             */
            if ( isa< Constant >( o ) && !all_children_are_same( op ))
            {
                promote_to_config.push_back( o );
                continue;
            }
            return false;
        }

        for(auto o : promote_to_config)
            write( o, sem_taint::Config);

        return true;
    }

    bool SemanticsTainterVisitor::all_children_are_same( Operation *op )
    {
        if ( op->operands_size() < 2 )
            return true;

        auto first = read_semantics( op->operand( 0 ) );
        for ( auto &o : op->operands() )
        {
            if ( first != read_semantics( o ) )
            {
                return false;
            }
        }
        return true;
    }

    void SemanticsTainterVisitor::write( Operation *op, sem_taint value )
    {
        op->set_meta< true >( meta_key, semantic_to_string( value ) );
    };

    std::string semantic_to_string( sem_taint sc )
    {
        switch ( sc )
        {
            case sem_taint::None: return "None";
            case sem_taint::Decode: return "Decode";
            case sem_taint::State: return "State";
            case sem_taint::Config: return "Config";
            case sem_taint::Semantics: return "Semantics";
            case sem_taint::Delete: return "Delete";
        }
    }

    sem_taint read_semantics( Operation *op )
    {
        auto key = SemanticsTainterVisitor::meta_key;
        if ( !op->has_meta( key ) )
            return sem_taint::None;
        if ( op->get_meta( key ) == "None" )
            return sem_taint::None;
        if ( op->get_meta( key ) == "Decode" )
            return sem_taint::Decode;
        if ( op->get_meta( key ) == "State" )
            return sem_taint::State;
        if ( op->get_meta( key ) == "Config" )
            return sem_taint::Config;
        if ( op->get_meta( key ) == "Semantics" )
            return sem_taint::Semantics;
        if ( op->get_meta( key ) == "Delete" )
            return sem_taint::Delete;

        circ::unreachable() << "could not decode semantics";
    }

    void SemanticsTainterRemovalVisitor::visit( Operation *op )
    {
        op->remove_meta(SemanticsTainterVisitor::meta_key);
        op->traverse(*this);
    }
}
