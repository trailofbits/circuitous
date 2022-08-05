#include <circuitous/IR/Circuit.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Decoder/DecodeAST.hpp>
namespace circ::decoder::semantics
{
    void print_semantics( Circuit *op );
    void print_semantics( VerifyInstruction *op );

    // Templated for constraints?
    std::string get_target( Operation *op );

    std::string get_semantics( Operation *op, VerifyInstruction *vi );

    Operation* get_adviced_value( Advice *advice , Operation *VI );

    struct ToSemanticsIRVisitor : NonDefaultingVisitor<ToSemanticsIRVisitor>
    {
        void visit(Select* op){
            std::cout << "omg -=----------" << std::endl;
        }

        void visit(Operation* op){
            std::cout << "sad:((((((((" << std::endl;
        }

    };

    struct SelectStorage{
        void register_select (Select* sel);
        decoder::FunctionCall get_specialization (circ::Select* sel, Expr index);
        std::vector<decoder::FunctionDeclaration> get_functions_for_select();

    private:
        std::unordered_map<std::size_t, decoder::FunctionDeclaration> selects;
        std::size_t hash_select_targets(Select* sel);
    };



    struct circIR_to_semIR_visitor : Visitor<circIR_to_semIR_visitor>
    {
        SelectStorage* st;
        VerifyInstruction* vi;
        int dispatch_counter = 0;

        circIR_to_semIR_visitor( SelectStorage *st, VerifyInstruction *vi ) :  st( st ), vi( vi )
        { }
        Expr visit(InputRegister *op)  { return Var(op->reg_name); }
        Expr visit(OutputRegister *op) { return Var(op->reg_name); }

        Expr visit(InputTimestamp *op)  { return Var(op->name()); }
        Expr visit(OutputTimestamp *op) { return Var(op->name()); }
        Expr visit(InputErrorFlag *op)  { return Var(op->name()); }
        Expr visit(OutputErrorFlag *op) { return Var(op->name()); }

//        Expr visit(Undefined *op) { return sized(op); }

//        Expr visit(Memory *op)   { return memop(op); }

        // TODO implement numbers
        Expr visit(Constant *op) { return Var(op->bits); }

        // TODO get adviced global func?
        Expr visit(Advice *op) { return dispatch( get_adviced_value(op, vi)); }

        Expr visit(InputInstructionBits *op) { return Var("input_bits"); }

        Expr visit(RegConstraint *op)
        {
            return Equal(
                (*this).dispatch(op->operands[0]),
                (*this).dispatch(op->operands[1])
            );
        }

        // TODO Remove this ?
        Expr visit(AdviceConstraint *op)
        {
            return Var("AC");
        }

        // TODO Implement memory
//        Expr visit(WriteConstraint *op)     { return opcode(op); }
//        Expr visit(ReadConstraint *op)      { return opcode(op); }

        // TODO Do we allow undeff?
//        Expr visit(UnusedConstraint *op)    { return opcode(op); }



        Expr visit(Add *op) { return binops<decoder::Plus>(op); }
//        Expr visit(Sub *op) { return binops<decoder::Sub>(op); }
        Expr visit(circ::Mul *op) { return binops<decoder::Mul>(op); }

        //TODO implement div
//        Expr visit(UDiv *op) { return binops<>(op); }
//        Expr visit(SDiv *op) { return binops<>(op); }

        //TODO implement div
//        Expr visit(SRem *op) { return binops<>(op); }
//        Expr visit(URem *op) { return binops<>(op); }

        Expr visit(Shl *op)  { return binops<Shfl>(op); }
        //TODO implement Lshr
//        Expr visit(LShr *op) { return binops<>(op); }
//        Expr visit(AShr *op) { return binops<>(op); }

//        Expr visit(Trunc *op) { return sized(op); }
//        Expr visit(ZExt *op)  { return sized(op); }
//        Expr visit(SExt *op)  { return sized(op); }

//        Expr visit(Icmp_ult *op)  { return sized(op); }
//        Expr visit(Icmp_slt *op)  { return sized(op); }
//        Expr visit(Icmp_ugt *op)  { return sized(op); }
        Expr visit(Icmp_eq *op)   { return binops<Equal>(op); }
//        Expr visit(Icmp_ne *op)   { return binops<Unequal>(op); }
//        Expr visit(Icmp_uge *op)  { return sized(op); }
//        Expr visit(Icmp_ule *op)  { return sized(op); }
//        Expr visit(Icmp_sgt *op)  { return sized(op); }
//        Expr visit(Icmp_sge *op)  { return sized(op); }
//        Expr visit(Icmp_sle *op)  { return sized(op); }
//
//        Expr visit(InputImmediate *op) { return sized(op); }

        // TODO Fix
        Expr visit(Extract *op) {
            std::stringstream ss;
            ExpressionPrinter ep(ss);
            ep.print(dispatch(op->operands[0]));
            auto val = ss.str();
            auto low = std::to_string(op->low_bit_inc);
            auto high = std::to_string(op->low_bit_inc);
            return Id(val + "[" + low + ".." + high+ "]"); }
        Expr visit(Operation*op)
        {
            dispatch_counter++;
            if(dispatch_counter > 100)
                circ::unreachable() << "threshold reached on:" << std::to_string(static_cast<uint32_t>(op->op_code));
            return dispatch(op);
        }

        Expr visit(Concat *op) {
            std::stringstream ss;
            ExpressionPrinter ep(ss);
            ep.print(dispatch(op->operands[0]));
            auto lhs = ss.str();

//                  circ::unreachable() << "misformed concat";
            ss.clear();
            std::stringstream ss2;
            ExpressionPrinter ep2(ss2);

            if(op->operands.size() == 1)
                return Id(lhs);
            //
            ep2.print(dispatch(op->operands[1]));
            auto rhs = ss2.str();

            return Id(lhs + " ++ " + rhs);
        }
//
//        Expr visit(PopulationCount *op)     { return sized(op); }
//        Expr visit(CountLeadingZeroes *op)  { return sized(op); }
//        Expr visit(CountTrailingZeroes *op) { return sized(op); }
//
//        Expr visit(Not *op) { return sized(op); }
//
//        Expr visit(Parity *op) { return opcode(op); }
//
        Expr visit(Select *op) { return st->get_specialization(op, dispatch(op->operands[0])); }
//
//        Expr visit(DecodeCondition *op)   { return opcode(op); }
//        Expr visit(DecoderResult *op)     { return opcode(op); }
//        Expr visit(VerifyInstruction *op) { return opcode(op); }
//        Expr visit(OnlyOneCondition *op)  { return opcode(op); }
//
//        Expr visit(Or *op)  { return sized(op); }
//        Expr visit(And *op) { return sized(op); }
//        Expr visit(Xor *op) { return sized(op); }

        Expr visit(Circuit * c) { unreachable() << "Unexpected case encountered in visit."; }

        template <typename bin_op> requires std::is_convertible_v<bin_op, Expr>
        Expr binops(Operation* op){
            return std::move(bin_op(dispatch(op->operands[0]), dispatch(op->operands[1])));
        }
    };
}
