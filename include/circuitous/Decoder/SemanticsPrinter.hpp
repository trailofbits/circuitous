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
    /*
     * leaf discovery -> create intrinsics for each
     * Select discovery -> returns intrinsics or leaves there of
     * Specialize -> run up from instr_bits, replace instr_bits with constants
     *                                      && replace select with select discovery funcs
     *
     * Transform into llvm ir
     *
     *  We runnen dus een circ -> llvm pass twee keer?
     *      als we de "perfecte circIR" hebben, moeten we dan ook nog twee passes hebben? nee
     *
     *  Geen idee of extract nodig is, maar is de enige die lichtelijk kut is?
     *
     * idea for current approach
     * set asside support extract and see how it will be handled later
     *
     */
    constexpr static const std::string_view name_irb = "irb";
    constexpr static const std::string_view name_llvm_module = "irb";



    struct SelectStorage{
        void register_select (Select* sel);
        decoder::FunctionCall get_specialization (circ::Select* sel, Expr index);
        std::vector<decoder::FunctionDeclaration> get_functions_for_select();

       private:
        std::unordered_map<std::size_t, decoder::FunctionDeclaration> selects;
        std::size_t hash_select_targets(Select* sel);
    };


    void print_semantics( Circuit *op );
    FunctionDeclaration get_function_for_VI( VerifyInstruction *op, SelectStorage &st,
                                             uint32_t size_of_encoding );

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


    struct select_vis : UniqueVisitor<select_vis>
    {
        SelectStorage* st;
        select_vis( SelectStorage *st ) : st( st ) { }
        void visit(Operation* op){ op->traverse(*this); }
        void visit(Select* op)
        {
            st->register_select(op);
        }
    };


    struct circIR_to_llvmIR_visitor : Visitor< circIR_to_llvmIR_visitor >
    {
        SelectStorage* st;
        VerifyInstruction* vi;
        std::string name_irb_ref;
        int dispatch_counter = 0;

        FunctionCall call_irb(std::string method_name, std::vector<Expr> args){
            return FunctionCall("irb." + method_name, args);
        }
        FunctionCall call_irb_binops(std::string method_name, Operation* op){
            return FunctionCall("irb." + method_name, { dispatch( op->operands[ 0 ] ), dispatch( op->operands[ 1 ] ) } );
        }

        Id getAPInt(std::string bits, uint size){return "llvm::APInt(\"" + bits + "\", size)";}

        circIR_to_llvmIR_visitor( SelectStorage *st, VerifyInstruction *vi,
                                  std::string nameIrbRef ) :  st( st ), vi( vi ), name_irb_ref( nameIrbRef )
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
        Expr visit(Constant *op) {
            return call_irb("getIntN", { Int(op->size) , Id("0b" + std::string(op->bits.rbegin(), op->bits.rend()))});
        }

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



        Expr visit(Add *op) { return call_irb_binops("CreateAdd", op); }
        Expr visit(Sub *op) { return call_irb_binops("CreateSub", op); }
        Expr visit(circ::Mul *op) { return call_irb_binops("CreateMul", op); }
        Expr visit(UDiv *op) { return call_irb_binops("CreateUDiv", op); }
        Expr visit(SDiv *op) { return call_irb_binops("CreateSDiv", op); }
        Expr visit(SRem *op) { return call_irb_binops("CreateSRem", op); }
        Expr visit(URem *op) { return call_irb_binops("CreateURem", op); }
        Expr visit(Shl *op)  { return call_irb_binops("CreateShl", op); }
        Expr visit(LShr *op) { return call_irb_binops("CreateLsr", op); }
        Expr visit(AShr *op) { return call_irb_binops("CreateAShr", op); }
        Expr visit(Trunc *op) { return call_irb_binops("CreateTrunc", op); }
        Expr visit(ZExt *op)  { return call_irb_binops("CreateZExt", op); }
        Expr visit(SExt *op)  { return call_irb_binops("CreateSExt", op); }
        Expr visit(Icmp_ult *op)  { return call_irb_binops("CreateIcmpULT", op); }
        Expr visit(Icmp_slt *op)  { return call_irb_binops("CreateIcmpSLT", op); }
        Expr visit(Icmp_ugt *op)  { return call_irb_binops("CreateIcmpUGT", op); }
        Expr visit(Icmp_eq *op)   { return call_irb_binops("CreateIcmpEQ", op); }
        Expr visit(Icmp_ne *op)   { return call_irb_binops("CreateIcmpNE", op); }
        Expr visit(Icmp_uge *op)  { return call_irb_binops("CreateIcmpUGE", op); }
        Expr visit(Icmp_ule *op)  { return call_irb_binops("CreateIcmpULE", op); }
        Expr visit(Icmp_sgt *op)  { return call_irb_binops("CreateIcmpSGT", op); }
        Expr visit(Icmp_sge *op)  { return call_irb_binops("CreateIcmpSGE", op); }
        Expr visit(Icmp_sle *op)  { return call_irb_binops("CreateIcmpSLE", op); }

//        Expr visit(InputImmediate *op) { return sized(op); }

        // TODO Fix`
        Expr visit(Extract *op)
        {
            circ::unreachable() << "Currently testing if we need to implement this to begin with";
//            std::stringstream ss;
//            ExpressionPrinter ep(ss);
//            ep.print(dispatch(op->operands[0]));
//            auto val = ss.str();
//            auto low = std::to_string(op->low_bit_inc);
//            auto high = std::to_string(op->high_bit_exc);
//            return Id(val + "[" + low + ".." + high+ "]");
        }

        Expr visit(Operation*op)
        {
            dispatch_counter++;
            if(dispatch_counter > 100)
                circ::unreachable() << "threshold reached on:" << std::to_string(static_cast<uint32_t>(op->op_code));
            return dispatch(op);
        }

        /*
         * find size of lhs and cacluate lhs + (rhs << size_of_lhs)
         */
        Expr visit(Concat *op) {
            if (op->operands.size() == 1)
                return dispatch(op->operands[0]);

            circ::check(op->operands.size() == 2)
                << "Concat does not have 2  children, instead has: "
                << op->operands.size();

            auto lhs = op->operands[0];
            auto rhs = op->operands[1];
            std::size_t size = 0;


            if (isa<Advice>(lhs))
                lhs = get_adviced_value(static_cast<Advice *>(lhs), vi);

            if (isa<Constant>(lhs))
                size = static_cast<Constant *>(lhs)->size;
            else if (isa<Extract>(lhs)) {
                auto extract = static_cast<Extract *>(lhs);
                size = extract->high_bit_exc - extract->low_bit_inc;
            } else
                circ::unreachable()
                    << "could not deduce width of concat target" << lhs->name();
//            auto x = std::bit_width(1ul);

            auto new_rhss =
                decoder::Shfl(dispatch(rhs), Int(static_cast<long>(size)));
            return decoder::Plus(new_rhss, dispatch(lhs));
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
