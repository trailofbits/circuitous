#include "DecoderPrinter.hpp"

#include <circuitous/Decoder/DecodeAST.hpp>
#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>
namespace circ::decoder::semantics
{


    /*
     * Approach for semantics emitter:
     *  Create enums <Arity>Operation i.e UnaryOperation::Trunc, BinaryOperation::Add. Note that arity is w.r.t to undefined arguments, So Trunc is unary as the size to truncate to is known beforehand
     *
     *  goal is to create a minimal set of functions such that it can execute (OP_OF_ARITY_X, <arg_1, arg_2, ..., arg_X>)
     *  Advantage of all operations being passed a lot of possible re-use.
     *  Disadvantage is that we have a separate function for every possible (used) call structure
     *      i.e (a op b) op c generates a different function than a op (b op c)
     *
     *  The entire call structure can be obtained by looking at the vector of the arguments.
     *  As each (sub)tree starts with a known N-ary value, we can parse until we hit N arguments which represent a terminal value.
     *
     */



    /*
     *  General:
     *      Create enum for all leaves/registers
     *      Create a gen'd visitor struct with a method for each non-enumified op (visit_name)
     *
     *  Semantics:
     *      Each VI Emits a function
     *          visitor either emits enum value or struct call
     *          enum values are added to arguments
     *          struct call can contain pre-filled in values to struct call (trunc_size)
     *
     *  Decoder side: Create enum conversion functions from decoder selects
     *
      *
     * How do we want to handle a function which can only do a subset of the registers?
     *      assume this doesn't exist?
     */


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
    static const Var irb("irb", "llvm::IRBuilder<>");
    static const Var llvm_context("llvm_context", "llvm::LLVMContext");
    static const Var llvm_module("llvm_context", "std::unique_ptr<llvm::Module>");

    Expr emit_llvm_context();



    void print_semantics( Circuit *op );
    FunctionDeclaration get_function_for_VI( VerifyInstruction *op, SelectStorage &st,
                                             uint32_t size_of_encoding );


    Operation* get_adviced_value( Advice *advice , Operation *VI );


    struct OperationVisitor : UniqueVisitor<OperationVisitor>
    {
        //TODO Maybe rename In/OUtput register to Read/Write memory
        void visit(Operation* op)
        {
            auto key = std::make_pair(op->op_code, op->size);
            if(!ops.contains( key ))
                ops.insert( { key, op } );

            op->traverse(*this);
        }

        std::map<std::pair<circ::Operation::kind_t, int>, Operation*> ops;

        std::vector<Enum> get_enums ()
        {
            std::vector<Enum> operations;
            operations.push_back( make_enum( "UnaryOperation", 1 ) );
            operations.push_back( make_enum( "BinaryOperation", 2 ) );
            operations.push_back( make_enum( "TertiaryOperation", 3 ) );
            return get_enums();
        }

        Enum make_enum(const std::string &name, int arity)
        {
            Enum e( name );
            for ( auto& pair : ops )
            {
                if(pair.first.second == arity)
                    e.Register(pair.second->op_code_str(), pair.second);
            }
            return e;
        }
    };

    struct OpCall {
        FunctionCall op;
        std::vector<Operation *> argumetns; //leave argumetns that will need to be converted, only values from the enum


    };

    struct FunctionVisitor : Visitor<FunctionVisitor>
    {
        using hash_t = std::string;
        std::pair<FunctionCall, hash_t> visit(Operation *op){
            std::stringstream ss;
            ss << std::to_string(op->size);
            for(auto o : op->operands)
                ss << dispatch(o).second;

            FunctionCall call = FunctionCall(hash, )
        }


    };

    /*
     * The first pass of semantics emission is to go and find all possible specializations
     *      A specialization is:
     *          a decode time variant on some semantic function
     *          an enum in the auto-generated code
     *          the value inside the semantic function is the _subtree_ of select value
     *              currently we only only have subtrees of depth one, so we emit them directly
     *              The emission of the value should be done to a call to a conversion function accepting the visitor
     *
     *          Requires two conversion functions
     *              bit sequence to Enum value (decoder)
     *              Enum value to semantics it represents (semantics)
     *
     */
    struct SpecializationVisitor : UniqueVisitor<SpecializationVisitor>
    {
        void visit( Select *op )
        {
            // TODO(Sebas) check for if this solely depends on decode

            if(op->operands.size() < 2)
                return;

            auto first_type = op->operands[1];
            if( is_one_of<InputInstructionBits, Advice, Memory>(first_type) || !is_one_of<leaf_values_ts>(first_type))
                return;

            // are all operands of the same type, probably a redundant check
            for(std::size_t i = 1; i < op->operands.size(); i++)
            {
                if(first_type->op_code != op->operands[i]->op_code)
                    return;
            }

            std::optional<Enum> enum_to_add_to = std::nullopt;

            // find viable enum if it exists
            // this would be perfect for the c++23 optional and_then :/
            for(std::size_t i = 1; i < op->operands.size(); i++)
            {
                for (auto& e : enums )
                {
                    if(e.get_by_op(op->operands[i]).has_value())
                    {
                        enum_to_add_to = e;
                    }
                }
            }

            // add if it doesn't exist
            if(!enum_to_add_to.has_value())
            {
                // TODO proper name generation
                enum_to_add_to = Enum("some_new_enum_name");
                enums.push_back(enum_to_add_to.value());
            }

            // add values which aren't present to the enum
            for(std::size_t i = 1; i < op->operands.size(); i++) {
                auto new_op = op->operands[i];
                if(!enum_to_add_to.value().get_by_op(new_op).has_value())
                    // TODO(Sebas) make this an enum friendly name
                    enum_to_add_to.value().Register(new_op->name(), new_op);
            }
        }

        // Can there be an operation which has been accepted by as a specialization which would still show up in the other
        // Conversion from Enum -> T needs to be user provided


        /*
         * ir emitter would have
         *  and(Register reg, Register reg) // VI
         *  and(T t, T t); // Operation defined on the visitor
         *  requires
         *      Visitor creating an instance of the specialization? -- I don't think we can get around that
         */

        void visit( Operation *op ) { op->traverse(*this); }
        /*
         * Each name inside an enum should be globally unique.
         * Enums should be merged based on type, so there should be an enum GPR ect.
         * Notice that in circIR there can exists multiple selects addressing over different subsets of an enum
         * and that these also might be duplicated.
         *
         * Merging is done in the following way:
         *  For all selects that depend on (tainted as) decode values
         *      if two values share a parent select, then both get put inside the same enum.
         *      Can be seen as building equivalence classes
         */
        std::vector<Enum> enums;
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


    struct operation_to_vistor_setup_visitor;

    struct SemanticsFunctionBodyVisitor : Visitor< SemanticsFunctionBodyVisitor >
    {
        SelectStorage* st;

        VerifyInstruction* vi;
        std::string name_irb_ref;

        int dispatch_counter = 0;
        Var name_context_ref;

        FunctionCall call_irb(std::string method_name, std::vector<Expr> args){
            return FunctionCall("irb." + method_name, args);
        }
        FunctionCall call_visit(std::string method_name, Operation* op){
            return FunctionCall("impl.visit_" + method_name, { dispatch( op->operands[ 0 ] ), dispatch( op->operands[ 1 ] ) } );
        }

        FunctionCall getIntN(uint n){
            return FunctionCall("llvm::Type::getIntNTy", { name_context_ref, Int(n) } );
        }

        SemanticsFunctionBodyVisitor( SelectStorage *st, VerifyInstruction *vi,
                                  std::string nameIrbRef, Var nameContextRef ) :  st( st ), vi( vi ), name_irb_ref( nameIrbRef ), name_context_ref( nameContextRef )
        { }

        //TODO should be calls to instrinsics
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

        Expr visit( RegConstraint *op )
        {
            return Equal( dispatch( op->operands[ 0 ] ), dispatch( op->operands[ 1 ] ) );
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



        Expr visit(circ::Add *op) { return call_visit( "Add", op ); }
        Expr visit(circ::Sub *op) { return call_visit( "Sub", op ); }
        Expr visit(circ::Mul *op) { return call_visit( "Mul", op ); }
        Expr visit(circ::UDiv *op) { return call_visit( "UDiv", op ); }
        Expr visit(circ::SDiv *op) { return call_visit( "SDiv", op ); }
        Expr visit(circ::SRem *op) { return call_visit( "SRem", op ); }
        Expr visit(circ::URem *op) { return call_visit( "URem", op ); }
        Expr visit(circ::Shl *op)  { return call_visit( "Shl", op ); }
        Expr visit(circ::LShr *op) { return call_visit( "Lsr", op ); }
        Expr visit(circ::AShr *op) { return call_visit( "AShr", op ); }
        Expr visit(circ::Trunc *op) { return call_visit( "Trunc", op ); }
        Expr visit(circ::ZExt *op)  { return call_visit( "ZExt", op ); }
        Expr visit(circ::SExt *op)  { return call_visit( "SExt", op ); }
        Expr visit(circ::Icmp_ult *op)  { return call_visit( "IcmpULT", op ); }
        Expr visit(circ::Icmp_slt *op)  { return call_visit( "IcmpSLT", op ); }
        Expr visit(circ::Icmp_ugt *op)  { return call_visit( "IcmpUGT", op ); }
        Expr visit(circ::Icmp_eq *op)   { return call_visit( "IcmpEQ", op ); }
        Expr visit(circ::Icmp_ne *op)   { return call_visit( "IcmpNE", op ); }
        Expr visit(circ::Icmp_uge *op)  { return call_visit( "IcmpUGE", op ); }
        Expr visit(circ::Icmp_ule *op)  { return call_visit( "IcmpULE", op ); }
        Expr visit(circ::Icmp_sgt *op)  { return call_visit( "IcmpSGT", op ); }
        Expr visit(circ::Icmp_sge *op)  { return call_visit( "IcmpSGE", op ); }
        Expr visit(circ::Icmp_sle *op)  { return call_visit( "IcmpSLE", op ); }

//        Expr visit(InputImmediate *op) { return sized(op); }

        // TODO Fix`
        Expr visit(circ::Extract *op)
        {
            /*
             * shift right by start offset
             * trunc to new size // does this not zero out old stuff?
             */
            check(op->operands.size() == 1 ) << "Extract has invalid operands, expected 1, was: " << op->operands.size();

            auto new_size = getIntN( op->high_bit_exc - op->low_bit_inc );
            auto shifted_const = call_irb(
                "CreateLshr", { dispatch( op->operands[ 0 ] ), Int( op->low_bit_inc ) } );
            return call_irb( "CreateTrunc", { shifted_const, new_size } );
        }

        Expr visit(Operation*op)
        {
            dispatch_counter++;
            if(dispatch_counter > 100)
                circ::unreachable() << "threshold reached on:" << std::to_string(static_cast<uint32_t>(op->op_code));
            return dispatch(op);
        }

        /*
         * find size of rhs and cacluate (lhs << rhs) + rhs
         * For N terms we have: result = (n_0 << sizeof(n_1...n_N) + (n_1 << sizeof(n_2 ... n_N) + ,,,)
         */
        Expr visit(Concat *op) {
            if (op->operands.size() == 1)
                return dispatch(op->operands[0]);

//            circ::check(op->operands.size() == 2)
//                << "Concat does not have 2  children, instead has: "
//                << op->operands.size();

            auto smallest_term = op->operands[op->operands.size() - 1];
            auto current_size = smallest_term->size;
            Expr prev_concat_term
                = call_irb("CreateAdd", { dispatch(smallest_term), Int(0)});

            for(auto it = op->operands.rbegin() + 1; it != op->operands.rend(); it++)
            {
                auto new_op = *it;
                auto lhs = call_irb( "CreateShl", { dispatch( new_op ),
                                                    Int( current_size ) } );
                current_size = current_size + new_op->size;
                prev_concat_term = call_irb("CreateAdd", {lhs, prev_concat_term });
            }

            return prev_concat_term;

        }
        Expr visit(Select *op) { return st->get_specialization(op, dispatch(op->operands[0])); }
        Expr visit(Circuit * c) { unreachable() << "Unexpected case encountered in visit."; }
        template <typename bin_op> requires std::is_convertible_v<bin_op, Expr>
        Expr binops(Operation* op){
            return std::move(bin_op(dispatch(op->operands[0]), dispatch(op->operands[1])));
        }
    };
}
