/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/ToLLVM.hpp>

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Support/Check.hpp>

#include <gap/core/ranges.hpp>

namespace circ
{
    namespace
    {
        llvm::Function *make_fn( llvm::Module *lmodule, const std::string &name )
        {
            auto &ctx = lmodule->getContext();
            auto fn_type = llvm::FunctionType::get( llvm::Type::getVoidTy( ctx ), {}, false );
            auto fn = lmodule->getOrInsertFunction( name, fn_type );
            return llvm::dyn_cast< llvm::Function >( fn.getCallee() );
        }

        struct ToLLVM : Visitor< ToLLVM, true >
        {
            using op_size_t = unsigned;
            using parent_t = Visitor< ToLLVM, true >;

            llvm::IRBuilder<> &irb;
            const Circuit *circuit;

            std::unordered_map< const Operation *, llvm::Value * > to_vals;

            ToLLVM( llvm::IRBuilder<> &irb, const Circuit *circuit )
                : irb( irb ), circuit( circuit )
            {}

            llvm::Value *get( const Operation *op )
            {
                if ( auto val = get_val( op ) )
                    return *val;
                return give_val( op, this->dispatch( op ) );
            }

            gap::generator< llvm::Value * > get( gap::ranges::range auto ops )
            {
                for ( auto o : ops )
                    co_yield get( o );
            }

            std::vector< llvm::Value * > get_frozen( gap::ranges::range auto ops )
            {
                std::vector< llvm::Value * > out;
                for ( auto o : get( std::move( ops ) ) )
                    out.push_back( o );
                return out;
            }

            std::optional< llvm::Value * > get_val( const Operation *op )
            {
                auto it = to_vals.find( op );
                if ( it == to_vals.end() )
                    return {};
                return it->second;
            }

            llvm::Value *give_val( const Operation *op, llvm::Value *v )
            {
                to_vals[ op ] = v;
                return v;
            }

            // TODO(lukas): Remove.
            void emit()
            {
                irb.CreateRet( get( circuit->root ) );
            }

            // TODO(lukas): Remove default once implementation is done, as this should
            //              not compile if a visit is missing.
            llvm::Value *visit( const Operation *op )
            {
                log_kill() << "Default ToLLVM::visit() was reached a case is missing for"
                           << pretty_print< true >( op );
            }

            // Non-io leaves

            llvm::Value *visit( const Constant *op )  { return make_int( op->size, op->bits ); }
            llvm::Value *visit( const Undefined *op ) { return make_undef( op->size ); }

            // Input trace

            llvm::Value *visit( const Advice *op )
            {
                return mk_leaf< irops::AdviceIndexed >( op->size, op->advice_idx );
            }

            llvm::Value *visit( const Memory *op )
            {
                return mk_leaf< irops::Memory >( op->size, op->mem_idx );
            }

            using io = irops::io_type;

            llvm::Value *visit( const InputRegister *op )
            {
                return mk_leaf< irops::Reg >( op->size, op->reg_name, io::in );
            }

            llvm::Value *visit( const InputErrorFlag *op )
            {
                return mk_leaf< irops::ErrorBit >( io::in );
            }

            llvm::Value *visit( const InputTimestamp *op )
            {
                return mk_leaf< irops::Timestamp >( io::in );
            }

            llvm::Value *visit( const InputInstructionBits *op )
            {
                return mk_leaf< irops::InstBits >( io::in );
            }

            // Output trace

            llvm::Value *visit( const OutputRegister *op )
            {
                return mk_leaf< irops::Reg >( op->size, op->reg_name, io::out );
            }

            llvm::Value *visit( const OutputErrorFlag *op )
            {
                return mk_leaf< irops::ErrorBit >( io::out );
            }

            llvm::Value *visit( const OutputTimestamp *op )
            {
                return mk_leaf< irops::Timestamp >( io::out );
            }

            llvm::Value *visit( const Extract *op )
            {
                // ( from, size )
                return make_intrinsic< irops::ExtractRaw >( get( op->operands() ),
                                                                 op->low_bit_inc,
                                                                 op->extracted_size() );
            }

            llvm::Value *visit( const Select *op )
            {
                auto llvm_ops = get_frozen( op->operands() );
                if ( op->operands_size() == 3 )
                    // LLVM select has true branch first.
                    return irb.CreateSelect( llvm_ops[ 0 ], llvm_ops[ 2 ], llvm_ops[ 1 ] );
                return irops::make< irops::Select >( irb, std::move( llvm_ops ) );

            }

            llvm::Value *visit( const Not * )
            {
                log_kill() << "Not implemented";
            }

            #define circuitous_tollvm_mk_cast( operation, call ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    dcheck( op->operands_size() == 1, []() { return "Expected unary op."; } );\
                    auto llvm_op = get( op->operand( 0 ) ); \
                    return irb.call( llvm_op, irb.getIntNTy( op->size ) ); \
                }

            circuitous_tollvm_mk_cast( SExt,  CreateSExt  );
            circuitous_tollvm_mk_cast( ZExt,  CreateZExt  );
            circuitous_tollvm_mk_cast( Trunc, CreateTrunc );

            #undef circuitous_tollvm_mk_cast

            #define circuitous_tollvm_mk_constraint( operation, irop ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    return make_intrinsic< irops::irop >( get( op->operands() ) ); \
                }

            circuitous_tollvm_mk_constraint( AdviceConstraint, AdviceConstraint );
            circuitous_tollvm_mk_constraint( RegConstraint, OutputCheck );
            circuitous_tollvm_mk_constraint( ReadConstraint, ReadConstraint );
            circuitous_tollvm_mk_constraint( WriteConstraint, WriteConstraint );
            circuitous_tollvm_mk_constraint( UnusedConstraint, UnusedConstraint );
            circuitous_tollvm_mk_constraint( DecodeCondition, DecodeCondition );
            circuitous_tollvm_mk_constraint( DecoderResult, DecoderResult );

            circuitous_tollvm_mk_constraint( InputImmediate, InputImmediate );
            #undef circuitous_tollvm_mk_constraint


            #define circuitous_tollvm_mk_sized_intrinsic( operation, irop ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    return make_intrinsic< irops::irop >( get( op->operands() ), op->size ); \
                } \

            circuitous_tollvm_mk_sized_intrinsic( Concat, Concat );
            circuitous_tollvm_mk_sized_intrinsic( OnlyOneCondition, Xor );
            circuitous_tollvm_mk_sized_intrinsic( VerifyInstruction, VerifyInst );


            #undef circuitous_tollvm_mk_sized_intrinsic

            #define circuitous_tollvm_mk_bin( operation, call ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    auto b = get_frozen( op->operands() ); \
                    dcheck( b.size() == 2, []() { return "Expected two operands."; } );\
                    return irb.call( b[ 0 ], b[ 1 ] ); \
                }

            circuitous_tollvm_mk_bin( Xor,  CreateXor  );

            circuitous_tollvm_mk_bin( Add,  CreateAdd  );
            circuitous_tollvm_mk_bin( Sub,  CreateSub  );
            circuitous_tollvm_mk_bin( Mul,  CreateMul  );
            circuitous_tollvm_mk_bin( UDiv, CreateUDiv );
            circuitous_tollvm_mk_bin( SDiv, CreateSDiv );
            circuitous_tollvm_mk_bin( Shl,  CreateShl  );

            circuitous_tollvm_mk_bin( LShr,  CreateLShr  );
            circuitous_tollvm_mk_bin( AShr,  CreateAShr  );

            circuitous_tollvm_mk_bin( URem,  CreateURem  );
            circuitous_tollvm_mk_bin( SRem,  CreateSRem  );

            circuitous_tollvm_mk_bin( Icmp_ult, CreateICmpULT );
            circuitous_tollvm_mk_bin( Icmp_slt, CreateICmpSLT );
            circuitous_tollvm_mk_bin( Icmp_ugt, CreateICmpUGT );
            circuitous_tollvm_mk_bin( Icmp_eq,  CreateICmpEQ  );
            circuitous_tollvm_mk_bin( Icmp_ne,  CreateICmpNE  );
            circuitous_tollvm_mk_bin( Icmp_uge, CreateICmpUGE );
            circuitous_tollvm_mk_bin( Icmp_ule, CreateICmpULE );
            circuitous_tollvm_mk_bin( Icmp_sgt, CreateICmpSGT );
            circuitous_tollvm_mk_bin( Icmp_sge, CreateICmpSGE );
            circuitous_tollvm_mk_bin( Icmp_sle, CreateICmpSLE );

            #undef circuitous_tollvm_mk_bin

            #define circuitous_tollvm_mk_bin_or_intrinsic( operation, call, irop ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    if ( op->operands_size() == 2 ) \
                    { \
                        auto b = get_frozen( op->operands() ); \
                        return irb.call( b[ 0 ], b[ 1 ] ); \
                    } \
                    return make_intrinsic< irops::irop >( get( op->operands() ) ); \
                }

            circuitous_tollvm_mk_bin_or_intrinsic( And,  CreateAnd, And  );
            circuitous_tollvm_mk_bin_or_intrinsic( Or,   CreateOr,  Or   );

            #undef circuitous_tollvm_mk_bin_or_intrinsic

            #define circuitous_tollvm_mk_unary_intrinsic( operation, id ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    dcheck( op->operands_size() == 1, []() { return "Expected unary op."; } );\
                    auto llvm_op = get( op->operand( 0 ) ); \
                    return irb.CreateUnaryIntrinsic( id, llvm_op ); \
                }

            #define circuitous_tollvm_mk_llvm_intrinsic( operation, id ) \
                llvm::Value *visit( const operation *op ) \
                { \
                    dcheck( op->operands_size() == 1, []() { return "Expected unary op."; } );\
                    auto llvm_op = get( op->operand( 0 ) ); \
                    return irb.CreateBinaryIntrinsic( id, llvm_op, irb.getTrue() ); \
                }

            circuitous_tollvm_mk_unary_intrinsic( PopulationCount, llvm::Intrinsic::ctpop );
            circuitous_tollvm_mk_llvm_intrinsic( CountLeadingZeroes, llvm::Intrinsic::ctlz );
            circuitous_tollvm_mk_llvm_intrinsic( CountTrailingZeroes, llvm::Intrinsic::cttz );


            #undef circuitous_tollvm_mk_unary_intrinsic
            #undef circuitous_tollvm_mk_llvm_intrinsic

            llvm::Value *visit( const ExternalComputation *op )
            {
                log_kill() << "Not implemented";
            }

            // Helpers

            template< typename IROp, typename ... Args >
            llvm::Value *mk_leaf( Args && ... args )
            {
                return irops::make_leaf< IROp >( irb, std::forward< Args >( args ) ... );
            }

            template< typename IROp, typename ... Args >
            llvm::Value *make_intrinsic( gap::ranges::range auto &&ops, Args && ... args )
            {
                return irops::make< IROp >( irb, std::move( ops ),
                                            std::forward< Args >(args) ... );
            }

            template< typename IROp >
            llvm::Value *make_sized_intrinsic( op_size_t size, auto &&ops )
            {
                return irops::make< IROp >( irb, get( std::move( ops ) ), size );
            }

            llvm::Value *make_undef( op_size_t size )
            {
                return llvm::UndefValue::get( irb.getIntNTy( size ) );
            }

            llvm::Value *make_int( op_size_t size, std::string bits )
            {
                std::reverse( bits.begin(), bits.end() );
                return irb.getInt( llvm::APInt( size, bits, 2 ) );
            }

        };

    } // namespace

    // Simply convert circuit to llvm function.
    // Returns context and function.
    auto convert_to_llvm( Circuit *circuit, const std::string &module_name )
        -> std::tuple< std::shared_ptr< llvm::LLVMContext >, std::unique_ptr< llvm::Module > >
    {
        auto ctx = std::make_shared< llvm::LLVMContext >();
        auto lmodule = std::make_unique< llvm::Module >( module_name, *ctx );

        // TODO(lukas): Set triplet?

        std::ignore = convert_to_llvm( circuit, lmodule.get(), "circuitous.circuit_fn" );
        return { std::move( ctx ), std::move( lmodule ) };
    }

    void convert_to_llvm( Circuit *circuit, llvm::IRBuilder<> &irb )
    {
        return ToLLVM( irb, circuit ).emit();
    }

    // Convert circuit to llvm function with given name in the provided module.
    auto convert_to_llvm( Circuit *circuit, llvm::Module *lmodule, const std::string &name )
        -> llvm::Function *
    {
        auto fn = make_fn( lmodule, name );
        check( fn );

        auto entry = llvm::BasicBlock::Create( lmodule->getContext(), "", fn );
        auto irb = llvm::IRBuilder<>( entry );
        convert_to_llvm( circuit, irb );
        return fn;
    }

} // namespace circ
