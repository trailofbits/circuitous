/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/LLVMToCircIR.hpp>

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Lifter/CircuitBuilder.hpp>
#include <circuitous/Lifter/CircuitSmithy.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>

#include <circuitous/Dbg/CtxPrint.hpp>

#include <iostream>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
CIRCUITOUS_UNRELAX_WARNINGS


#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

namespace circ
{
namespace
{

    // Returning a vector as a size is sometimes required.
    auto call_args( llvm::CallInst *call ) -> std::vector< llvm::Value * >
    {
        std::vector< llvm::Value * > out;
        for ( uint32_t i = 0; i < call->arg_size(); ++i )
        {
            // Check if we do not include the called fn by accident.
            check( !llvm::isa< llvm::Function >( call->getArgOperand( i ) ) );
            out.push_back( call->getArgOperand( i ) );
        }
        return out;
    }

    template< typename T >
    auto operand_values( llvm::Instruction *inst )
        -> gap::generator< T * >
    {
        for ( auto i : inst->operand_values() )
            if ( auto c = llvm::dyn_cast< T >( i ) )
                co_yield c;
    }

    auto operand_values( llvm::Instruction *inst )
        -> gap::generator< llvm::Value * >
    {
        for ( auto i : inst->operand_values() )
            co_yield i;
    }

    auto constants_in( llvm::BasicBlock &bb )
        -> gap::generator< llvm::Constant * >
    {
        for ( auto &inst : bb )
        {
            for ( auto operand : operand_values( &inst ) )
            {
                if ( llvm::isa< llvm::Instruction >( operand ) )
                    continue;
                // Most likely intrinsic call (or an error).
                if ( llvm::isa< llvm::Function >( operand ) )
                    continue;

                check( !llvm::isa< llvm::ConstantExpr >( operand ) );

                // Taken care in different pass
                // TODO( from-llvm ): Hard error once lifter v2 is introduced.
                if ( llvm::isa< llvm::Argument >( operand ) )
                    continue;

                if ( auto casted = llvm::dyn_cast< llvm::Constant >( operand ) )
                    co_yield casted;
                // co_yield is not a terminator!
                else
                    log_kill() << "Unsupported constant:" << dbg_dump( operand );
            }
        }
    }

    void remove_dbg( llvm::BasicBlock &block )
    {
        std::vector< llvm::Instruction * > to_remove;
        for ( auto &inst : block )
        {
            if ( auto call = llvm::dyn_cast< llvm::CallInst >( &inst ) )
            {
                auto callee = call->getCalledFunction();
                check( callee && callee->hasName() );

                if ( callee->getName().startswith( "llvm.experimental" ) )
                    to_remove.emplace_back( call );
            }
        }

        for ( auto inst : to_remove )
        {
            check( std::distance( inst->user_begin(), inst->user_end() ) == 0 );
            inst->eraseFromParent();
        }
    }

    auto size_arg( std::size_t i ) { return static_cast< uint32_t >( i ); }

    // TODO( from-llvm ): Purge this pointless forward declare.
    template < typename Self, typename RetT >
    struct BottomUpDependencyVisitor
    {
        using ret_t = RetT;

        auto &self() { return static_cast< Self & >( *this ); }

        template< typename H, typename ... Tail >
        ret_t cast_and_visit( llvm::Value *v )
        {
            auto casted = llvm::dyn_cast< H >( v );
            if ( casted )
                return self().visit( casted );
            if constexpr ( sizeof ... ( Tail ) != 0 )
                return cast_and_visit< Tail ... >( v );
            else
                return self().unknown( v );
        }

        ret_t dispatch( llvm::Value *val )
        {
            // We should never hit args as they are not present.
            // In compatibility mode they are handled separately outside of
            // main visitor.
            if ( auto arg_val = llvm::dyn_cast< llvm::Argument >( val ) )
                return self().unknown( arg_val );

            // Instruction; follow the dependency chain.
            if ( auto inst = llvm::dyn_cast< llvm::Instruction >( val ) )
            {
                return cast_and_visit<   llvm::CallInst
                                       , llvm::BinaryOperator
                                       , llvm::CmpInst
                                       , llvm::TruncInst
                                       , llvm::ZExtInst
                                       , llvm::SExtInst
                                       , llvm::ReturnInst
                                       , llvm::SelectInst
                                       , llvm::FreezeInst
                                     >( inst );
            }

            // Bottom out at a constant, ignore for now.
            if ( auto constant = llvm::dyn_cast< llvm::Constant >( val ) )
            {
                return cast_and_visit<   llvm::UndefValue
                                       , llvm::ConstantInt
                                       , llvm::ConstantFP
                                     >( constant );
            }
            unreachable() << "Unexpected value encountered during dependency visitor: "
                          << remill::LLVMThingToString(val);
        }
    };

    void lower_ces( llvm::Function *circuit_fn )
    {
        check( circuit_fn->size() == 1 ) << "Circuit function is expected to have exactly"
                                         << "one block";

        // Since we are going to modify IR, first just collect all instructions
        // so we do not accidentally invalidate iterators.
        std::vector< llvm::ConstantExpr * > to_lower;

        auto &bb = *circuit_fn->begin();
        for ( auto &inst : bb )
            if ( auto ce = llvm::dyn_cast< llvm::ConstantExpr >( &inst ) )
                to_lower.emplace_back( ce );

        for ( auto ce : to_lower )
        {
            auto ce_inst = ce->getAsInstruction();
            ce_inst->insertBefore( &*( bb.getFirstInsertionPt() ) );
            ce->replaceAllUsesWith( ce_inst );
        }

        // TODO( from-llvm ): Do we need to erase all replaced expressions? Or does
        //                    that happend by default.
    }

    struct IRImporter : public BottomUpDependencyVisitor< IRImporter, Operation * >
    {
        using parent_t = BottomUpDependencyVisitor< IRImporter, Operation * >;
        using self_t = IRImporter;

        using target_t = Operation *;
        using source_t = llvm::Value *;


        explicit IRImporter( const llvm::DataLayout &dl, Circuit *impl,
                             llvm::Function *circuit_fn )
            : dl( dl ), circuit_fn( circuit_fn ),
              impl( impl )
        {
            clock = std::chrono::steady_clock::now();
        }

        target_t unknown( llvm::Value *v )
        {
            log_kill() << dbg_dump( v ) << " encountered and there is no rule to lower it";
        }

        target_t visit( llvm::FreezeInst *freeze )
        {
            return get( freeze->getOperand( 0 ) );
        }

        target_t visit( llvm::ReturnInst *ret )
        {
            return get( ret->getOperand( 0 ) );
        }

        static uint32_t size_from_suffix( llvm::StringRef name )
        {
            if (name.endswith("_8"))    return 8u;
            if (name.endswith("_16"))   return 16u;
            if (name.endswith("_32"))   return 32u;
            if (name.endswith("_64"))   return 64u;
            if (name.endswith("_f32"))  return 32u;
            if (name.endswith("_f64"))  return 64u;
            if (name.endswith("_f80"))  return 80u;
            if (name.endswith("_f128")) return 128u;

            unreachable() << "Unsupported memory read intrinsic: " << name.str();
        }

        auto inst_bits_node()
        {
            check(impl->attr< InputInstructionBits >().size() == 1);
            return *impl->attr< InputInstructionBits >().begin();
        }

        target_t extract_argument( llvm::CallInst *call )
        {
            auto args = call_args( call );
            check( args.size() <= 1 );
            return (args.size() == 0) ? inst_bits_node() : get( args[ 0 ] );
        }

        target_t VisitExtractIntrinsic( llvm::CallInst *call, llvm::Function *fn )
        {
            auto [ extract_from, size ] = irops::Extract::parse_args( fn );
            auto arg = extract_argument( call );

            // We split extract to sepratate bytes. This is so we can reorder them,
            // which can be handy if the extracted data are in a different order
            // (endiannity for example).
            const unsigned step = 8;
            std::deque< target_t  > partials;
            auto generate_fragments = [ & ]( uint32_t from, uint32_t to )
            {
                std::deque< Operation * > partials;
                while ( true )
                {
                    uint32_t y = std::min( from + ( step - from % step ), to );
                    auto op = impl->create< Extract >( from, y );
                    op->add_operand( arg );
                    partials.push_front( op );

                    if ( y == to )
                        return partials;
                    from = y;
                }
            };
            partials = generate_fragments(
                    static_cast< uint32_t >( extract_from ),
                    static_cast< uint32_t >( extract_from + size ) );

            if ( partials.size() == 1 )
            {
                // `Emplace` was not called, therefore manual assignement is needed.
                //val_to_op[ call ] = partials.front();
                return partials.front();
            }

            // x86 immediates are encoded using little-endian however instruction bytes
            // will be encoded differently:
            // ba 12 00 00 00 - mov 12, %rdx
            // If we do extract(32, 0) we end up with `12000000` as number, but we would
            // expect `00000012` therefore we must reorder them and then concat.
            auto full = emplace< Concat >( static_cast< uint32_t >( size ) );
            for ( auto x : partials )
                full->add_operand( x );
            return full;
        }

        target_t VisitExtractRawIntrinsic( llvm::CallInst *call, llvm::Function *fn )
        {
            auto arg = extract_argument( call );
            auto [ from, size ] = irops::ExtractRaw::parse_args( fn );
            auto op = emplace< Extract >( static_cast< uint32_t >( from ),
                                          static_cast< uint32_t >( from + size ) );

            auto args = call_args( call );
            if ( !args.empty() )
                op->add_operand( get( args[ 0 ] ) );
            else
                op->add_operand( arg );

            return op;
        }

        template< typename O, typename ... Args >
        target_t VisitGenericIntrinsic( llvm::CallInst *call, llvm::Function *fn,
                                        Args &&... args)
        {
            return emplace_full< O >( call_args( call ), std::forward< Args >( args ) ... );
        }

        template< typename T >
        auto get_make_io_leaf()
        {
            return [ = ]< typename ... Args >( Args && ... args )
            {
                return emplace< T >( std::forward< Args >( args ) ... );
            };
        }

        template< typename IT, typename OT, typename ... Args >
        auto VisitIOLeaf( llvm::CallInst *call, llvm::Function *fn,
                          uint32_t io_type, Args && ... args )
        {
            if ( io_type == irops::io_type::in )
                return get_or_emplace_cached< IT >( fn, std::forward< Args >( args ) ... );

            if ( io_type == irops::io_type::out )
                return get_or_emplace_cached< OT >( fn, std::forward< Args >( args ) ... );
            unreachable() << "Unreachable";
        }

        uint32_t value_size( llvm::Value *val )
        {
            return static_cast< uint32_t >( dl.getTypeSizeInBits( val->getType() ) );
        }

        target_t call_arg( llvm::CallInst *call, uint32_t idx )
        {
            return get( call->getArgOperand( idx ) );
        }

        target_t VisitLLVMIntrinsic( llvm::CallInst *call, llvm::Function *fn )
        {
            switch ( fn->getIntrinsicID() )
            {
                case llvm::Intrinsic::ctpop :
                    return VisitGenericIntrinsic< PopulationCount >( call, fn,
                                                                     value_size( call ) );
                // TODO( from-llvm ): Second argument is irrelevant for our use case?
                case llvm::Intrinsic::ctlz :
                {
                    auto out = emplace< CountLeadingZeroes >( value_size( call ) );
                    out->add_operands( get( call_args( call ), { 0u } ) );
                    return out;
                }
                // TODO( from-llvm ): Second argument is irrelevant for our use case?
                case llvm::Intrinsic::cttz :
                {
                    auto out = emplace< CountTrailingZeroes >( value_size( call ) );
                    out->add_operands( get( call_args( call ), { 0u } ) );
                    return out;
                }
                default:
                  unreachable() << "Unsupported intrinsic call: "
                                << remill::LLVMThingToString( call );
            }
        }

        target_t VisitIntrinsic( llvm::CallInst *call, llvm::Function *fn )
        {
            auto name = fn->getName();
            check( !name.startswith( "__remill_read_memory_" ) )
                << "__remill_read_memory_* should not be present!";
            check( !name.startswith( "__remill_write_memory_" ) )
                << "__remill_write_memory_* should not be present!";

            if ( name.startswith( "__remill_undefined_" ) )
                return emplace< Undefined >( size_from_suffix( name ) );

            if ( irops::OpSelector::is( fn ) )
            {
                auto [ size, _ ] = irops::OpSelector::parse_args( fn );
                return get_or_make_cached( fn, [ &, size = size ] {
                    return make_next_advice( size_arg( size ) );
                } );
            }

            if (irops::Reg::is(fn)) {
                auto [ size, reg, io_type ] = irops::Reg::parse_args(fn);
                return VisitIOLeaf< InputRegister, OutputRegister >(
                        call, fn, io_type, reg, static_cast< uint32_t >( size ) );
            }

            if (irops::Extract::is(fn)) {
                return VisitExtractIntrinsic(call, fn);
            }
            if (irops::ExtractRaw::is(fn)) {
                return VisitExtractRawIntrinsic(call, fn);
            }
            if (irops::InputImmediate::is(fn)) {
                auto [size] = irops::InputImmediate::parse_args(fn);
                return VisitGenericIntrinsic< InputImmediate >(call, fn, static_cast< uint32_t >(size));
            }

            if ( irops::Option::is( fn ) )
            {
                auto [ size ] = irops::Option::parse_args( fn );
                return VisitGenericIntrinsic< Option >( call, fn, static_cast< uint32_t >( size ) );
            }

            if ( irops::Switch::is( fn ) )
            {
                auto [ size ] = irops::Option::parse_args( fn );
                return VisitGenericIntrinsic< Switch >( call, fn, static_cast< uint32_t >( size ) );
            }


            if (irops::Xor::is(fn)) {
                return VisitGenericIntrinsic< OnlyOneCondition >(call, fn);
            }
            if (irops::Concat::is(fn)) {
                auto [size] = irops::Concat::parse_args(fn);
                return VisitGenericIntrinsic< Concat >(call, fn, static_cast< uint32_t >(size));
            }
            if (irops::Select::is(fn)) {
                // TODO( from-llvm ): Minimal tests did not catch these being swapped.
                auto [size, select_bits] = irops::Select::parse_args(fn);
                return VisitGenericIntrinsic< Select >(call, fn,
                        static_cast< uint32_t >(select_bits), static_cast< uint32_t >(size));
            }
            if (irops::OutputCheck::is(fn)) {
                return VisitGenericIntrinsic< RegConstraint >(call, fn);
            }
            if (irops::DecodeCondition::is(fn)) {
                return VisitGenericIntrinsic< DecodeCondition >(call, fn);
            }
            if (irops::VerifyInst::is(fn)) {
                return VisitGenericIntrinsic< VerifyInstruction >(call, fn);
            }
            if (irops::Advice::is(fn)) {
                auto [type] = irops::Advice::parse_args(fn);
                auto size = irops::impl::suffix::llvm_type::to_bw< uint32_t >(type);
                return VisitGenericIntrinsic< Advice >(call, fn, size, ++advice_idx);
            }
            if ( irops::Operand::is(fn)) {
                auto [type, _] = irops::Operand::parse_args(fn);
                auto size = irops::impl::suffix::llvm_type::to_bw< uint32_t >(type);
                return VisitGenericIntrinsic< Advice >(call, fn, static_cast< uint32_t >(size), ++advice_idx);
            }

            if ( irops::RegSelector::is(fn)) {
                auto [type, _] = irops::RegSelector::parse_args(fn);
                auto size = irops::impl::suffix::llvm_type::to_bw< uint32_t >(type);
                return VisitGenericIntrinsic< Advice >(call, fn, size, ++advice_idx);
            }

            if (irops::AdviceConstraint::is(fn)) {
                return VisitGenericIntrinsic< AdviceConstraint >(call, fn);
            }
            if (irops::Or::is(fn)) {
                // TODO( from-llvm ): Size should be specified by intrinsic.
                return VisitGenericIntrinsic< Or >(call, fn, 1u);
            }
            if (irops::DecoderResult::is(fn)) {
                return VisitGenericIntrinsic< DecoderResult >(call, fn);
            }
            if (irops::Memory::is(fn)) {
                auto [_, id] = irops::Memory::parse_args(fn);
                return VisitGenericIntrinsic< Memory >(call, fn,
                        irops::memory::size(impl->ptr_size), static_cast< uint32_t >(id));
            }
            if (irops::And::is(fn)) {
                // TODO( from-llvm ): Size should be specified by intrinsic.
                return VisitGenericIntrinsic< And >(call, fn, 1u);
            }
            if (irops::ReadConstraint::is(fn)) {
                return VisitGenericIntrinsic< ReadConstraint >(call, fn);
            }
            if (irops::WriteConstraint::is(fn)) {
                return VisitGenericIntrinsic< WriteConstraint >(call, fn);
            }
            if (irops::UnusedConstraint::is(fn)) {
                return VisitGenericIntrinsic< UnusedConstraint >(call, fn);
            }
            if (irops::ErrorBit::is(fn)) {
                auto [size, io_type] = irops::ErrorBit::parse_args(fn);
                return VisitIOLeaf< InputErrorFlag, OutputErrorFlag >(call, fn, io_type, static_cast< uint32_t >(size));
            }
            if (irops::Timestamp::is(fn)) {
                auto [size, io_type] = irops::Timestamp::parse_args(fn);
                return VisitIOLeaf< InputTimestamp, OutputTimestamp >(call, fn, io_type, static_cast< uint32_t >(size));
            }
            if (irops::InstBits::is(fn)) {
                auto [size, io_type] = irops::InstBits::parse_args(fn);
                check(io_type == irops::io_type::in);
                return get_or_emplace_cached< InputInstructionBits >( fn, size_arg( size ) );
            }

            if (irops::AdviceIndexed::is(fn)) {
                auto [size, idx] = irops::AdviceIndexed::parse_args(fn);
                auto ctor = [&, s = size]()
                {
                    return make_next_advice( size_arg( s ) );
                };
                return get_or_make_cached( fn, ctor );

            }

            if (irops::Entry::is(fn))
            {
                auto [type] = irops::Entry::parse_args(fn);
                auto size = irops::impl::suffix::llvm_type::to_bw< uint32_t >(type);

                auto args = call_args( call );
                check( args.size() == 1 );
                if ( size != value_size( args[ 0 ] ) )
                {
                    log_kill() << "Not implementede yet.";
                }
                return get( args[ 0 ] );
            }

            unreachable() << "Unsupported function: " << remill::LLVMThingToString(call);
        }

        // This function is responsible for binding some node to `val` inside `val_to_op`.
        target_t visit( llvm::CallInst *call )
        {
            auto func = call->getCalledFunction();
            check( func ) << "Cannot find called function used in call: "
                          << remill::LLVMThingToString( call );


            auto op = [ & ] {
                if ( func->getIntrinsicID() != llvm::Intrinsic::not_intrinsic )
                    return VisitLLVMIntrinsic( call, func );
                return VisitIntrinsic( call, func );
            }();

            return op;
        }

        target_t visit( llvm::CmpInst *cmp )
        {
            if ( has_undefined_ops( cmp ) )
                return emplace< Undefined >( value_size( cmp ) );

            auto mk = [ & ]< typename T >() -> target_t
            {
                return emplace_full< T >( operand_values( cmp ), value_size( cmp ) );
            };


            switch ( cmp->getPredicate() )
            {
                case llvm::CmpInst::ICMP_EQ:   return mk.operator()< Icmp_eq  >();
                case llvm::CmpInst::ICMP_NE:   return mk.operator()< Icmp_ne  >();
                case llvm::CmpInst::ICMP_ULT:  return mk.operator()< Icmp_ult >();
                case llvm::CmpInst::ICMP_SLT:  return mk.operator()< Icmp_slt >();
                case llvm::CmpInst::ICMP_UGT:  return mk.operator()< Icmp_ugt >();
                case llvm::CmpInst::ICMP_UGE:  return mk.operator()< Icmp_uge >();
                case llvm::CmpInst::ICMP_ULE:  return mk.operator()< Icmp_ule >();
                case llvm::CmpInst::ICMP_SGT:  return mk.operator()< Icmp_sgt >();
                case llvm::CmpInst::ICMP_SGE:  return mk.operator()< Icmp_sge >();
                case llvm::CmpInst::ICMP_SLE:  return mk.operator()< Icmp_sle >();
                default: unreachable() << "Cannot lower llvm predicate " << cmp->getPredicate();
            }
        }

        template< typename T >
        target_t maybe_undef( llvm::Instruction *inst )
        {
            if ( has_undefined_ops( inst ) )
                return emplace< Undefined >( value_size( inst ) );

            auto s = value_size( inst );
            return with_operands( emplace< T >( s ), inst );
        }

        target_t visit( llvm::ZExtInst *zext ) {  return maybe_undef< ZExt >( zext ); }
        target_t visit( llvm::SExtInst *sext ) {  return maybe_undef< SExt >( sext ); }
        target_t visit( llvm::TruncInst *trunc ) {  return maybe_undef< Trunc >( trunc ); }

        target_t visit( llvm::BinaryOperator *inst )
        {
            if ( has_undefined_ops( inst ) )
                return emplace< Undefined >( value_size( inst ) );

            auto s = value_size( inst );
            auto op_code = inst->getOpcode();

            auto mk = [ & ]< typename T >() -> target_t
            {
                auto raw = emplace< T >( s );
                return with_operands( raw, inst );
            };

            switch (op_code)
            {
                case llvm::BinaryOperator::Add: return mk.operator()< Add >();
                case llvm::BinaryOperator::Sub: return mk.operator()< Sub >();
                case llvm::BinaryOperator::Mul: return mk.operator()< Mul >();

                case llvm::BinaryOperator::UDiv: return mk.operator()< UDiv >();
                case llvm::BinaryOperator::SDiv: return mk.operator()< SDiv >();

                case llvm::BinaryOperator::And: return mk.operator()< And >();
                case llvm::BinaryOperator::Or:  return mk.operator()< Or >();
                case llvm::BinaryOperator::Xor: return mk.operator()< Xor >();

                case llvm::BinaryOperator::Shl:  return mk.operator()< Shl >();
                case llvm::BinaryOperator::LShr: return mk.operator()< LShr >();
                case llvm::BinaryOperator::AShr: return mk.operator()< AShr >();

                case llvm::BinaryOperator::URem: return mk.operator()< URem >();
                case llvm::BinaryOperator::SRem: return mk.operator()< SRem >();

                default :
                    unreachable() << "Cannot lower llvm inst: "
                                  << llvm::Instruction::getOpcodeName(op_code);
            }
        }

        target_t visit( llvm::SelectInst *sel )
        {
            if ( isa< Undefined >( get( sel->getCondition() ) ) )
                return emplace< Undefined >( value_size( sel ) );

            auto true_val = get( sel->getTrueValue() );
            auto false_val = get( sel->getFalseValue() );

            check( !isa< Undefined >( true_val ) || !isa< Undefined >( false_val ) );

            auto root = emplace< Select >( 1u, value_size( sel ) );
            root->add_operands( get( operand_values( sel ), { 0, 2, 1 } ) );

            return root;
        }

        bool has_undefined_ops( llvm::Instruction *inst )
        {
            for ( const auto &op : inst->operand_values() )
                if ( get( op )->op_code == Undefined::kind )
                    return true;
            return false;
        }

        target_t visit_apint( llvm::Constant *val, llvm::APInt ap_val )
        {
            auto num_bits = value_size( val );
            llvm::SmallString< 64 > val_bits;

            val_bits.reserve( num_bits );
            ap_val.toStringUnsigned( val_bits, 2 );

            while ( val_bits.size() < num_bits )
                val_bits.insert( val_bits.begin(), '0' );

            std::reverse( val_bits.begin(), val_bits.end() );
            auto bits_str = val_bits.str().str();

            auto &bits_op = bits_to_constants[ bits_str ];
            if ( !bits_op )
            {
                check(num_bits == bits_str.size());
                bits_op = emplace< Constant >( std::move( bits_str ),
                                               static_cast< unsigned >( num_bits ) );
            }
            return bits_op;
        }

        target_t visit( llvm::UndefValue *val )
        {
            auto num_bits = static_cast< uint32_t >( dl.getTypeSizeInBits( val->getType() ) );
            return emplace< Undefined >( num_bits );
        }

        target_t visit( llvm::ConstantInt *val )
        {
            return visit_apint(val, val->getValue());
        }

        target_t visit( llvm::ConstantFP *val )
        {
            check( false ) << "Floats are not yet supported.";
            //VisitAPInt(val, val->getValueAPF().bitcastToAPInt());
            return nullptr;
        }

        /* Pre-process */

        // 1) Lower all constant expression to the beginning of the function as
        //    instruction.
        // 2) [optional] Remove all code that is unreachable from the return value -
        //    this way we do not lower stuff we do not need. Can be instead just lowered
        //    and then purged from the circuit.
        self_t &prepare_fn()
        {
            lower_ces( circuit_fn );
            remove_dbg( block() );

            // TODO( from-llvm ): Purge all dead code ( can happen that LLVM won't remove
            //                    dead code if it calls frozen intrinsic.
            return *this;
        }


        /* Random conjurers. */

        self_t &conjure_instbits( uint32_t size )
        {
            std::ignore = emplace< InputInstructionBits >( size );
            return *this;
        }

        // TODO( from-llvm ): Make deprecated once lifter v2 is available.
        self_t &lower_args()
        {
            for ( auto &arg : circuit_fn->args() )
            {
                auto size = value_size( &arg );

                auto trg = [ & ]()
                {
                    if ( auto out_name = circuit_builder::is_output_reg( &arg ) )
                        return emplace< OutputRegister >( *out_name, size );

                    if ( auto in_name = circuit_builder::is_input_reg( &arg ) )
                        return emplace< InputRegister >( *in_name, size );

                    log_kill() << "Unrecognized argument:" << dbg_dump( &arg );
                }();

                record( &arg, with_src_metadata( trg, &arg ) );
            }
            return *this;
        }

        self_t &lower_constants()
        {
            for ( auto c : constants_in( block() ) )
            {
                if ( !is_lowered( c ) )
                    lower( c );
            }
            return *this;
        }

        self_t &lower_insts()
        {
            for ( auto &inst : block() )
                lower( &inst );

            return *this;
        }

        // TODO( from-llvm ): Deprecate this?
        // Create a root operation for the circuit as a `AND` of all verify instructions.
        self_t &conjure_root()
        {
            impl->root = impl->create< OnlyOneCondition >();

            auto visit_context = [ & ]( llvm::CallInst *ctx )
            {
                auto lowered_ctx = get( ctx );
                check( isa< VerifyInstruction >( lowered_ctx ) );
                impl->root->add_operand( lowered_ctx );
            };
            irops::VerifyInst::for_all_in( circuit_fn, visit_context );
            return *this;
        }

        self_t &make_root()
        {
            auto ret = block().getTerminator();
            if ( !ret )
                return conjure_root();

            check( llvm::isa< llvm::ReturnInst >( ret ) );

            impl->root = get( ret );
            return *this;
        }

        // Since some intrinsic calls represent only one value we do not want to
        // emit more nodes (there are no gurantees that LLVM will always eliminate
        // them to only one value).

        bool is_cached( llvm::Function *fn )
        {
            return leaves.count( fn );
        }

        std::optional< target_t > get_cached( llvm::Function *fn )
        {
            if ( !is_cached( fn ) )
                return {};
            return { leaves[ fn ] };
        }

        template< typename Ctor >
        target_t get_or_make_cached( llvm::Function *fn, Ctor &&ctor )
        {
            if ( auto op = get_cached( fn ) )
                return *op;
            auto val = ctor();
            leaves[ fn ] = val;
            return val;
        }

        template< typename T, typename ... Args >
        target_t get_or_emplace_cached( llvm::Function *fn, Args && ... args )
        {
            auto mk = [ & ]
            {
                return emplace< T >( std::forward< Args >( args ) ... );
            };
            return get_or_make_cached( fn, mk );
        }

        // Raw creation of nodes

        template< typename T >
        target_t with_operands( T target, llvm::Instruction *inst )
        {
            for ( const auto &op : inst->operand_values() )
                target->add_operand( get( op ) );
            return target;
        }

        template< typename T >
        target_t with_operands( T target, llvm::CallInst *v )
        {
            target->add_operands( get( call_args( v ) ) );
            return target;
        }

        // Helper
        template< typename H, typename ... Args >
        constexpr static bool no_head_range()
        {
            return !gap::ranges::range< H >;
        }

        template< typename T, typename ... Args >
        target_t emplace( Args && ... args )
        {
            return impl->create< T >( std::forward< Args >( args ) ... );
        }

        template< typename T, gap::ranges::range R, typename ... Args >
        target_t emplace_full( R &&operands, Args && ... args )
        {
            auto trg = emplace< T >( std::forward< Args >( args ) ... );
            trg->add_operands( get( std::forward< R >( operands ) ) );
            return trg;
        }

        template< typename ... Args >
        target_t make_next_advice( Args && ... args )
        {
            return emplace< Advice >( std::forward< Args >( args ) ..., ++advice_idx );
        }

        // Top level api - modifies/queries the current state.

        template< gap::ranges::range R, bool allow_failure = false >
        auto get( R &&from ) const
            -> gap::generator< target_t >
        {
            for ( auto v : from )
                co_yield get< allow_failure >( v );
        }

        template< bool allow_failure = false >
        target_t get( source_t key ) const
        {
            auto it = val_to_op.find( key );
            if constexpr ( !allow_failure )
            {
                check( it != val_to_op.end() && it->second );
            }
            return ( it != val_to_op.end() ) ? it->second : nullptr;
        }

        // TODO( from-llvm ): Wrap into a template?
        template< gap::ranges::range R, bool allow_failure = false >
        auto get( R &&source, std::vector< std::size_t > idxs )
            -> gap::generator< target_t >
        {
            auto frozen = freeze< std::vector >( std::forward< R >( source ) );

            for ( auto i : idxs )
            {
                check( i < frozen.size() ) << "Index out of bounds.";
                co_yield get< allow_failure >( frozen[ i ] );
            }
        }

        target_t dispatch( source_t v )
        {
            return this->parent_t::dispatch( v );
        }

        target_t lower( source_t v )
        {
            // TODO( from-llvm ): Figure how to properly log this.
            //log_dbg() << "[ from-llvm ]:" << "lower(" << dbg_dump( v ) << ")";
            check( !is_lowered( v ), [ & ]() { return dbg_dump( v ); } );
            auto op = with_src_metadata( dispatch( v ), v );
            return record( v, op );
        }

        target_t with_src_metadata( target_t op, source_t val )
        {
            std::stringstream ss;
            ss << "[ " << op->id() << " ]: " << dbg_dump( val );

            auto append = []( auto a, auto b ) { return a + "\n" + b; };

            op->set_or_append_meta( circir_llvm_meta::llvm_source_dump, ss.str(), append );
            return op;
        }

        bool is_lowered( source_t v ) const { return val_to_op.count( v ); }

        target_t record( source_t v, target_t op )
        {
            // TODO( from-llvm ): Figure how to properly log this.
            //log_dbg() << "[ from-llvm ]: Rcord(" << dbg_dump( v ) << "," << op->id() << ")";
            val_to_op[ v ] = op;
            return op;
        }

        /* Circuit function helpers */

        llvm::BasicBlock &block()
        {
            check( circuit_fn->size() == 1 );
            return *circuit_fn->begin();
        }


        const llvm::DataLayout &dl;
        llvm::Function *circuit_fn = nullptr;
        Circuit *impl = nullptr;

        std::size_t advice_idx = 0;

        std::unordered_map< llvm::Value *, Operation * > val_to_op;
        std::unordered_map< llvm::Value *, Operation * > leaves;

        std::unordered_map< std::string, Operation * > bits_to_constants;

        std::chrono::time_point< std::chrono::steady_clock > clock =
            std::chrono::steady_clock::now();
      private:
        IRImporter() = delete;
    };

    void clear_names(llvm::Function *fn)
    {
        for (auto &bb : *fn)
            for (auto &inst : bb)
                inst.setName("");
    }

}  // namespace


circuit_owner_t lower_fn( llvm::Function *circuit_fn,
                          std::size_t ptr_size )
{
    check( circuit_fn ) << "( nullptr ) passed to lower_fn.";
    // Simply to improve human debugging.
    clear_names( circuit_fn );

    const auto module = circuit_fn->getParent();
    const auto &dl = module->getDataLayout();

    log_info() << "IRImpoter starting.";
    auto impl = std::make_unique<Circuit>(ptr_size);
    IRImporter importer( dl, impl.get(), circuit_fn );

    importer.prepare_fn()
            .conjure_instbits( kMaxNumInstBits )
            .lower_constants()
            .lower_args()
            .lower_insts()
            .make_root();

    log_info() << "IRImpoter done.";

    VerifyCircuit("Lowered llvm circuit.", impl.get(), "Lowered circuit is valid.");
    auto dce_count = impl->remove_unused();
    log_dbg() << "Eliminated" << dce_count << "unused nodes";
    return impl;
}

circuit_owner_t lower_module(llvm::Module *lmodule,
                                    std::size_t ptr_size)
{
    for ( auto &fn : *lmodule )
        if ( !fn.isDeclaration() )
            return lower_fn( &fn, ptr_size );
    return {};
}

}  // namespace circ
