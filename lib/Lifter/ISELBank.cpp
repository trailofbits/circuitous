/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/ISELBank.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ::isem
{

    // Move to utils
    llvm::Value *state_arg( llvm::Function &fn )
    {
        return remill::NthArgument( &fn, remill::kStatePointerArgNum );
    }

    llvm::Value *memory_arg( llvm::Function &fn )
    {
        return remill::NthArgument( &fn, remill::kMemoryPointerArgNum );
    }

    // This is hidden on purpose for now as there is no customization mechanism.
    // If a need to override/extend this lifter ever arises, pull it into header,
    // there is no fundamental issue with that.
    namespace
    {
        struct AdviceLifter
        {
            using isem_def_t = typename ISemBank::isem_def_t;
            using arg_blueprint_t = typename ISem::arg_blueprint_t;

          private:

            llvm::IRBuilder<> &irb;

            llvm::Function *into;
            llvm::Function *sem;
            isem_def_t current;

            std::vector< llvm::Value * > call_args;

            AdviceLifter( llvm::IRBuilder<> &irb,
                          llvm::Function *into,
                          llvm::Function *sem )
                : irb( irb ), into( into ), sem( sem ),
                  current( std::make_shared< ISem >() )
            {}

            auto make_arg( llvm::Value *arg, std::size_t idx )
                -> arg_blueprint_t
            {
                if ( auto ptr_type = llvm::dyn_cast< llvm::PointerType >( arg->getType() ) )
                {
                    auto raw_type = ptr_type->getPointerElementType();
                    auto val = irops::make_leaf< irops::ISemDstArg >( irb, raw_type, idx );
                    auto alloca = irops::make_leaf< irops::AllocateDst >( irb, ptr_type );
                    irb.CreateStore( val, alloca );

                    call_args.emplace_back( alloca );

                    current->dsts.push_back( val );
                    return { false, raw_type, idx };
                }

                // Must be a scalar type, but for now we can only handle integers.
                check( llvm::dyn_cast< llvm::IntegerType >( arg->getType() ) );

                auto val = irops::make_leaf< irops::ISemSrcArg >( irb, arg->getType(), idx );
                call_args.emplace_back( val );
                current->srcs.push_back( val );
                return { false, arg->getType(), idx };
            }

            void make_prologue()
            {
                log_info() << "[isem]: Making prologue";
                // This order is significant as all the builder methods are
                // stateful.
                call_args.emplace_back( memory_arg( *into ) );
                call_args.emplace_back( state_arg( *into ) );

                log_info() << "[isem]: Making args";
                for ( std::size_t i = 2u; i < sem->arg_size(); ++i )
                {
                    auto v = make_arg( sem->getArg( static_cast< unsigned int >( i ) ), i - 2 );
                    current->args.push_back( v );
                }
            }


            void make_call()
            {
                std::ignore = irb;
                std::ignore = sem;

                log_info() << "[isem]: Making call";

                enable_opts( sem );
                auto call = irb.CreateCall( sem, call_args );
                irb.CreateRet( call );

                sem->print( llvm::errs() );
                llvm::InlineFunctionInfo info;
                auto was_inlined = llvm::InlineFunction( *call, info );
                check( was_inlined.isSuccess() );

            }

            isem_def_t make() &&
            {
                make_prologue();
                make_call();
                return std::move( current );
            }

          public:

            static isem_def_t lift( llvm::Function &into, llvm::Function &sem )
            {
                check( !into.isDeclaration() );
                llvm::IRBuilder<> irb( &*into.begin() );
                auto out = AdviceLifter( irb, &into, &sem ).make();
                out->_self = &into;
                out->_original = &sem;

                post_lift( into );

                log_info() << "[isem]: Created:";
                into.print( llvm::errs() );
                return out;
            }
        };


    } // namespace

    auto semantic_fns( llvm::Module &in )
        -> gap::generator< llvm::Function * >
    {
        for ( auto &func : in )
            if ( remill::HasOriginType< remill::Semantics >( &func ) )
                co_yield &func;
    }

    auto isel_map( llvm::Module &in )
        -> std::unordered_map< std::string, llvm::Function * >
    {
        std::unordered_map< std::string, llvm::Function * > out;

        for ( auto &gv : in.getGlobalList() )
        {
            if ( !gv.hasName() )
                continue;

            auto name = gv.getName();
            if ( !name.startswith( "ISEL_" ) && !name.startswith( "COND_" ) )
                continue;

            check( gv.hasInitializer() );

            auto init = gv.getInitializer()->stripPointerCasts();
            auto sem_fn = llvm::dyn_cast< llvm::Function >( init );
            check( sem_fn );

            out[ name.str() ] = sem_fn;
        }

        return out;
    }

    auto semantic_fn( const std::string &isel_name, llvm::Module &in )
        -> std::optional< llvm::Function * >
    {
        log_info() << "[isem]: Fetching semantics for" << isel_name;
        auto mapping = isel_map( in );
        if ( auto it = mapping.find( "ISEL_" + isel_name ); it != mapping.end() )
            return { it->second };
        return {};
    }

    llvm::Instruction *ISem::reconstruct_arg( llvm::IRBuilder<> &irb,
                                              const arg_blueprint_t &bp )
    {
        auto [ is_read, type, idx ] = bp;
        if ( is_read )
            return irops::make_leaf< irops::ISemSrcArg >( irb, type, idx );
        return irops::make_leaf< irops::ISemDstArg >( irb, type, idx );
    }

    auto ISemBank::cached( const std::string &isel_name )
        -> isem_def_t
    {
        auto lookup = [ & ]( auto name ) -> isem_def_t
        {
            auto it = cache.find( name );
            if ( it != cache.end() )
                return it->second;
            return {};
        };

        if ( auto exact_match = lookup( isel_name ) )
            return exact_match;

        return lookup( "ISEL_" + isel_name );

    }

    auto ISemBank::lift( const std::string &isel_name, llvm::Module &from,
                         llvm::Function &into )
        -> isem_def_t
    {
        auto fn = semantic_fn( isel_name, from );
        check( fn );
        return AdviceLifter::lift( into, **fn );
    }

    auto ISemBank::make( const std::string &isel_name, llvm::Module &from )
        -> isem_ref_t
    {
        if ( auto def = cached( isel_name ) )
            return def.get();

        auto fn = ctx.arch()->DeclareLiftedFunction( make_name( isel_name ), ctx.module() );
        ctx.arch()->InitializeEmptyLiftedFunction( fn );

        check( fn );
        auto [ it, _ ] = cache.emplace( isel_name, lift( isel_name, from, *fn ) );
        return it->second.get();
    }


} // namespace circ::isem
