/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/Circuit.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/CircuitBuilder.hpp>
#include <circuitous/Lifter/CircuitSmithy.hpp>
#include <circuitous/Lifter/Lifter.hpp>
#include <circuitous/Lifter/LLVMToCircIR.hpp>

namespace circ
{

    CircuitSmithy::CircuitSmithy(const std::string &arch_name, const std::string &os_name)
        : ctx(arch_name, os_name), batch(ctx)
    {}

    CircuitSmithy::CircuitSmithy(Ctx ctx_) : ctx(std::move(ctx_)), batch(ctx) {}

    auto CircuitSmithy::smelt(const std::vector< InstBytes > &insts) -> self_t &
    {
        auto decoder = Decoder(ctx);
        std::vector< remill::Instruction > rinsts;
        for (const auto &x : insts)
        {
            auto maybe_inst = decoder.decode(x);
            check(maybe_inst) << "Decoder failed on:" << x.as_hex_str();
            rinsts.push_back(std::move(*maybe_inst));
        }
        return smelt(std::move(rinsts));
    }

    auto CircuitSmithy::smelt(std::string_view raw_bytes) -> self_t &
    {
        return smelt(Decoder(ctx).decode_all(raw_bytes));
    }

    auto CircuitSmithy::smelt(std::vector< remill::Instruction > &&rinsts) -> self_t &
    {
        batch.add(std::move(rinsts));
        return *this;
    }

    auto CircuitSmithy::forge() -> circuit_ptr_t
    {
        check(!batch->empty()) << "No valid instructions provided, cannot produce circuit.";

        batch.fuzz()
             .lift< ILifter< OpaqueILifter > >();
        return lower_fn(CircuitMaker(ctx).make_from(std::move(batch)),
                        ctx.ptr_size);
    }


    /* v2 */

    // Group by ISEL
    auto CircuitSmithy_v2::categorize( atoms_t atoms ) -> worklist_t
    {
        std::unordered_map< isel_t, atoms_t > groups;

        for ( auto &atom : std::move( atoms ) )
        {
            auto isel = atom.isel();
            groups[ isel ].push_back( std::move( atom ) );
        }

        worklist_t out;
        for ( auto &[ isel, atom ] : std::move( groups ) )
            out.emplace( isel, std::move( atom ) );

        return out;
    }

    auto CircuitSmithy_v2::purify( const std::vector< InstBytes > &insts ) -> concretes_t
    {
        return freeze< std::vector >( decode_all( ctx, insts ) );
    }

    auto CircuitSmithy_v2::purify( std::string_view raw_bytes ) -> concretes_t
    {
        return decode_all( ctx, raw_bytes );
    }

    auto CircuitSmithy_v2::smelt ( concretes_t &&concretes ) -> atoms_t
    {
        auto dsts = []( auto c )
        {
            std::size_t got = 0;
            for ( auto x : c.operands )
                if ( x.action == remill::Operand::kActionWrite )
                    ++got;
            if ( got > 1 )
                log_info() << "[REPORT]:" << c.Serialize();
        };

        for ( auto c : concretes )
        {
            dsts( c );
        }

        atoms_t out;
        for ( auto concrete : std::move( concretes ) )
        {
            auto abstract = fuzz_operands( *ctx.arch(), concrete );
            out.emplace_back( std::move( concrete ), std::move( abstract ) );
        }

        for ( auto &atom : out )
            atom.abstract.distribute_selectors();

        return out;
    }

    auto CircuitSmithy_v2::forge( atoms_t &&atoms ) -> circuit_ptr_t
    {
        auto circuit_fn = CircuitFunction_v2( ctx );
        auto worklist = categorize( std::move( atoms ) );

        log_info() << "[smithy]:" << "Worklist contains:" << worklist.size() << "entries!";

        auto exalt_context = ExaltationContext( ctx, circuit_fn );
        for ( auto &unit : worklist )
            exalt_context.exalt( unit );

        exalt_context.finalize();

        return lower_fn( &*circuit_fn, ctx.ptr_size );
    }

} // namespace circ
