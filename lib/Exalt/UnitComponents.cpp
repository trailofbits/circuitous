/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/UnitComponents.hpp>

#include <circuitous/Lifter/Memory.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS


namespace circ
{
    exalted_values_t memory_checks::after_isem( unit_t &unit, isem_range_t isem )
    {
        auto [ begin, end ] = isem;
        uint32_t ptr_size = b_ctx.ctx.ptr_size;
        auto mem_checks = mem::synthetize_memory( begin, end, ptr_size );
        // TODO( exalt ): Do we need to fixup the `irb`?
        return wrap_as_exalted( mem_checks, place::ctx );
    }

    exalted_values_t error_bit::after_isem( unit_t &unit, isem_range_t isem )
    {
        return {};
    }

    void timestamp::init()
    {
        // TODO( exalt ): Ideally we would want to be really only once in the circuit
        //                (including the `AND` with the rest of conditions).
        auto &bld = b_ctx.irb();
        auto [ ts_in, ts_out ] = irops::make_all_leaves< irops::Timestamp >( bld );
        auto runtime_ts = bld.CreateAdd( ts_in, bld.getInt64( 1 ) );
        auto oc = irops::OutputCheck::make( bld, { runtime_ts, ts_out } );
        checks.emplace_back( place::ctx, oc );
    }

    auto timestamp::after_isem( unit_t &, isem_range_t ) -> exalted_values_t
    {
        return checks;
    }
}  // namespace circ
