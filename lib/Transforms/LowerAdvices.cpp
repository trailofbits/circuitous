/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/Passes.hpp>

#include <circuitous/IR/Shapes.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Support/Log.hpp>

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <vector>
#include <chrono>

namespace circ
{
    void lower( circuit_ref_t circuit, Advice *advice )
    {
        // Collect conditions for each value.
        using conds_t = std::vector< Operation * >;
        using op_t = Operation *;

        std::unordered_map< op_t, conds_t > val_to_ctx;
        for ( auto ac : filter< AdviceConstraint >( advice->users() ) )
        {
            auto val = ac->value();
            auto &mapping = val_to_ctx[ val ];

            for ( auto cond : ac->ctx_conds() )
                mapping.emplace_back( cond );
        }

        //auto ordered = std::vector< std::pair< op_t, conds_t >( val_to_ctx.begin(),
        //                                                        val_to_ctx.end() );
        //auto cmp = []( const auto &lhs,  const auto &rhs )
        //{
        //    return lhs.second.size() < rhs.second.size();
        //};

        //std::sort( ordered.begin(), order.end(), cmp );

        auto s = circuit->create< Switch >( advice->size );

        for ( auto &[ val, conds ] : val_to_ctx )
        {
            auto option = circuit->create< Option >( val->size );
            option->add_operand( val );
            option->add_operands( conds );

            s->add_operands( option );
        }

        advice->replace_all_uses_with( s );
    }

    circuit_owner_t LowerAdvices::run( circuit_owner_t &&circuit )
    {
        for ( auto advice : circuit->attr< Advice >() )
            lower( circuit.get(), advice );

        auto is = []< typename T >() { return [&]( auto op ){ return isa< T >( op ); }; };
        auto removed_advices = circuit->remove_if( is.operator()< Advice >() );
        auto removed_acs = circuit->remove_if( is.operator()< AdviceConstraint >() );
        auto removed_unused = circuit->remove_unused();

        log_dbg() << "Purged:\n"
                  << "  * Advices:" << removed_advices << "\n"
                  << "  * AdviceConstraint " << removed_acs << "\n"
                  << "  * Dead nodes: " << removed_unused;
        return circuit;

    }
}  // namespace circ
