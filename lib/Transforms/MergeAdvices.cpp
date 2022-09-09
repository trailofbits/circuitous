/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/MergeAdvices.hpp>

#include <circuitous/IR/Shapes.hpp>

#include <circuitous/Support/Log.hpp>

#include <map>
#include <set>
#include <string>
#include <sstream>
#include <vector>

namespace circ
{
    // TODO(lukas): Use this as replacement of runtime_find.
    template< typename Match, typename Yield , typename H, typename ... Tail >
    auto match_on( Match &&match, Yield &&yield )
    {
        if ( match.template operator()< H >() )
            return yield.template operator()< H >();
        if constexpr ( sizeof ... (Tail) == 0 )
            log_kill() << "Was not able to resolve match_on!";
        else
            match_on< Match, Yield, Tail ... >( std::forward< Match >( match ),
                                                std::forward< Yield >( yield ) );
    }

    template< typename Match, typename Yield, typename ... Es >
    auto match_on( Match &&match, Yield &&yield, tl::TL< Es ... > )
    {
        return match_on< Match, Yield, Es ... >( std::forward< Match >( match ),
                                                 std::forward< Yield >( yield ) );
    }


    template< typename Op >
    struct MergeWithAdvice
    {
        Circuit *circuit;
        std::size_t advice_idx = 0;

        MergeWithAdvice( Circuit *circuit ) : circuit( circuit )
        {
            for ( auto a : circuit->attr< Advice >() )
                advice_idx = std::max< std::size_t >( a->advice_idx, advice_idx );
        }

        void run()
        {
            auto ctxs = collect_ops();
            //dbg( ctxs );
            auto bp_pool = generate_bps( ctxs );
            //log_dbg() << "Generated pool of size: " << bp_pool.size();
            assign( bp_pool, ctxs );

            for ( auto add : circuit->attr< Op >() )
                for ( auto op : add->operands() )
                    check( isa< Advice >( op ) ) << pretty_print( add );
        }

        inline static auto order_by_size = []( const Operation *lhs, const Operation *rhs )
        {
            return lhs->size > rhs->size;
        };
        using ordered_ops_t = std::multiset< Op *, decltype( order_by_size ) >;

        struct Collected
        {
            Operation *ctx;
            ordered_ops_t ops;

            Collected( Operation *ctx ) : ctx( ctx ) {}
            Collected( Operation *ctx, ordered_ops_t ops )
                : ctx( ctx ), ops( std::move( ops ) )
            {}

            Collected( const Collected & ) = delete;
            Collected &operator=( const Collected & ) = delete;

            bool operator==( const Collected &other ) const
            {
                return ctx == other.ctx;
            }

            uint32_t required_size() const
            {
                uint32_t out = 0;
                for ( auto op : ops )
                    out = std::max< uint32_t >( out, op->size );
                return out;
            }

            bool operator!=( const Collected & ) const = default;

            std::string to_string() const
            {
                std::stringstream os;
                os << "Collected of : " << pretty_print< false >( ctx ) << "\n";
                os << "[\n";
                for ( auto op : ops )
                    os << "\t" << op->size << ": " << pretty_print< false >( op ) << "\n";
                os << "]\n";
                return os.str();
            }
        };

        inline static auto order_by_ops_count = []( const Collected &a, const Collected &b )
        {
            return a.ops.size() > b.ops.size();
        };

        using collected_set_t = std::multiset< Collected, decltype( order_by_ops_count ) >;

        void dbg( const collected_set_t &cs )
        {
            log_dbg() << "std::multiset< Collected > contains:";
            for ( const auto &c : cs )
                log_dbg() << c.to_string();
            log_dbg() << "EOL";
        }

        using blueprint_t = Op *;
        using blueprint_pool_t = std::vector< blueprint_t >;
        using blueprint_handle_t = std::size_t;

        blueprint_t mk_bp( uint32_t size )
        {
            auto x = circuit->create< Op >( size );
            for ( std::size_t i = 0; i < 2; ++i )
                x->add_operand( circuit->create< Advice >( size, ++advice_idx ) );
            return x;
        }

        blueprint_pool_t generate_bps( const collected_set_t &cs )
        {
            // We know they are oredered by the size of their operands to be replaced.
            if ( cs.empty() )
                return {};

            uint32_t size = 0;
            for ( const auto &c : cs )
                size = std::max< uint32_t >( size, c.required_size() );
            if ( size == 0 )
                return {};

            blueprint_pool_t pool;
            for ( std::size_t i = 0; i < cs.begin()->ops.size(); ++i )
                pool.emplace_back( mk_bp( size ) );
            return pool;
        }

        void emit_constraints( Operation *ctx, Operation *op, blueprint_t bp )
        {
            for ( std::size_t i = 0; i < op->operands_size(); ++i)
            {
                auto coerced = [ & ]( Operation *concrete, Operation *advice ) -> Operation *
                {
                    if ( concrete->size == advice->size )
                        return concrete;
                    check( concrete->size < advice->size );

                    auto sext = circuit->create< SExt >( advice->size );
                    sext->add_operand( concrete );
                    return sext;
                }( op->operand( i ), bp->operand( i ) );


                auto ac = circuit->create< AdviceConstraint >();
                ac->add_operand( coerced );
                ac->add_operand( bp->operand( i ) );
                ctx->add_operand( ac );
            }
        }

        void assign( const blueprint_pool_t &pool, const collected_set_t &cs )
        {
            using ops_t = std::unordered_set< Operation * >;
            using used_bp_t = std::unordered_set< blueprint_t >;

            // Context -> all its operations that are yet to be replaced.
            std::unordered_map< Operation *, ops_t > states;
            // Context -> blueprints that are not available to it.
            std::unordered_map< Operation *, used_bp_t > global_usages;

            for ( const auto &c : cs )
                states.emplace( c.ctx, ops_t{ c.ops.begin(), c.ops.end() } );


            for ( const auto &c : cs )
            {
                // Fetch all that are already unavaible - taking a ref so newly used
                // ones can be added.
                auto &used = global_usages[ c.ctx ];

                // Compute all matches - replacement is done after to avoid some container
                // invalidations.
                std::map< Operation *, blueprint_t > matched;
                for ( auto op : states[ c.ctx ] )
                {
                    auto bp = [&]()
                    {
                        for ( auto bp : pool )
                            if ( !used.count( bp ) )
                                return bp;
                        log_kill() << "Not enough blueprints!";
                    }();

                    used.insert( bp );
                    matched.emplace( op, bp );
                }

                // Emit constraints and update local structures with computed results.
                for ( auto &[ ctx, todo ] : states )
                {
                    // Process matches one by one
                    for ( auto &[ op, bp ] : matched )
                    {
                        // Skip as there is nothing to be done.
                        if ( !todo.count( op ) )
                            continue;

                        todo.erase( op );
                        emit_constraints( ctx, op, bp );

                        // Add information about this context already using this `bp`.
                        global_usages[ ctx ].insert( bp );

                    }
                }

                // Now replace operation with its blueprint - constraints are already in place.
                for ( auto &[ op_, bp_ ] : matched )
                {
                    auto coerced = [ &, op = op_, bp = bp_ ]() -> Operation *
                    {
                        if ( bp->size == op->size )
                            return bp;
                        check( bp->size > op->size );
                        auto trunc = circuit->create< Trunc >( op->size );
                        trunc->add_operand( bp );
                        return trunc;
                    }();
                    op_->replace_all_uses_with( coerced );
                }
            }
            circuit->remove_unused();
        }



        collected_set_t collect_ops()
        {
            std::map< Operation *, ordered_ops_t > tmp;

            CtxCollector ctx_info;
            ctx_info.Run( circuit );

            for ( auto op : circuit->attr< Op >() )
                for ( auto ctx : ctx_info.op_to_ctxs[ op ] )
                    tmp[ ctx ].insert( op );

            collected_set_t out;
            for ( auto &&[ ctx, ops ] : tmp )
                out.emplace( ctx, std::move( ops ) );

            return out;
        }
    };

    using allowed_ops_ts = tl::TL< Mul, Add, Sub >;


    auto merge_with_advice( Circuit::circuit_ptr_t &&circuit,
                            const std::vector< Operation::kind_t > &kinds )
        -> Circuit::circuit_ptr_t
    {
        auto merge_one = [&]< typename Op >()
        {
            MergeWithAdvice< Op >{ circuit.get() }.run();
        };

        for ( auto kind : kinds )
        {
            auto matches = [&]< typename Op >(){ return isa< Op >( kind ); };
            match_on( std::move( matches ), merge_one, allowed_ops_ts{} );
        }

        return circuit;
    }

}  // namespace circ
