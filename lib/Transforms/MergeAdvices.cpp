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


    template< typename Op, template< typename > class Config >
    struct MergeWithAdvice
    {
        using config = Config< Op >;

        Circuit *circuit;
        std::size_t advice_idx = 0;

        MergeWithAdvice( Circuit *circuit ) : circuit( circuit )
        {
            for ( auto a : circuit->attr< Advice >() )
                advice_idx = std::max< std::size_t >( a->advice_idx, advice_idx );
            advice_idx += 1;
        }

        void run()
        {
            do_assign( C::collect( circuit ) );
            circuit->remove_unused();
        }

        inline static auto order_by_size = []( const Operation *lhs, const Operation *rhs )
        {
            if ( lhs->size == rhs->size )
                return lhs->id() > rhs->id();
            return lhs->size > rhs->size;
        };

        struct C
        {
            Operation *ctx;

            using ops_bucket_t = std::set< Op *, decltype( order_by_size ) >;
            std::map< uint32_t, ops_bucket_t > buckets;

            std::string to_string() const
            {
                std::stringstream os;
                os << "Collected of : " << pretty_print< false >( ctx ) << "\n";
                for ( const auto &[ size, bucket ] : buckets )
                {
                    os << "[ " << size << " ]\n";
                    for ( auto op : bucket )
                        os << "\t | " << pretty_print< false >( op ) << "\n";
                }
                return os.str();
            }

            void add( Op *op )
            {
                buckets[ op->size ].emplace( op );
            }

            // ctx -> C
            using CS = std::unordered_map< Operation *, C >;
            static CS collect( Circuit *circuit )
            {
                CS out;

                CtxCollector ctx_info;
                ctx_info.Run( circuit );

                for ( auto op : circuit->attr< Op >() )
                    for ( auto ctx : ctx_info.op_to_ctxs[ op ] )
                        if ( config::replace( op ) )
                        {
                            out[ ctx ].add( op );
                            out[ ctx ].ctx = ctx;
                        }
                return out;
            }
        };

        static inline auto buckets_by_size = []( const auto &a, const auto &b )
        {
            return std::get< 1 >( a ).size() > std::get< 1 >( b ).size();
        };

        using ordered_buckets_t = std::unordered_map< Operation *,
                                                      typename C::ops_bucket_t >;
        ordered_buckets_t fetch_buckets( typename C::CS &cs, uint32_t size )
        {
            ordered_buckets_t out;
            for ( auto &[ ctx, c ] : cs )
                out.emplace( ctx, c.buckets[ size ] );
            return out;

        }

        using blueprint_t = Op *;

        struct BlueprintPool
        {
            using pool_t = std::vector< blueprint_t >;
            std::map< uint32_t, std::vector< blueprint_t > > pools;

            std::size_t advice_idx = 0;
            Circuit *circuit;

            BlueprintPool( Circuit *circuit ) : circuit( circuit )
            {
                for ( auto a : circuit->attr< Advice >() )
                    advice_idx = std::max< std::size_t >( a->advice_idx, advice_idx );
                advice_idx += 1;
            }

            blueprint_t emplace( uint32_t size, std::size_t arity )
            {
                auto x = circuit->create< Op >( size );
                for ( std::size_t i = 0; i < arity; ++i )
                    x->add_operand( circuit->create< Advice >( size, ++advice_idx ) );
                return pools[ size ].emplace_back( x );
            }

            pool_t &pool( uint32_t size )
            {
                return pools[ size ];
            }
        };

        static inline auto decreasing = []( const auto &a, const auto &b )
        {
            return a > b;
        };

        std::set< uint32_t, decltype( decreasing ) > sizes( const auto &cs ) const
        {
            std::set< uint32_t, decltype( decreasing ) > out;
            for ( const auto &[ _, c ] : cs )
                for ( const auto &[ size, _1 ] : c.buckets )
                    out.emplace( size );
            return out;
        }

        void do_assign( typename C::CS cs )
        {
            CtxCollector ctx_info;
            ctx_info.Run( circuit );

            auto all_pools = BlueprintPool( circuit );
            auto todo_sizes = sizes( cs );

            std::unordered_map< Operation *, std::set< blueprint_t > > global_usages;

            auto do_level = [ & ]( auto &item, auto level, auto &states )
            {
                auto &[ ctx, todo ] = item;
                std::map< Op *, blueprint_t > matched;

                for ( auto op : todo )
                {
                    auto bp = [ & ]()
                    {
                        for ( auto &[ size, pool ] : all_pools.pools )
                        {
                            if ( size < op->size )
                                break;
                            if ( !config::can_extend() && size != op->size )
                                continue;

                            for ( auto bp : pool )
                            {
                                std::size_t reserved_count = 0;
                                for ( auto req_ctx : ctx_info.op_to_ctxs[ op ] )
                                    reserved_count += global_usages[ req_ctx ].count( bp );
                                if ( reserved_count == 0 )
                                    return bp;
                            }
                        }
                        return all_pools.emplace( op->size, op->operands_size() );
                    }();

                    global_usages[ ctx ].insert( bp );
                    matched.emplace( op, bp );
                }

                for ( auto &[ o_ctx, o_todo ] : states )
                {
                    for ( auto &[ matched_op, matched_bp ] : matched )
                    {
                        if ( !o_todo.count( matched_op ) )
                            continue;

                        o_todo.erase( matched_op );
                        emit_constraints( o_ctx, matched_op, matched_bp );

                        global_usages[ o_ctx ].emplace( matched_bp );
                    }
                }

                for ( auto &[ matched_op, matched_bp ] : matched )
                {
                    auto coerced = config::adjust_result( circuit, matched_bp, matched_op );
                    matched_op->replace_all_uses_with( coerced );
                }
            };

            for ( auto level : todo_sizes )
            {
                auto todo = fetch_buckets( cs, level );
                for ( auto todo_item : todo )
                    do_level( todo_item, level, todo );
            }

        }

        void emit_constraints( Operation *ctx, Operation *op, blueprint_t bp )
        {
            check( op->operands_size() == bp->operands_size() )
                << op->operands_size() << "!=" << bp->operands_size();
            for ( std::size_t i = 0; i < op->operands_size(); ++i)
            {
                auto coerced = config::adjust_operand( circuit, bp, op->operand( i ) );

                auto ac = circuit->create< AdviceConstraint >();
                ac->add_operand( coerced );
                ac->add_operand( bp->operand( i ) );
                ctx->add_operand( ac );
            }
        }
    };

    template< typename Op, typename Trg >
    struct PropagateOp
    {
        Circuit *circuit;

        void run()
        {
            for ( auto op : circuit->attr< Op >() )
                run( op );
        }

        void run( Op *op )
        {
            std::unordered_map< std::size_t, uint32_t > can_propagate;
            std::size_t op_idx = 0;

            for ( auto operand : op->operands() )
            {
                auto advice = dyn_cast< Advice >( operand );
                if ( !advice )
                    continue;

                if ( auto size = size_of_extension( advice, op ); size != 0 )
                    can_propagate.emplace( op_idx, size );
                ++op_idx;
            }

            if ( can_propagate.empty() )
                return;

            for ( auto &[ idx, size ] : can_propagate )
            {
                auto old_advice = dyn_cast< Advice >( op->operand( idx ) );
                check( old_advice );
                auto new_size = old_advice->size - size;
                check( new_size != 0 );

                auto new_advice = circuit->create< Advice >( new_size, old_advice->advice_idx );
                auto coerced_advice = circuit->create< Trg >( old_advice->size );
                coerced_advice->add_operand( new_advice );

                op->replace_operand( idx, coerced_advice );

                std::unordered_map< Operation *, Operation * > to_replace;
                for ( auto user : old_advice->users() )
                {
                    if ( user == op )
                        continue;
                    check( isa< AdviceConstraint >( user ) );
                    auto ac = circuit->create< AdviceConstraint >();

                    auto old_cast = dyn_cast< Trg >( user->operand( 0 ) );
                    check( old_cast );
                    auto raw_op = old_cast->operand( 0 );

                    auto coerced = [ & ]() -> Operation *
                    {
                        if ( raw_op->size == new_advice->size )
                            return raw_op;
                        auto new_cast = circuit->create< Trg >( new_advice->size );
                        new_cast->add_operand( raw_op );
                        return new_cast;
                    }();

                    ac->add_operand( coerced );
                    ac->add_operand( new_advice );
                    to_replace[ user ] = ac;
                }

                for ( auto &[ old, replacement ] : to_replace )
                    old->replace_all_uses_with( replacement );
            }

        }

        uint32_t size_of_extension( Advice *advice, Op *self )
        {
            uint32_t size = advice->size;
            for ( auto user : advice->users() )
            {
                if ( user == self )
                    continue;
                auto ac = dyn_cast< AdviceConstraint >( user );
                check( ac );

                auto sext = dyn_cast< Trg >( ac->runtime_value() );
                if ( !sext )
                    return 0;

                size = std::min< uint32_t >( size, sext->size - sext->operand( 0 )->size );
            }
            return size;
        }

    };

    using allowed_ops_ts = tl::TL<
        Mul, Add, Sub, SDiv, UDiv, URem, SRem, PopulationCount,
        LShr, AShr, Shl
    >;

    template< typename T >
    struct MergeConfig{};

    template< typename Op >
    struct BasicConfig
    {
        static bool replace( Op * ) { return true; }
        static bool can_extend() { return true; }

        static Operation *adjust_operand( Circuit *circuit, Operation *advice,
                                          Operation *runtime )
        {
            if ( advice->size == runtime->size )
                return runtime;
            check( advice->size > runtime->size );
            auto cast = circuit->create< SExt >( advice->size );
            cast->add_operand( runtime );
            return cast;
        }

        static Operation *adjust_result( Circuit *circuit, Op *op, Operation *old )
        {
            if ( op->size == old->size )
                return op;
            check( op->size > old->size );
            auto trunc = circuit->create< Trunc >( old->size );
            trunc->add_operand( op );
            return trunc;
        }
    };

    template<> struct MergeConfig< Mul > : BasicConfig< Mul > {};
    template<> struct MergeConfig< Add > : BasicConfig< Add > {};
    template<> struct MergeConfig< Sub > : BasicConfig< Sub > {};
    template<> struct MergeConfig< SDiv > : BasicConfig< SDiv > {};
    template<> struct MergeConfig< UDiv > : BasicConfig< UDiv > {};
    template<> struct MergeConfig< SRem > : BasicConfig< SRem > {};
    template<> struct MergeConfig< URem > : BasicConfig< URem > {};
    template<> struct MergeConfig< PopulationCount > : BasicConfig< PopulationCount > {};


    template< typename Op >
    struct ShiftConfig
    {
        static bool replace( Op *op )
        {
            // Constant shifts should be really cheap.
            return !isa< Constant >( op->operand( 1 ) );
        }

        static bool can_extend() { return true; }

        static Operation *adjust_operand( Circuit *circuit, Operation *advice,
                                          Operation *runtime )
        {
            if ( advice->size == runtime->size )
                return runtime;
            check( advice->size > runtime->size );

            auto diff = runtime->size - advice->size;
            auto zero = std::string( diff, '0' );
            auto fill = circuit->create< Constant >( zero, diff );
            auto concat = circuit->create< Concat >( runtime->size );
            concat->add_operand( runtime );
            concat->add_operand( fill );

            return concat;
        }

        static Operation *adjust_result( Circuit *circuit, Op *op, Operation *old )
        {
            if ( op->size == old->size )
                return op;
            check( op->size > old->size );

            auto extract = circuit->create< Extract >( 0u, old->size );
            extract->add_operand( op );
            return extract;
        }
    };

    template<> struct MergeConfig< LShr > : ShiftConfig< LShr > {};
    template<> struct MergeConfig< AShr > : ShiftConfig< AShr > {};
    template<> struct MergeConfig< Shl > : ShiftConfig< Shl > {};

    auto merge_with_advice( Circuit::circuit_ptr_t &&circuit,
                            const std::vector< Operation::kind_t > &kinds )
        -> Circuit::circuit_ptr_t
    {
        auto merge_one = [&]< typename Op >()
        {
            MergeWithAdvice< Op, MergeConfig >{ circuit.get() }.run();
            if constexpr ( Op::kind != LShr::kind && Op::kind != AShr::kind &&
                           Op::kind != Shl::kind )
            {
                PropagateOp< Op, SExt >{ circuit.get() }.run();
                PropagateOp< Op, ZExt >{ circuit.get() }.run();
            }
        };

        for ( auto kind : kinds )
        {
            auto matches = [&]< typename Op >(){ return isa< Op >( kind ); };
            match_on( std::move( matches ), merge_one, allowed_ops_ts{} );
        }

        return circuit;
    }

}  // namespace circ
