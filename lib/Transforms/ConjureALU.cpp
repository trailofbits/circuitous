/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/ConjureALU.hpp>

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
    template< typename T >
    struct MergeConfig {};

    template< typename Op >
    struct ShiftConfig
    {
        static bool replace( Op *op ) { return true; }
        static bool ignore( Op * op ) { return false; }

        static Operation *adjust_operand( Circuit *circuit,
                                          Switch *bp_operand,
                                          Operation *runtime )
        {
            if ( bp_operand->size == runtime->size )
                return runtime;
            check( bp_operand->size > runtime->size );

            auto diff = bp_operand->size - runtime->size;
            auto zero = std::string( diff, '0' );
                auto fill = circuit->create< Constant >( zero, diff );
                auto concat = circuit->create< Concat >( bp_operand->size );

                concat->add_operand( runtime );
                concat->add_operand( fill );

            check( concat->size == bp_operand->size )
                << concat->size << "!=" << bp_operand->size;
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

        static void emit_constraints( Circuit *circuit,
                                      VerifyInstruction *ctx, Operation *op, Op *bp )
        {
            check( op->operands_size() == bp->operands_size() )
                << op->operands_size() << "!=" << bp->operands_size();
            for ( std::size_t i = 0; i < op->operands_size(); ++i)
            {
                auto bp_operand = dyn_cast< Switch >( bp->operand( i ) );
                check( bp_operand ) << pretty_print( bp->operand( i ) );
                auto coerced = adjust_operand( circuit, bp_operand, op->operand( i ) );

                auto decoder = ctx->decoder();
                check( decoder );

                auto opt = [ & ]() -> Option *
                {
                    if ( auto o = bp_operand->option( coerced ) )
                        return *o;
                    auto o = circuit->create< Option >( coerced->size );
                    bp_operand->add_operand( o );
                    o->add_operand( coerced );
                    return o;
                }();

                opt->add_operands( *decoder );

            }
        }
    };

    template<> struct MergeConfig< Add > : ShiftConfig< Add >
    {
        static bool ignore( Add *op )
        {
            for ( auto o : dyn_cast< Constant >( op->operands() ) )
                if ( o )
                    return true;
            return false;

        }
    };
    template<> struct MergeConfig< Sub > : ShiftConfig< Sub >
    {
        static bool ignore( Sub *op )
        {
            for ( auto o : dyn_cast< Constant >( op->operands() ) )
                if ( o )
                    return true;
            return false;

        }
    };

    template<> struct MergeConfig< SDiv > : ShiftConfig< SDiv > {};
    template<> struct MergeConfig< UDiv > : ShiftConfig< UDiv > {};

    template<> struct MergeConfig< URem > : ShiftConfig< URem > {};
    template<> struct MergeConfig< SRem > : ShiftConfig< SRem > {};

    template<> struct MergeConfig< Mul > : ShiftConfig< Mul > {};
    template<> struct MergeConfig< PopulationCount > : ShiftConfig< PopulationCount > {};

    template<> struct MergeConfig< Select > : ShiftConfig< Select >
    {
        static Operation *adjust_operand( Circuit *circuit,
                                          Switch *bp_operand,
                                          Operation *runtime )
        {
            if ( bp_operand->size == runtime->size )
                return runtime;
            check( bp_operand->size > runtime->size );

            auto diff = bp_operand->size - runtime->size;
            auto zero = std::string( diff, '0' );
                auto fill = circuit->create< Constant >( zero, diff );
                auto concat = circuit->create< Concat >( bp_operand->size );

                concat->add_operand( runtime );
                concat->add_operand( fill );

            check( concat->size == bp_operand->size )
                << concat->size << "!=" << bp_operand->size;
            return concat;
        }

        static void emit_constraints( Circuit *circuit,
                                      VerifyInstruction *ctx, Operation *op, Select *bp )
        {
            auto selector = dyn_cast< Switch >( bp->operand( 0 ) );
            check( selector );

            auto coerced = adjust_operand( circuit, selector, op->operand( 0 ) );
            auto opt = [ & ]() -> Option *
            {
                if ( auto o = selector->option( coerced ) )
                    return *o;
                auto o = circuit->create< Option >( coerced->size );
                selector->add_operand( o );
                o->add_operand( coerced );
                return o;
            }();

            opt->add_operand( *ctx->decoder() );
        }
    };

    template< typename Op >
    struct BlueprintPool_
    {
        using blueprint_t = Op *;
        using aspirant_t = Op *;
        using pool_t = std::vector< blueprint_t >;

        std::map< uint32_t, std::vector< blueprint_t > > pools;

        Circuit *circuit;

        BlueprintPool_( Circuit *circuit ) : circuit( circuit ) {}

        blueprint_t emplace( uint32_t size, aspirant_t op )
        {
            auto blueprint = circuit->create< Op >( size );
            for ( std::size_t i = 0; i < op->operands_size(); ++i )
            {
                auto s = circuit->create< Switch >( size );
                s->set_meta( "conjure-alu-blueprint-operand", "0" );
                blueprint->add_operand( s );
            }

            return pools[ size ].emplace_back( blueprint );
        }

        bool can_be_used( uint32_t size, Operation * op) const
        {
            return op->size <= size;
        }
    };

    template< typename Op >
    struct BlueprintPool : BlueprintPool_< Op >
    {
        using parent_t = BlueprintPool_< Op >;
        using parent_t::parent_t;
    };

    template<>
    struct BlueprintPool< Select > : BlueprintPool_< Select >
    {
        using parent_t = BlueprintPool_< Select >;
        using blueprint_t = typename parent_t::blueprint_t;
        using aspirant_t = typename parent_t::aspirant_t;

        uint32_t min_size;
        std::unordered_set< blueprint_t > images;

        std::unordered_map< blueprint_t, std::vector< blueprint_t > > pools;

        BlueprintPool( Circuit *circuit ) : parent_t( circuit )
        {
            for ( auto select : circuit->attr< Select >() )
                select_image( select );
        }

        void select_image( aspirant_t select )
        {
            auto generalized = false;
            for ( auto it = images.begin(); it != images.end(); )
            {
                if ( select->is_extension_of( *it ) )
                {
                    it = images.erase( it );
                    continue;
                }

                if ( (*it)->is_extension_of( select ) )
                    generalized = true;
                ++it;
            }

            if ( !generalized )
                images.emplace( select );
        }

        blueprint_t emplace( uint32_t, aspirant_t op )
        {
            for ( auto image : images )
            {
                if ( !op->can_be_extended_to( image ) )
                    continue;
                auto materialized = circuit->create< Select >( image->bits, image->size );
                materialized->add_operands( image->operands() );

                auto s = circuit->create< Switch >( image->operand( 0 )->size );
                materialized->replace_operand( 0, s );

                pools[ image ].emplace_back( materialized );
                return materialized;
            }
            log_kill() << "Unreachable.";
            return nullptr;

        }

        bool can_be_used( blueprint_t key, Select * op) const
        {
            return op->can_be_extended_to( key );
        }
    };

    template< typename Op, template< typename > class Configuration >
    struct ALUConjurer
    {
        using aspirant_t = Op *;
        using ctx_t = VerifyInstruction *;

        using config = Configuration< Op >;
        // TODO(lukas): Move to config?
        using blueprint_t = aspirant_t;

        template< typename V >
        using ctx_map_t = std::unordered_map< ctx_t, V >;

        struct aspirants_by_size_t
        {
            using ops_bucket_t = std::set< Op * >;
            std::map< uint32_t, ops_bucket_t > buckets;

            std::string to_string() const
            {
                std::stringstream os;
                for ( const auto &[ size, bucket ] : buckets )
                {
                    os << "[ " << size << " ]\n";
                    for ( auto op : bucket )
                        os << "\t | " << pretty_print< false >( op ) << "\n";
                }
                return os.str();
            }

            void add( aspirant_t op )
            {
                buckets[ op->size ].emplace( op );
            }
        };

        struct aspirants_of_ctx_t : aspirants_by_size_t
        {
            using parent_t = aspirants_by_size_t;

            VerifyInstruction *ctx;

            aspirants_of_ctx_t( parent_t &&parent, ctx_t ctx )
                : parent_t( std::move( parent ) ),
                  ctx( ctx )
            {}

            aspirants_of_ctx_t( ctx_t ctx ) : ctx( ctx ) {}

            std::string to_string() const
            {
                std::stringstream os;
                os << "Collected of : " << pretty_print< false >( ctx ) << "\n";
                os << this->parent_t::to_string();
                return os.str();
            }
        };

        struct all_aspirants_map_t
        {
            ctx_map_t< aspirants_of_ctx_t > mapping;

            std::string to_string() const
            {
                std::stringstream ss;
                for ( const auto &[ _, v ] : mapping )
                    ss << v.to_string() << "\n";
                return ss.str();
            }

            static std::unordered_set< Operation * > decoding_tree( Circuit *circuit )
            {
                // We need to ignore those that are part of the decoder, as the switch we emit
                // will use the decoder results as keys.
                std::unordered_set< Operation * > out;
                auto ignore = [ & ]( auto op, auto &&next ) -> void
                {
                    out.emplace( op );
                    for ( auto o : op->operands() )
                        next( o, next );
                };

                for ( auto op : circuit->attr< DecoderResult >())
                    ignore( op, ignore );

                return out;
            }


            all_aspirants_map_t( Circuit *circuit, const CtxCollector &ctx_info )
            {
                auto ignored = decoding_tree( circuit );

                for ( auto op : circuit->attr< Op >() )
                {
                    if ( Configuration< Op >::ignore( op ) )
                        continue;
                    for ( auto ctx : ctx_info[ op ] )
                    {
                        if ( ignored.count( op ) )
                            continue;
                        auto insert_point = [&]()
                        {
                            auto it = mapping.find( ctx );
                            if ( it == mapping.end() )
                            {
                                auto [ o, _ ] = mapping.emplace(
                                        ctx,
                                        aspirants_of_ctx_t{ ctx } );
                                return o;
                            }
                            return it;
                        }();
                        insert_point->second.add( op );
                    }
                }

            }

            const auto &raw() const { return mapping; }
        };

        // We need to use `_` to avoid name shadowing.
        Circuit *_circuit;
        const CtxCollector &ctx_info;
        BlueprintPool< Op > all_pools;

        std::unordered_map< ctx_t,
                            std::unordered_set< blueprint_t > > global_usages;

        std::unordered_map< ctx_t, uint64_t > top;
        std::unordered_set< Operation * > decoder_ops;

        ALUConjurer( Circuit *circuit, const CtxCollector &ctx_info )
            : _circuit( circuit ),
              ctx_info( ctx_info ),
              all_pools( circuit ),
              decoder_ops( all_aspirants_map_t::decoding_tree( circuit ) )
        {}

        void conjure_all()
        {
            return do_assign( all_aspirants_map_t( _circuit, ctx_info ) );
        }

        blueprint_t allocate_bp( aspirant_t op, uint32_t bp_default_size )
        {
            for ( auto &[ size, pool ] : all_pools.pools )
            {
                if ( !all_pools.can_be_used( size, op ) )
                    continue;

                for ( auto bp : pool )
                {
                    std::size_t reserved_count = 0;
                    for ( auto req_ctx : ctx_info[ op ] )
                    {
                        if ( top.count( req_ctx ) && bp->id() < top[ req_ctx ] )
                        {
                            reserved_count += 1;
                        }
                        reserved_count += global_usages[ req_ctx ].count( bp );
                    }
                    if ( reserved_count == 0 )
                        return bp;
                }
            }
            return all_pools.emplace( std::max< uint32_t >( bp_default_size, op->size ),
                                      op );
        }

        bool was_conjured( aspirant_t aspirant ) const
        {
            return aspirant->has_meta( "conjure-alu-blueprint-operand");
        }

        bool ignore( aspirant_t aspirant ) const
        {
            return decoder_ops.count( aspirant ) && !was_conjured( aspirant );
        }

        auto get_is_aspirant() const
        {
            return []( auto op )
            {
                auto c = dyn_cast< Op >( op );
                return c && !config::ignore( c );
            };
        }

        std::vector< aspirant_t > of_size() const
        {
            std::vector< aspirant_t > out;
            for ( auto aspirant : _circuit->attr< Op >() )
                if ( !ignore( aspirant ) )
                    out.emplace_back( aspirant );
            return out;
        }

        void do_assign( const all_aspirants_map_t &cs )
        {
            auto do_aspirant = [ & ]( auto aspirant, auto size )
            {
                auto bp = allocate_bp( aspirant, size );

                for ( auto req_ctx : ctx_info[ aspirant ] )
                {
                    config::emit_constraints( _circuit, req_ctx, aspirant, bp );
                    global_usages[ req_ctx ].emplace( bp );
                    if ( top.count( req_ctx ) )
                        top[ req_ctx ] = std::max< uint64_t >( top[ req_ctx ], bp->id() );
                    else
                        top[ req_ctx ] = bp->id();
                }

                auto coerced = config::adjust_result( _circuit, bp, aspirant );
                aspirant->replace_all_uses_with( coerced );
            };

            auto topo_sorted = topology( _circuit->root, get_is_aspirant() );
            for ( auto aspirant : dyn_cast< Op >( topo_sorted ) )
            {
                check( aspirant );
                do_aspirant( aspirant, 64u );
            }

        }
    };

    bool constrains( Operation *ctx, Operation *advice )
    {
        for ( auto x : ctx->operands() )
            if ( auto y = dyn_cast< AdviceConstraint >( x ) )
                if ( y->advice() == advice )
                    return true;
        return false;
    }

    bool constrains( Operation *ctx, Memory *hint )
    {
        for ( auto x : ctx->operands() )
            if ( constrained_by( hint, x ) )
                return true;
        return false;
    }

    auto mend_acs( Circuit *circuit )
    {
        for ( auto advice : circuit->attr< Advice >() )
        {
            for ( auto ctx : circuit->attr< VerifyInstruction >() )
            {
                if ( constrains( ctx, advice ) )
                    continue;

                auto ac = circuit->create< AdviceConstraint >();
                auto zero = std::string( advice->size, '0' );
                auto fill = circuit->create< Constant >( zero, advice->size );
                ac->add_operand( fill );
                ac->add_operand( advice );
                ctx->add_operand( ac );

                ac->set_meta( "mend_acs", "0" );
            }

        }

        for ( auto hint : circuit->attr< Memory >() )
        {
            for ( auto ctx : circuit->attr< VerifyInstruction >() )
            {
                if ( constrains( ctx, hint ) )
                    continue;
                auto unused = circuit->create< UnusedConstraint >();
                unused->add_operand( hint );
                ctx->add_operand( unused );
            }
        }
    }

    using allowed_ops_ts = tl::TL<
        Mul, Add, Sub, SDiv, UDiv, URem, SRem, PopulationCount, Select
    >;

    auto conjure_alu( circuit_owner_t &&circuit,
                      const std::vector< Operation::kind_t > &kinds )
        -> circuit_owner_t
    {
        auto ctx_info = CtxCollector( circuit.get() );
        auto merge_one = [&]< typename Op >()
        {
            if ( circuit->attr< Op >().size() <= 1 )
                return;
            log_dbg() << "Going to handle:" << Op::op_code_str();
            ALUConjurer< Op, MergeConfig >( circuit.get(), ctx_info ).conjure_all();
            VerifyCircuit( "Verification after ALU conjuring.", circuit.get() );
        };

        for ( auto kind : kinds )
            log_dbg() << "  | " << op_code_str( kind );

        for ( auto kind : kinds )
            dispatch_on_kind( allowed_ops_ts{}, kind, merge_one );

        mend_acs( circuit.get() );
        return circuit;
    }

}  // namespace circ
