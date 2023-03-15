/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */
#pragma once

#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

#include <gap/core/generator.hpp>

#include <optional>

namespace circ
{
    // TODO(lukas): Provide better api than `remill::Instruction`.
    struct Instruction
    {

    };

    // TODO(lukas): Just temporary struct to make refactoring easier.
    struct InstructionInfo
    {
        using rinst_t = remill::Instruction;

        using shadow_t = shadowinst::Instruction;
        using shadows_t = std::vector< shadow_t >;

        using lifted_t = llvm::Function *;
        using enc_t = std::bitset< 15 * 8 >;

        // enc must be first as we will be moving `rinst` in.
        std::optional< enc_t >    _enc;
        std::optional< rinst_t >  _rinst;

        std::optional< lifted_t > _lifted;

        std::vector< shadow_t > shadows;
        std::vector< rinst_t > rinsts;

      public:
        InstructionInfo(remill::Instruction rinst, std::string bytes)
            : _enc(InstBytes( bytes ).to_enc< 15 * 8 >()),
              rinsts{ std::move( rinst ) }
        {}

        InstructionInfo() = delete;

        bool has_shadow() const { return !shadows.empty(); }
        bool has_lifted() const { return _lifted.has_value(); }

        rinst_t &rinst()   { check(!rinsts.empty()); return rinsts[ 0 ]; }
        shadow_t &shadow() { check(has_shadow()); return shadows.front(); }
        lifted_t &lifted() { check(_lifted); return *_lifted; }
        enc_t &enc()       { check(_enc); return *_enc; }

        const rinst_t &rinst()   const  { check(!rinsts.empty()); return rinsts[ 0 ]; }
        const shadow_t &shadow() const { check(has_shadow()); return shadows.front(); }
        const lifted_t &lifted() const { check(_lifted); return *_lifted; }
        const enc_t &enc()       const { check(_enc); return *_enc; }

        void set(rinst_t r) { rinsts.emplace_back( std::move(r) ); }
        void set(shadow_t s) { shadows.emplace_back( std::move(s) ); }
        void set(enc_t e) { _enc = std::make_optional( std::move(e) ); }
        void set(lifted_t l) { _lifted = std::make_optional( std::move(l) ); }

        void make_fuzz(CtxRef & ctx)
        {
            set(fuzz_operands(*ctx.arch(), rinst()));
            shadow().distribute_selectors();
        }

        template< typename ILifter >
        void make_lifted(CtxRef &ctx) {
            auto maybe_fn = ILifter(ctx).lift(*this);
            check(maybe_fn);
            set(*maybe_fn);
        }

        bool is_ready() const
        {
            check(_rinst && _enc) << "InstructionInfo is in inconsistent state!";
            return has_shadow() && has_lifted();
        }

        void merge( InstructionInfo &&other )
        {
            shadows.insert( shadows.end(),
                            std::make_move_iterator( other.shadows.begin() ),
                            std::make_move_iterator( other.shadows.end() ) );
            rinsts.insert( rinsts.end(),
                           std::make_move_iterator( other.rinsts.begin() ),
                           std::make_move_iterator( other.rinsts.end() ) );
        }
    };

    // One atomic thing to lift, should consist of
    //  * concrete instance
    //  * shadow
    //  TODO( lifter ): Do we really need the concrete instance moving forward?
    struct Atom
    {
        using concrete_t = remill::Instruction;
        using abstract_t = shadowinst::Instruction;

        concrete_t concrete;
        abstract_t abstract;

        Atom( concrete_t concrete, abstract_t abstract )
            : concrete( std::move( concrete ) ),
              abstract( std::move( abstract ) )
        {}

        Atom( const Atom & ) = delete;
        Atom( Atom && ) = default;

        Atom &operator=( const Atom & ) = delete;
        Atom &operator=( Atom && ) = default;

        isel_t isel()
        {
            return concrete.function;
        }

        struct slice_view
        {
            template< typename C, typename A >
            using p = std::tuple< C *, A * >;

            using rop = remill::Operand;

            using imm = p< rop::Immediate , shadowinst::Immediate >;
            using reg = p< rop::Register , shadowinst::Reg >;
            using addr = p< rop::Address , shadowinst::Address >;

            using storage_t = std::variant< imm, reg, addr >;
            storage_t raw;

            std::size_t size;
            bool is_read;

            slice_view( remill::Operand *concrete_op, shadowinst::Operand *abstract_op )
                : raw( mk_raw( concrete_op, abstract_op ) ),
                  size( concrete_op->size ),
                  is_read( concrete_op->action == remill::Operand::kActionWrite )
            {}

            static storage_t mk_raw( remill::Operand *concrete_op,
                                     shadowinst::Operand *abstract_op )
            {
                switch( concrete_op->type )
                {
                    case remill::Operand::kTypeRegister:
                        return reg( &concrete_op->reg, abstract_op->reg() );
                    case remill::Operand::kTypeImmediate:
                        return imm( &concrete_op->imm, abstract_op->immediate() );
                    case remill::Operand::kTypeAddress:
                        return addr( &concrete_op->addr, abstract_op->address() );
                    default:
                        log_kill() << "Unsupported type of concrete instruction.";
                }
            }

        };

        auto slice( std::size_t idx ) -> slice_view
        {
            return slice_view( &concrete.operands[ idx ], &abstract.operands[ idx ] );
        }

        auto slices() -> gap::generator< slice_view >
        {
            for ( std::size_t i = 0; i < concrete.operands.size(); ++i )
                co_yield slice( i );
        }

        std::size_t operand_count()
        {
            return concrete.operands.size();
        }

        bool is_read( std::size_t i )
        {
            return concrete.operands[ i ].action = remill::Operand::kActionRead;
        }

        bool is_write( std::size_t i )
        {
            return concrete.operands[ i ].action = remill::Operand::kActionWrite;
        }
    };

    // template args:
    //  `A` - atom to be used
    //  `I` - materialized isel
    template< typename A >
    struct Unit
    {
        using atom_t = A;
        using atoms_t = std::vector< atom_t >;

        isel_t isel;
        atoms_t atoms;

        Unit( isel_t isel, atoms_t atoms )
            : isel( isel ), atoms( std::move( atoms ) )
        {}

        auto begin() { return atoms.begin(); }
        auto end() { return atoms.end(); }

        auto operand_count()
        {
            check( !atoms.empty() );
            std::size_t out = atoms.front().operand_count();
            for ( std::size_t i = 0; i < atoms.size(); ++i )
                check( out == atoms[ i ].operand_count() );
            return out;
        }

        auto slices( std::size_t idx ) -> gap::generator< typename atom_t::slice_view >
        {
            for ( std::size_t i = 0u; i < atoms.size(); ++i )
                co_yield atoms[ i ].slice( idx );
        }

        bool is_read( std::size_t i ) { return atoms.front().is_read( i ); }
        bool is_write( std::size_t i ) { return atoms.front().is_write( i ); }
    };


    template< typename ISem, typename Next >
    struct with_materialized_isel : Next
    {
        using isem_t = ISem;

        isem_t isem_instance;
    };

    template< typename U >
    struct Worklist
    {
        using unit_t = U;

        std::vector< U > todo;

        template< typename M >
        auto materialize( auto &&materialize ) &&
        {
            Worklist< decltype( materialize( std::declval< unit_t >() ) ) > out;

            for ( auto unit : std::move( todo ) )
                out.emplace_back( materialize( std::move( unit ) ) );
            return out;
        }

        auto begin() { return todo.begin(); }
        auto begin() const { return todo.begin(); }

        auto end() { return todo.end(); }
        auto end() const { return todo.end(); }

        void add( unit_t unit )
        {
            todo.emplace_back( std::move( unit ) );
        }

        template< typename ... Args >
        void emplace( Args && ... args )
        {
            todo.emplace_back( std::forward< Args >( args ) ... );
        }

        std::size_t size() const { return todo.size(); }
    };

    struct InstructionBatch : has_ctx_ref
    {
        using parent_t = has_ctx_ref;
        using self_t = InstructionBatch;
        using insts_t = std::vector< InstructionInfo >;
        using raw_insts_t = std::vector< remill::Instruction >;

        using shadows_t = InstructionInfo::shadows_t;

        insts_t insts;

        InstructionBatch(Ctx &ctx);
        InstructionBatch(Ctx &ctx, const std::vector< remill::Instruction > &rinsts);
        InstructionBatch(Ctx &ctx, const std::vector< InstBytes > &inst_bytes);
        InstructionBatch(Ctx &ctx, const std::string &raw_bytes);

        insts_t *operator->() { return &insts; }
        const insts_t *operator->() const { return &insts; }

        // Order of entries is not guaranteed.
        self_t &add(InstructionBatch &&other);
        self_t &add(InstructionInfo &&inst_info);
        self_t &add(raw_insts_t &&rinsts);

        self_t &fuzz();

        // Aggregate all Instructions with the same ISEL to one while remembering all shadows.
        // TODO( lifter ): Shadows need to be compatible?
        self_t &aggregate();

        template< typename ILifter >
        self_t &lift()
        {
            for (auto &info : insts)
                if (!info.has_lifted())
                    info.make_lifted< ILifter >(this->ctx);
            return *this;
        }

        insts_t take() { return std::move(insts); }
        const insts_t &get() const { return insts; }

        std::string categories() const;
    };

} // namespace circ
