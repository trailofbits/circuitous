/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.hpp>
#include <circuitous/Util/TypeList.hpp>
#include <circuitous/Util/TypeTraits.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/IR/Metadata.hpp>

#include <bitset>
#include <iosfwd>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <unordered_map>
#include <vector>

namespace circ
{
    struct CircuitStorage;

    // A general instruction.
    struct Operation : public Node< Operation >, HasStringMeta
    {
      public:
        enum class kind_t : uint32_t
        {
            kFirst = 0,
            kOperation = 1,

            kInputSyscallState,
            kOutputSyscallState,

            kInputSyscallReg,
            kOutputSyscallReg,

            kInputRegister,
            kOutputRegister,

            kInputErrorFlag,
            kOutputErrorFlag,

            kInputTimestamp,
            kOutputTimestamp,

            kUndefined,
            kMemory,
            kConstant,
            kAdvice,
            kInputInstructionBits,

            kRegConstraint,
            kAdviceConstraint,
            kWriteConstraint,
            kReadConstraint,
            kUnusedConstraint,

            // LLVM
            kAdd,
            kSub,
            kMul,
            kUDiv,
            kSDiv,
            kShl,
            kLShr,
            kAShr,
            kTrunc,
            kZExt,
            kSExt,
            kIcmp_ult,
            kIcmp_slt,
            kIcmp_ugt,
            kIcmp_eq,
            kIcmp_ne,
            kIcmp_uge,
            kIcmp_ule,
            kIcmp_sgt,
            kIcmp_sge,
            kIcmp_sle,
            kSRem,
            kURem,
            kXor,
            kAnd,
            kOr,

            kInpuImmediate,

            kExtract,
            kConcat,
            kPopulationCount,
            kCountTrailingZeroes,
            kCountLeadingZeroes,
            kParity,
            kNot,

            kSwitch,
            kOption,
            kSelect,

            // Condition
            kDecodeCondition,
            kDecoderResult,
            kOnlyOneCondition,
            kVerifyInstruction,

            kSyscallModule,

            kLast
        };

        static constexpr kind_t kind = kind_t::kOperation;
        static constexpr uint32_t bool_size = 1u;

        virtual ~Operation() = default;

        virtual std::string name() const;
        static std::string op_code_str() { not_implemented(); }

        const auto &operator[](std::size_t idx) const { return _operands[idx]; }

        template< typename Vis >
        void traverse(Vis &vis)
        {
            for (auto op : _operands)
                vis.dispatch(op);
        }

        template< typename Vis >
        void traverse_upwards(Vis &vis)
        {
            for (auto op : users())
                vis.dispatch(op);
        }

      protected:
        // Please note, that id is not set.
        explicit Operation(unsigned size_, Operation::kind_t op_code_)
            : size(size_), op_code(op_code_)
        {}

      public:
        // `id` should be unique in a given circuit.
        uint64_t id() const { return _id; }

        // Size in bits of this instruction's "result" value. For example, a zero-
        // extension will represent the size of the output value.
        const unsigned size;

        // The "opcode" of this.
        const kind_t op_code;

      private:
        friend CircuitStorage;

        // Must be set manually after ctor is called.
        uint64_t _id = 0;
    };

    template< typename O >
    concept circ_ir_op = std::is_base_of_v< Operation, O >;

    template< typename O >
    concept circ_ir_ptr = circ_ir_op< std::remove_pointer_t< O > > && std::is_pointer_v< O >;

    static inline std::unordered_set< const Operation * > unique_operands( const Operation *op )
    {
        return freeze< std::unordered_set< const Operation * > >( op->operands() );
    }

    template< typename R >
    std::optional< Operation::kind_t > reconstruct_kind(R r)
    {
       if (r > util::to_underlying(Operation::kind_t::kFirst) &&
           r < util::to_underlying(Operation::kind_t::kLast))
       {
           return { static_cast< Operation::kind_t >(r) };
       }
       return {};
    }

    // TODO(lukas): Move to some class that inherits from gap::generator? Together with
    //              freeze possibly define more generic framework in a range-like fashion.
    template< circ_ir_op Op, gap::ranges::range From >
        requires ( circ_ir_ptr< gap::ranges::range_value_t< From > > )
    auto filter( From from )
        -> gap::generator< util::copy_const_t< gap::ranges::range_value_t< From >, Op * > >
    {
        for ( auto op : from )
            if ( auto casted = dyn_cast< Op >( op ) )
                co_yield casted;
    }

    static inline auto frozen_users( Operation *op )
    {
        return freeze< std::vector >( op->users() );
    }
    static inline auto frozen_operands( Operation *op )
    {
        return freeze< std::vector >( op->operands() );
    }


    template< class T >
    concept is_type_list = requires (T t)
    {
        { tl::TL{ t } } -> std::same_as< T >;
    };


    template< typename T >
    bool isa(Operation::kind_t rkind) { return T::kind == rkind; }

    template< typename T >
    bool isa(const Operation *op) { return isa< T >(op->op_code); }

    template< typename TypeList >
    bool is_in(Operation::kind_t rkind)
    {
        auto accept = [&]< typename T >() { return isa< T >(rkind); };
        return tl::contains< TypeList >(accept);
    }

    template< typename T > requires (is_type_list< T >)
    bool isa(Operation::kind_t rkind) { return is_in< T >(rkind); }

    template< typename ...Ts >
    bool is_one_of(const Operation *op)
    {
        return (isa< Ts >(op->op_code) || ...);
    }

    template< typename T > requires ( std::is_base_of_v< Operation, T > )
    T *dyn_cast( Operation * op )
    {
        if ( !isa< T >( op ) )
            return nullptr;
        return static_cast< T * >( op );
    }

    template< typename T > requires ( circ_ir_op< T > )
    gap::generator< T * > dyn_cast( const std::vector< Operation * > &from )
    {
        for ( auto x : from )
            co_yield dyn_cast< T >( x );
    }

    template< typename T, gap::ranges::range From > requires ( circ_ir_op< T > )
    gap::generator< T * > dyn_cast( From from )
    {
        for ( auto x : from )
            co_yield dyn_cast< T >( x );
    }

    template< typename ... Ts >
    bool is_one_of(Operation *op, tl::TL< Ts ... >) { return is_one_of< Ts ... >(op); }

    template< typename T, typename ... Ts >
    void collect_kinds(std::unordered_set< Operation::kind_t > &seen)
    {
        seen.insert(T::kind);
        if constexpr (sizeof ... (Ts) != 0)
            return collect_kinds< Ts ... >( seen );
    }

    template< typename ... Ts >
    std::unordered_set< Operation::kind_t > collect_kinds()
    {
        std::unordered_set< Operation::kind_t > out;
        collect_kinds< Ts ... >( out );
        return out;
    }

    namespace tag
    {
        struct external {};
        struct external_io : external {};
        struct external_input : external_io {};
        struct external_output : external_io {};

        struct external_computation : external {};
    };

    /* Leaves */

    template< class Next, Operation::kind_t k >
    struct Input : Next
    {
        using parent_t = Next;
        using parent_t::parent_t;

        static inline constexpr Operation::kind_t kind = k;

        template< typename ... Args >
            requires (!util::is_copy_ctor_of< Input< Next, k >, Args ... >)
        explicit Input(Args && ... args) : Next(kind, std::forward< Args >(args) ... ) {}

        explicit Input(const Input< Next, k > & ) = default;

        static std::string op_code_str() { return "in." + parent_t::op_code_str(); }
        std::string name() const override { return "In." + parent_t::name(); }
    };

    template< class Next, Operation::kind_t k >
    struct Output : Next
    {
        using parent_t = Next;
        using parent_t::parent_t;

        static inline constexpr Operation::kind_t kind = k;

        template< typename ... Args >
            requires (!util::is_copy_ctor_of< Output< Next, k >, Args ... >)
        explicit Output(Args && ... args) : Next(kind, std::forward< Args >(args) ... ) {}

        explicit Output(const Output< Next, k > &) = default;

        static std::string op_code_str() { return "out." + parent_t::op_code_str(); }
        std::string name() const override { return "Out." + parent_t::name(); }
    };

    /* I/O & Leaf nodes */

    struct Register : Operation
    {
      protected:

        explicit Register(Operation::kind_t kind, const std::string &rn_, uint32_t size_)
            : Operation(size_, kind), reg_name(rn_)
        {}
        explicit Register(const Register &) = default;

      public:

        static std::string op_code_str() { return "register"; }
        std::string name() const override { return make_name(reg_name); }

        static std::string make_name(const std::string &reg)
        {
            return op_code_str() + "." + reg;
        }

        std::string reg_name;
    };

    using InputRegister = Input< Register, Operation::kind_t::kInputRegister >;
    using OutputRegister = Output< Register, Operation::kind_t::kOutputRegister >;

    struct ErrorFlag : Operation
    {
      protected:
        explicit ErrorFlag(Operation::kind_t kind, uint32_t size_) : Operation(size_, kind) {}

      public:

        static std::string op_code_str() { return "error_flag"; }
        std::string name() const override { return "error_flag"; }
    };

    using InputErrorFlag = Input< ErrorFlag, Operation::kind_t::kInputErrorFlag >;
    using OutputErrorFlag = Output< ErrorFlag, Operation::kind_t::kOutputErrorFlag >;

    struct Timestamp : Operation
    {
      protected:
        explicit Timestamp(Operation::kind_t kind, uint32_t size_) : Operation(size_, kind) {}

      public:

        static std::string op_code_str() { return "timestamp"; }
        std::string name() const override { return "timestamp"; }
    };

    using InputTimestamp = Input< Timestamp, Operation::kind_t::kInputTimestamp >;
    using OutputTimestamp = Output< Timestamp, Operation::kind_t::kOutputTimestamp >;

    struct SyscallState : Operation, tag::external_io
    {
      protected:
        explicit SyscallState(Operation::kind_t kind, uint32_t size) : Operation(size, kind) {}

      public:

        static std::string op_code_str() { return "syscall_state"; }
        std::string name() const override { return "syscall_state"; }
    };

    using InputSyscallState = Input< SyscallState, Operation::kind_t::kInputSyscallState >;
    using OutputSyscallState = Output< SyscallState, Operation::kind_t::kOutputSyscallState >;

    struct SyscallReg : Operation, tag::external_io
    {
      protected:

        explicit SyscallReg( Operation::kind_t kind, const std::string &reg_name,
                             uint32_t size )
            : Operation( size, kind ), reg_name( reg_name )
        {}

      public:

        static std::string op_code_str() { return "syscall_reg"; }
        std::string name() const override { return make_name(reg_name); }

        static std::string make_name(const std::string &reg)
        {
            return op_code_str() + "." + reg;
        }

        std::string reg_name;
    };

    using InputSyscallReg = Input< SyscallReg, Operation::kind_t::kInputSyscallReg >;
    using OutputSyscallReg = Input< SyscallReg, Operation::kind_t::kOutputSyscallReg >;

    // An undefined value.
    struct Undefined final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kUndefined;

        explicit Undefined(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "undefined"; }
        std::string name() const override { return "undefined"; }
    };

    struct Memory : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kMemory;

        explicit Memory(unsigned size_, uint32_t mem_idx_)
            : Operation(size_, kind), mem_idx(mem_idx_) {}

        static uint32_t  expected_size(uint32_t ptr_size);

        static std::string op_code_str() { return "memory"; }
        std::string name() const override { return "memory." + std::to_string(mem_idx); }

        uint32_t mem_idx = 0;
    };

    struct Constant final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kConstant;

        explicit Constant(std::string bits_, unsigned size_)
            : Operation(size_, kind),
            bits(std::move(bits_))
        {}

        static std::string op_code_str() { return "constant"; }
        std::string name() const override;

        // Value of this constant. The least significant bit is stored in `bits[0]`,
        // and the most significant bit is stored in `bits[size - 1u]`.
        const std::string bits;
    };

    struct Advice final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kAdvice;

        inline explicit Advice(unsigned size_, std::size_t advice_idx_)
            : Operation(size_, kind), advice_idx(advice_idx_)
        {}

        static std::string op_code_str() { return "Advice"; }
        std::string name() const override { return "Advice." + std::to_string(advice_idx) ; }

        std::size_t advice_idx = 0;
    };

    // Input bits that
    struct InputInstructionBits : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kInputInstructionBits;

        explicit InputInstructionBits(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "instruction_bits"; }
        std::string name() const override { return "instruction_bits"; }
    };

    using input_leaves_ts = tl::make_list<
        InputInstructionBits, Advice, Memory,
        InputTimestamp, InputErrorFlag, InputRegister,
        InputSyscallState, InputSyscallReg
    >;

    using output_leaves_ts = tl::make_list<
        OutputTimestamp, OutputErrorFlag, OutputRegister,
        OutputSyscallState, OutputSyscallReg
    >;

    using nontrace_leaves_ts = tl::make_list<
        Constant, Undefined
    >;

    using leaf_values_ts = tl::merge< input_leaves_ts, output_leaves_ts, nontrace_leaves_ts >;

    /* Constaints */

    struct EnforceCtx : Operation
    {
        using Operation::Operation;
        enum : uint8_t { kDynamic = 0u, kFixed = 1u };

        Operation *dynamic() { return _operands[kDynamic]; }
        Operation *fixed() { return _operands[kFixed]; }

        const Operation *dynamic() const { return _operands[kDynamic]; }
        const Operation *fixed() const { return _operands[kFixed]; }

        Operation *variable() const { return _operands[ kFixed ]; }
        Operation *value() const { return _operands[ kDynamic ]; }

        std::string suffix_() const
        {
            if (_operands.size() != 2)
                return "invalid.0";
            return fixed()->name() + "." + std::to_string(fixed()->size);
        }
    };

    struct MemoryConstraint : Operation
    {
        using Operation::Operation;

        enum : uint8_t { kFixed = 0u, kSize = 1u, kAddr = 2u, kTS = 3u, kValue = 4u };

        auto hint_arg() const  { return _operands[kFixed]; }
        auto size_arg() const { return _operands[kSize]; }
        auto addr_arg() const { return _operands[kAddr]; }
        auto ts_arg() const { return _operands[kTS]; }
        auto val_arg() const { return _operands[kValue]; }
        auto mem_idx() const
        {
            return dynamic_cast< Memory * >( hint_arg() )->mem_idx;
        }
    };

    // A comparison between the proposed output value of a register, and the
    // output register itself.
    struct RegConstraint final : EnforceCtx
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kRegConstraint;

        explicit RegConstraint() : EnforceCtx(this->bool_size, kind) {}
        explicit RegConstraint(unsigned size) : EnforceCtx(size, kind) {}

        static std::string op_code_str() { return "register_constraint"; }
        std::string name() const override
        {
            return "register_constraint." + EnforceCtx::suffix_();
        }
    };

    // A comparison between the proposed output value of a register, and the
    // output register itself.
    struct AdviceConstraint final : EnforceCtx
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kAdviceConstraint;

        explicit AdviceConstraint() : EnforceCtx(this->bool_size, kind) {}
        explicit AdviceConstraint(unsigned size) : EnforceCtx(size, kind) {}

        static std::string op_code_str() { return "advice_constraint"; }
        std::string name() const override
        {
            return "advice_constraint." + EnforceCtx::suffix_();
        }

        Advice *advice() { return dyn_cast< Advice >( this->EnforceCtx::fixed() ); }
        Operation *runtime_value() { return this->EnforceCtx::dynamic(); }

        auto ctx_conds() -> gap::generator< Operation * >;
    };

    struct WriteConstraint : MemoryConstraint
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kWriteConstraint;

        explicit WriteConstraint() : MemoryConstraint(this->bool_size, kind) {}
        explicit WriteConstraint(unsigned size) : MemoryConstraint(size, kind) {}

        static std::string op_code_str() { return "write_constraint"; }
        std::string name() const override { return "write_constraint"; }
    };

    struct ReadConstraint : MemoryConstraint
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kReadConstraint;

        explicit ReadConstraint() : MemoryConstraint(this->bool_size, kind) {}
        explicit ReadConstraint(unsigned size) : MemoryConstraint(size, kind) {}

        static std::string op_code_str() { return "read_constraint"; }
        std::string name() const override { return "read_constraint"; }

        Operation *val_arg() const
        {
            unreachable() << "There is no `val_arg` in read_constraint.";
        }
    };

    struct UnusedConstraint : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kUnusedConstraint;

        explicit UnusedConstraint() : Operation(this->bool_size, kind) {}
        explicit UnusedConstraint(unsigned size) : Operation(size, kind) {}

        static std::string op_code_str() { return "unused_constraint"; }
        std::string name() const override { return "unused_constraint"; }
    };

    using constraint_opts_ts = tl::TL< RegConstraint, AdviceConstraint,
                                       WriteConstraint, ReadConstraint, UnusedConstraint >;

    // TODO(lukas): It would be nice to move these to struct defs
    static inline bool constrained_by(Operation *v, Operation *c)
    {
        switch (v->op_code) {
            case OutputErrorFlag::kind:
            case OutputTimestamp::kind:
            case OutputRegister::kind :
                return is_one_of< RegConstraint >(c);
            case Advice::kind : return is_one_of<AdviceConstraint>(c);
            case Memory::kind :
                return is_one_of<ReadConstraint, WriteConstraint, UnusedConstraint>(c);
            default: return true;
        }
    }

    /* LLVMOP */

    #define circuitous_declare_llvm_op(cls, idx) \
    struct cls final : Operation \
    { \
        static constexpr Operation::kind_t kind = Operation::kind_t::k##cls; \
        explicit cls(unsigned size_) : Operation(size_, kind) {} \
        static std::string op_code_str() { return #cls; } \
        std::string name() const override { return #cls; } \
    }

    circuitous_declare_llvm_op(Add, 0);
    circuitous_declare_llvm_op(Sub, 1);
    circuitous_declare_llvm_op(Mul, 2);

    circuitous_declare_llvm_op(UDiv, 3);
    circuitous_declare_llvm_op(SDiv, 4);

    circuitous_declare_llvm_op(Shl, 5);
    circuitous_declare_llvm_op(LShr, 6);
    circuitous_declare_llvm_op(AShr, 7);

    circuitous_declare_llvm_op(Trunc, 8);
    circuitous_declare_llvm_op(ZExt, 9);
    circuitous_declare_llvm_op(SExt, 10);

    circuitous_declare_llvm_op(Icmp_ult, 11);
    circuitous_declare_llvm_op(Icmp_slt, 12);
    circuitous_declare_llvm_op(Icmp_ugt, 13);
    circuitous_declare_llvm_op(Icmp_eq, 14);
    circuitous_declare_llvm_op(Icmp_ne, 15);
    circuitous_declare_llvm_op(Icmp_uge, 16);
    circuitous_declare_llvm_op(Icmp_ule, 17);
    circuitous_declare_llvm_op(Icmp_sgt, 18);
    circuitous_declare_llvm_op(Icmp_sge, 19);
    circuitous_declare_llvm_op(Icmp_sle, 20);

    circuitous_declare_llvm_op(SRem, 21);
    circuitous_declare_llvm_op(URem, 22);

    circuitous_declare_llvm_op(Xor, 23);
    circuitous_declare_llvm_op(And, 24);
    circuitous_declare_llvm_op(Or, 25);

    #undef circuitous_declare_llvm_op

    using llvm_ops_t = tl::TL<
        Add, Sub, Mul, UDiv, SDiv, Shl, LShr, AShr, Trunc, ZExt, SExt,
        Icmp_ult, Icmp_slt, Icmp_ugt, Icmp_eq, Icmp_ne, Icmp_uge, Icmp_ule,
        Icmp_sgt, Icmp_sge, Icmp_sle,
        SRem, URem,
        Xor, And, Or
    >;

    /* Hidden */

    struct InputImmediate : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kInpuImmediate;

        explicit InputImmediate(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "input_immediate"; }
        std::string name() const override { return "input_immediate"; }
    };

    using hidden_values_ts = tl::TL< InputImmediate >;

    /* BitManips */

    struct Extract final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kExtract;

        static std::string op_code_str() { return "extract"; }

        std::string name() const override
        {
          std::stringstream ss;
          ss << "extract." << high_bit_exc << "." << low_bit_inc;
          return ss.str();
        }


        uint32_t extracted_size() const { return high_bit_exc - low_bit_inc; }

        explicit Extract(uint32_t low_bit_inc_, uint32_t high_bit_exc_)
            : Operation(high_bit_exc_ - low_bit_inc_, kind),
              low_bit_inc(low_bit_inc_),
              high_bit_exc(high_bit_exc_)
        {}

        const uint32_t low_bit_inc;
        const uint32_t high_bit_exc;
    };


    struct Concat final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kConcat;

        explicit Concat(uint32_t size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "concat"; }
        std::string name() const override { return "concat"; }
    };

    using bit_manips_ts = tl::TL< Extract, Concat >;

    /* BitOps */

    struct PopulationCount final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kPopulationCount;

        explicit PopulationCount(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "pop_count"; }
        std::string name() const override { return "pop_count"; }
    };

    struct CountLeadingZeroes final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCountLeadingZeroes;

        explicit CountLeadingZeroes(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "count_lead_zeroes"; }
        std::string name() const override { return "count_lead_zeroes"; }
    };

    struct CountTrailingZeroes final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCountTrailingZeroes;

        explicit CountTrailingZeroes(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "count_trailing_zeroes"; }
        std::string name() const override { return "count_trailing_zeroes"; }
    };

    struct Not final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kNot;

        explicit Not(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "not"; }
        std::string name() const override { return "not"; }
    };

    struct Parity final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kParity;

        explicit Parity() : Operation(this->bool_size, kind) {}
        explicit Parity(unsigned size) : Operation(size, kind) {}

        static std::string op_code_str() { return "parity"; }
        std::string name() const override { return "parity"; }
    };

    using bit_ops_ts = tl::TL< PopulationCount, CountLeadingZeroes,
                               CountTrailingZeroes, Not, Parity >;

    /* Without category */

    struct Select : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kSelect;

        explicit Select(uint32_t bits_, uint32_t size_)
            : Operation(size_, kind), bits(bits_)
        {}

        static std::string op_code_str() { return "select"; }
        std::string name() const override
        {
            std::stringstream ss;
            ss << "select." << bits;
            return ss.str();
        }

        bool is_extension_of( const Select *other ) const;
        bool can_be_extended_to( const Select *other ) const;

        Operation *selector() { return _operands[0]; }

        // Return one of the `2 ^ bits` values. It is also expected that this node
        // has `2 ^ bits + 1` operands.
        uint32_t bits = 0;
    };

    struct Option : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kOption;

        explicit Option(uint32_t size) : Operation(size, kind) {}

        static std::string op_code_str() { return "option"; }
        std::string name() const override
        {
            return op_code_str() + "." + std::to_string(size);
        }

        Operation *value()
        {
            check( operands_size() >= 1 );
            return operand( 0 );
        }

        gap::generator< Operation * > conditions()
        {
            check( operands_size() >= 1 );
            auto range = operands();
            for ( auto it = ++(range.begin()); it != range.end(); ++it)
                co_yield *it;
        }
    };

    // Main purpose of this operation is to model mux.
    struct Switch : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kSwitch;

        explicit Switch(uint32_t size) : Operation(size, kind) {}

        static std::string op_code_str() { return "switch"; }
        std::string name() const override
        {
            std::stringstream ss;
            ss << "switch." << size;
            return ss.str();
        }

        std::optional< Option * > option( Operation *value )
        {
            for ( auto op : dyn_cast< Option >( operands() ) )
            {
                check( op );
                if ( op->value() == value )
                    return { op };
            }
            return {};
        }
    };


    struct SyscallModule : Operation, tag::external_computation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kSyscallModule;

        explicit SyscallModule() : Operation( 1u, kind ) {}

        static std::string op_code_str() { return "syscall_module"; }
        std::string name() const override { return "syscall_module"; }
    };

    using uncategorized_ops_ts = tl::TL< Select, Switch, Option, SyscallModule >;

    #define circuitous_make_bool_op(cls, idx) \
    struct cls final : Operation \
    { \
      static constexpr Operation::kind_t kind = Operation::kind_t::k##cls; \
      explicit cls() : Operation(this->bool_size, kind) {} \
      explicit cls(unsigned size) : Operation(size, kind) {} \
      static std::string op_code_str() { return #cls; } \
      std::string name() const override { return #cls; } \
    }

    circuitous_make_bool_op(DecodeCondition, 0);
    circuitous_make_bool_op(OnlyOneCondition, 3);
    circuitous_make_bool_op(DecoderResult, 5);

    #undef circuitous_make_bool_op

    struct DecoderResult;

    struct VerifyInstruction : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kVerifyInstruction;

        explicit VerifyInstruction( unsigned size ) : Operation( size, kind ) {}
        explicit VerifyInstruction() : VerifyInstruction( Operation::bool_size ) {}

        static std::string op_code_str() { return "VerifyInstruction"; }
        std::string name() const override { return op_code_str(); }

        std::optional< DecoderResult * > decoder();
    };

    using bool_ops_ts = tl::TL< DecodeCondition, VerifyInstruction, OnlyOneCondition,
                                DecoderResult
                              >;

    using generic_list_t =
    tl::TL<
        Not, Concat,
        CountLeadingZeroes, CountTrailingZeroes, Extract, PopulationCount,
        Parity, InputImmediate,
        DecodeCondition,
        DecoderResult,
        VerifyInstruction, OnlyOneCondition
    >;

    using collapsable = tl::TL< Concat, And, Or >;

    using all_nodes_list_t = tl::merge< generic_list_t, llvm_ops_t,
                                        leaf_values_ts,
                                        constraint_opts_ts,
                                        uncategorized_ops_ts >;
}  // namespace circ
