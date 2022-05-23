/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.h>
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
    // A general instruction.
    struct Operation : public Node< Operation >, HasStringMeta
    {
      public:
        enum class kind_t : uint32_t
        {
            kFirst = 0,
            kOperation = 1,
            kCircuit = 2,

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
            kPreservedConstraint,
            kCopyConstraint,
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

            kSelect,

            // Condition
            kDecodeCondition,
            kDecoderResult,
            kOnlyOneCondition,
            kVerifyInstruction,
            kLast
        };

        static constexpr kind_t kind = kind_t::kOperation;
        static constexpr uint32_t bool_size = 1u;

        virtual ~Operation() = default;

        virtual std::string Name() const;
        static std::string op_code_str() { not_implemented(); }
        virtual bool Equals(const Operation *that) const;

        auto &operator[](std::size_t idx) { return operands[idx]; }
        const auto &operator[](std::size_t idx) const { return operands[idx]; }

        // `id` should be unique in a given circuit.
        uint64_t id() const { return _id; }


        // Size in bits of this instruction's "result" value. For example, a zero-
        // extension will represent the size of the output value.
        const unsigned size;

        // The "opcode" of this.
        const kind_t op_code;

        // Must be set manually after ctor is called.
        uint64_t _id = 0;


        template< typename Vis >
        void Traverse(Vis &vis)
        {
            for (auto op : operands)
                vis.Dispatch(op);
        }

      protected:
        // Please note, that id is not set.
        explicit Operation(unsigned size_, Operation::kind_t op_code_)
            : size(size_), op_code(op_code_)
        {}
    };

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

    template< class O >
    concept is_operation_type = std::is_base_of_v< Operation, O >;

    template< class T >
    concept is_type_list = requires (T t)
    {
        { tl::TL{ t } } -> std::same_as< T >;
    };


    template< typename T >
    bool isa(Operation::kind_t rkind) { return T::kind == rkind; }

    template< typename T >
    bool isa(Operation *op) { return isa< T >(op->op_code); }

    template< typename TypeList >
    bool is_in(Operation::kind_t rkind)
    {
        auto accept = [&]< typename T >() { return isa< T >(rkind); };
        return tl::contains< TypeList >(accept);
    }

    template< typename T > requires (is_type_list< T >)
    bool isa(Operation::kind_t rkind) { return is_in< T >(rkind); }

    template< typename ...Ts >
    bool is_one_of(Operation *op)
    {
        return (isa< Ts >(op->op_code) || ...);
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

    /* Leaves */

    template< class Next, Operation::kind_t k >
    struct Input : Next
    {
        using parent_t = Next;
        using parent_t::parent_t;

        static inline constexpr Operation::kind_t kind = k;

        template< typename ... Args >
            requires (!util::is_copy_ctor_of< Input< Next, k >, Args ... >)
        Input(Args && ... args) : Next(kind, std::forward< Args >(args) ... ) {}

        static std::string op_code_str() { return "in." + parent_t::op_code_str(); }
        std::string Name() const override { return "In." + parent_t::Name(); }
        std::string raw_name() const { return parent_t::Name(); }
    };

    template< class Next, Operation::kind_t k >
    struct Output : Next
    {
        using parent_t = Next;
        using parent_t::parent_t;

        static inline constexpr Operation::kind_t kind = k;

        template< typename ... Args >
            requires (!util::is_copy_ctor_of< Output< Next, k >, Args ... >)
        Output(Args && ... args) : Next(kind, std::forward< Args >(args) ... ) {}

        Output(const Output &) = default;

        static std::string op_code_str() { return "out." + parent_t::op_code_str(); }
        std::string Name() const override { return "Out." + parent_t::Name(); }
        std::string raw_name() const { return parent_t::Name(); }
    };

    /* I/O & Leaf nodes */

    struct Register : Operation
    {
      protected:

        Register(Operation::kind_t kind, const std::string &rn_, uint32_t size_)
            : Operation(size_, kind), reg_name(rn_)
        {}
        Register(const Register &) = default;

      public:

        static std::string op_code_str() { return "register"; }
        std::string Name() const override { return "register." + reg_name; }
        bool Equals(const Operation *other) const override { not_implemented(); }

        std::string reg_name;
    };

    using InputRegister = Input< Register, Operation::kind_t::kInputRegister >;
    using OutputRegister = Output< Register, Operation::kind_t::kOutputRegister >;

    struct ErrorFlag : Operation
    {
      protected:
        ErrorFlag(Operation::kind_t kind, uint32_t size_) : Operation(size_, kind) {}

      public:

        static std::string op_code_str() { return "error_flag"; }
        std::string Name() const override { return "error_flag"; }
    };

    using InputErrorFlag = Input< ErrorFlag, Operation::kind_t::kInputErrorFlag >;
    using OutputErrorFlag = Output< ErrorFlag, Operation::kind_t::kOutputErrorFlag >;

    struct Timestamp : Operation
    {
      protected:
        Timestamp(Operation::kind_t kind, uint32_t size_) : Operation(size_, kind) {}
      public:

        static std::string op_code_str() { return "timestamp"; }
        std::string Name() const override { return "timestamp"; }
    };

    using InputTimestamp = Input< Timestamp, Operation::kind_t::kInputTimestamp >;
    using OutputTimestamp = Output< Timestamp, Operation::kind_t::kOutputTimestamp >;

    // An undefined value.
    struct Undefined final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kUndefined;

        explicit Undefined(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "undefined"; }
        std::string Name() const override { return "undefined"; }
    };

    struct Memory : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kMemory;

        explicit Memory(unsigned size_, uint32_t mem_idx_)
            : Operation(size_, kind), mem_idx(mem_idx_) {}

        static uint32_t  expected_size(uint32_t ptr_size);

        static std::string op_code_str() { return "memory"; }
        std::string Name() const override { return "memory." + std::to_string(mem_idx); }

        uint32_t mem_idx = 0;
    };

    struct Constant final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kConstant;

        explicit Constant(std::string bits_, unsigned size_)
            : Operation(size_, kind),
            bits(std::move(bits_))
        {}

        bool Equals(const Operation *that) const override;
        static std::string op_code_str() { return "constant"; }
        std::string Name() const override;

        // Value of this constant. The least significant bit is stored in `bits[0]`,
        // and the most significant bit is stored in `bits[size - 1u]`.
        const std::string bits;
    };

    struct Advice final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kAdvice;

        inline explicit Advice(unsigned size_, uint32_t advice_idx_)
            : Operation(size_, kind), advice_idx(advice_idx_)
        {}

        static std::string op_code_str() { return "Advice"; }
        std::string Name() const override { return "Advice." + std::to_string(advice_idx) ; }

        uint32_t advice_idx = 0;
    };

    // Input bits that
    struct InputInstructionBits : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kInputInstructionBits;

        explicit InputInstructionBits(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "instruction_bits"; }
        std::string Name() const override { return "instruction_bits"; }
    };

    using input_leaves_ts = tl::make_list<
        InputInstructionBits, Advice, Memory,
        InputTimestamp, InputErrorFlag, InputRegister
    >;

    using output_leaves_ts = tl::make_list<
        OutputTimestamp, OutputErrorFlag, OutputRegister
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

        Operation *dynamic() { return operands[kDynamic]; }
        Operation *fixed() { return operands[kFixed]; }

        const Operation *dynamic() const { return operands[kDynamic]; }
        const Operation *fixed() const { return operands[kFixed]; }

        std::string suffix_() const
        {
            if (operands.size() != 2)
                return "invalid.0";
            return fixed()->Name() + "." + std::to_string(fixed()->size);
        }
    };

    struct MemoryConstraint : Operation
    {
        using Operation::Operation;

        enum : uint8_t { kFixed = 0u, kSize = 1u, kAddr = 2u, kTS = 3u, kValue = 4u };

        auto hint_arg() const  { return operands[kFixed]; }
        auto size_arg() const { return operands[kSize]; }
        auto addr_arg() const { return operands[kAddr]; }
        auto ts_arg() const { return operands[kTS]; }
        auto val_arg() const { return operands[kValue]; }
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

        RegConstraint() : EnforceCtx(this->bool_size, kind) {}

        static std::string op_code_str() { return "register_constraint"; }
        std::string Name() const override
        {
            return "register_constraint." + EnforceCtx::suffix_();
        }
    };

    // A comparison between the proposed output value of a register, and the
    // output register itself.
    struct AdviceConstraint final : EnforceCtx
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kAdviceConstraint;

        AdviceConstraint() : EnforceCtx(this->bool_size, kind) {}

        static std::string op_code_str() { return "advice_constraint"; }
        std::string Name() const override
        {
            return "advice_constraint." + EnforceCtx::suffix_();
        }
    };

    // Says that we are preserving the value of a register.
    struct PreservedConstraint final : EnforceCtx
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kPreservedConstraint;

        PreservedConstraint() : EnforceCtx(this->bool_size, kind) {}

        static std::string op_code_str() { return "preserved_constraint"; }
        std::string Name() const override
        {
            return "preserved_constraint." + EnforceCtx::suffix_();
        }
    };

    // Says that we are moving one register to a different register.
    struct CopyConstraint final : EnforceCtx
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCopyConstraint;

        CopyConstraint() : EnforceCtx(this->bool_size, kind) {}

        static std::string op_code_str() { return "copy_constraint"; }
        std::string Name() const override { return "copy_constraint." + EnforceCtx::suffix_(); }
    };

    struct WriteConstraint : MemoryConstraint
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kWriteConstraint;

        WriteConstraint() : MemoryConstraint(this->bool_size, kind) {}

        static std::string op_code_str() { return "write_constraint"; }
        std::string Name() const override { return "write_constraint"; }
    };

    struct ReadConstraint : MemoryConstraint
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kReadConstraint;

        ReadConstraint() : MemoryConstraint(this->bool_size, kind) {}

        static std::string op_code_str() { return "read_constraint"; }
        std::string Name() const override { return "read_constraint"; }

        Operation *val_arg() const
        {
            unreachable() << "There is no `val_arg` in read_constraint.";
        }
    };

    struct UnusedConstraint : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kUnusedConstraint;

        UnusedConstraint() : Operation(this->bool_size, kind) {}

        static std::string op_code_str() { return "unused_constraint"; }
        std::string Name() const override { return "unused_constraint"; }
    };

    using constraint_opts_ts = tl::TL< RegConstraint, AdviceConstraint,
                                       PreservedConstraint, CopyConstraint,
                                       WriteConstraint, RegConstraint, UnusedConstraint >;

    // TODO(lukas): It would be nice to move these to struct defs
    static inline bool constrained_by(Operation *v, Operation *c)
    {
        switch (v->op_code) {
            case OutputErrorFlag::kind:
            case OutputTimestamp::kind:
            case OutputRegister::kind :
                return is_one_of<RegConstraint, PreservedConstraint, CopyConstraint>(c);
            case Advice::kind : return is_one_of<AdviceConstraint>(c);
            case Memory::kind :
                return is_one_of<ReadConstraint, WriteConstraint, UnusedConstraint>(c);
            default: return true;
        }
    }

    /* LLVMOP */

    #define declare_llvm_op(cls, idx) \
    struct cls final : Operation \
    { \
        static constexpr Operation::kind_t kind = Operation::kind_t::k##cls; \
        cls(unsigned size_) : Operation(size_, kind) {} \
        static std::string op_code_str() { return #cls; } \
        std::string Name() const override { return #cls; } \
    }

    declare_llvm_op(Add, 0);
    declare_llvm_op(Sub, 1);
    declare_llvm_op(Mul, 2);

    declare_llvm_op(UDiv, 3);
    declare_llvm_op(SDiv, 4);

    declare_llvm_op(Shl, 5);
    declare_llvm_op(LShr, 6);
    declare_llvm_op(AShr, 7);

    declare_llvm_op(Trunc, 8);
    declare_llvm_op(ZExt, 9);
    declare_llvm_op(SExt, 10);

    declare_llvm_op(Icmp_ult, 11);
    declare_llvm_op(Icmp_slt, 12);
    declare_llvm_op(Icmp_ugt, 13);
    declare_llvm_op(Icmp_eq, 14);
    declare_llvm_op(Icmp_ne, 15);
    declare_llvm_op(Icmp_uge, 16);
    declare_llvm_op(Icmp_ule, 17);
    declare_llvm_op(Icmp_sgt, 18);
    declare_llvm_op(Icmp_sge, 19);
    declare_llvm_op(Icmp_sle, 20);

    declare_llvm_op(SRem, 21);
    declare_llvm_op(URem, 22);

    declare_llvm_op(Xor, 23);
    declare_llvm_op(And, 24);
    declare_llvm_op(Or, 25);

    #undef declare_llvm_op

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

        static std::string op_code_str() { return "input_immediate"; }
        std::string Name() const override { return "input_immediate"; }
        bool Equals(const Operation *that) const override;

        explicit InputImmediate(unsigned size_) : Operation(size_, kind) {}
    };

    using hidden_values_ts = tl::TL< InputImmediate >;

    /* BitManips */

    struct Extract final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kExtract;

        static std::string op_code_str() { return "extract"; }
        std::string Name() const override
        {
          std::stringstream ss;
          ss << "extract." << high_bit_exc << "." << low_bit_inc;
          return ss.str();
        }

        bool Equals(const Operation *that) const override;

        uint32_t extracted_size() const { return high_bit_exc - low_bit_inc; }

        inline explicit Extract(uint32_t low_bit_inc_, uint32_t high_bit_exc_)
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

        Concat(uint32_t size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "concat"; }
        std::string Name() const override { return "concat"; }
    };

    using bit_manips_ts = tl::TL< Extract, Concat >;

    /* BitOps */

    struct PopulationCount final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kPopulationCount;

        explicit PopulationCount(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "pop_count"; }
        std::string Name() const override { return "pop_count"; }
    };

    struct CountLeadingZeroes final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCountLeadingZeroes;

        explicit CountLeadingZeroes(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "count_lead_zeroes"; }
        std::string Name() const override { return "count_lead_zeroes"; }
    };

    struct CountTrailingZeroes final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCountTrailingZeroes;

        explicit CountTrailingZeroes(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "count_trailing_zeroes"; }
        std::string Name() const override { return "count_trailing_zeroes"; }
    };

    struct Not final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kNot;

        explicit Not(unsigned size_) : Operation(size_, kind) {}

        static std::string op_code_str() { return "not"; }
        std::string Name() const override { return "not"; }
    };

    struct Parity final : Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kParity;

        explicit Parity() : Operation(1, kind) {}

        static std::string op_code_str() { return "parity"; }
        std::string Name() const override { return "parity"; }
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
        std::string Name() const override
        {
            std::stringstream ss;
            ss << "select." << bits;
            return ss.str();
        }

        Operation *selector() { return operands[0]; }

        // Return one of the `2 ^ bits` values. It is also expected that this node
        // has `2 ^ bits + 1` operands.
        uint32_t bits = 0;
    };

    using uncategorized_ops_ts = tl::TL< Select >;

    #define make_bool_op(cls, idx) \
    struct cls final : Operation \
    { \
      static constexpr Operation::kind_t kind = Operation::kind_t::k##cls; \
      cls() : Operation(this->bool_size, kind) {} \
      static std::string op_code_str() { return #cls; } \
      std::string Name() const override { return #cls; } \
    }

    make_bool_op(DecodeCondition, 0);
    make_bool_op(VerifyInstruction, 1);
    make_bool_op(OnlyOneCondition, 3);
    make_bool_op(DecoderResult, 5);

    #undef make_bool_op

    using bool_ops_ts = tl::TL< DecodeCondition, VerifyInstruction, OnlyOneCondition,
                                DecoderResult >;

    using generic_list_t =
    tl::TL<
        Not, Concat,
        CountLeadingZeroes, CountTrailingZeroes, Extract, PopulationCount,
        Parity, InputImmediate,
        RegConstraint, PreservedConstraint, CopyConstraint, DecodeCondition,
        DecoderResult,
        ReadConstraint, WriteConstraint, UnusedConstraint,
        VerifyInstruction, OnlyOneCondition,
        AdviceConstraint, Select
    >;

    using subnode_list_t = tl::merge< generic_list_t, llvm_ops_t, leaf_values_ts >;
}  // namespace circ
