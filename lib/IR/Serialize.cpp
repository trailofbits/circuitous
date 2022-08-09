/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Printers.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <gap/core/ranges.hpp>

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ {
namespace {

    struct FileConfig {
        enum class Selector : uint8_t
        {
            Operation = 0x0,
            LeafOperation = 0x1,
            Metadatum = 0x3,
            Invalid = 0x4,
            Reference = 0xff
        };

        using raw_op_code_t = std::underlying_type_t< Operation::kind_t >;
        using raw_id_t = uint64_t;

        static std::string to_string(const Selector &sel)
        {
            switch(sel) {
                case Selector::Operation :     return "Operation";
                case Selector::LeafOperation : return "LeafOperation";
                case Selector::Invalid   :     return "Invalid";
                case Selector::Metadatum :     return "Metadatum";
                case Selector::Reference :     return "Reference";
            }
        }

        static bool is_valid_sel(const Selector &sel)
        {
            switch(sel)
            {
                case Selector::Invalid: return false;
                default:                return true;
            }
        }

        static bool is_op_definition(const Selector &sel)
        {
            return sel == Selector::Operation || sel == Selector::LeafOperation;
        }
    };

    struct SerializeVisitor : public Visitor< SerializeVisitor >, FileConfig
    {
        using Selector = FileConfig::Selector;

        std::ostream &os;
        std::unordered_set< uint64_t > written;

        ~SerializeVisitor()
        {
            os.flush();
        }

        explicit SerializeVisitor(std::ostream &os_) : os(os_) {}

        void serialize(Operation *op) { return Write(op); }

        Selector get_selector(Operation *op)
        {
            if (op->operands_size() == 0)
                return Selector::LeafOperation;
            return Selector::Operation;
        }

        void Write(Operation *op)
        {
            if (!written.count(op->id()))
                return write_new_entry(op);
            return write_reference(op);
        }

        void write_new_entry(Operation *op)
        {
            auto sel = get_selector(op);
            // Write generic for all.
            Write(sel);
            Write(op->id());
            Write(op->op_code);
            written.insert(op->id());
            // Write special attributes based on runtime type.
            this->dispatch(op);

            // If the operation is not a leaf, write operands.
            if (sel != Selector::LeafOperation)
            {
                Write(static_cast< std::size_t >(op->operands_size()));
                Write(op->operands());
            }
        }

        void write_reference(Operation *op)
        {
            Write(Selector::Reference);
            Write< raw_id_t >(op->id());
        }

        void Write(Selector sel)
        {
            check(is_valid_sel(sel), [&]() { return "Trying to write invalid selector!"; });
            Write(static_cast< std::underlying_type_t< Selector > >(sel));
        }

        void Write(Operation::kind_t kind)
        {
            Write(util::to_underlying(kind));
        }

        void Write(const std::string &str)
        {
            Write< uint32_t >(static_cast< uint32_t >(str.size()));
            for (auto ch : str)
                Write(ch);
        }

        template< typename I > requires (std::is_integral_v< I >)
        void Write(I data)
        {
            auto bytes = reinterpret_cast< const uint8_t * >(&data);
            for (auto i = 0ull; i < sizeof(data); ++i)
                os << (static_cast< uint8_t >(bytes[i]));
        }

        template< gap::ranges::range Ops >
        void Write(Ops &&ops)
        {
            for (auto op : ops)
                Write(op);
        }

        template< typename ...Args >
        void write(Args &&... args) { (Write(std::forward< Args >(args)), ...); }

        void write_metadata(Operation *op)
        {
            // TODO(lukas): Was excluded (most likely dead node). However, we want to serialize
            //              *all* nodes, rewrite as check once serializer is fixed.
            if (!written.count(op->id()))
                return;
            for (auto &[key, val] : op->meta)
            {
                Write(Selector::Metadatum);
                Write(op->id());
                Write(key);
                Write(val);
            }
        }

        // TODO(lukas): This should be called, but is not.
        void visit(Circuit *op) { write(op->ptr_size);  }

        void visit(InputRegister *op) { write(op->reg_name, op->size); }
        void visit(OutputRegister *op) { write(op->reg_name, op->size); }

        void visit(Operation *op) { write(op->size); }
        void visit(Constant *op) { write(op->size, op->bits); }
        void visit(InputImmediate *op) { write(op->size); }

        void visit(Extract *op) { write(op->high_bit_exc, op->low_bit_inc); }
        void visit(Select *op) { write(op->size, op->bits); }
        void visit(Memory *op) { write(op->size, op->mem_idx); }
        void visit(Advice *op) { write(op->size, op->advice_idx); }
        void visit(InputErrorFlag *op)  { write(op->size); }
        void visit(OutputErrorFlag *op) { write(op->size); }
        void visit(InputTimestamp *op)  { write(op->size); }
        void visit(OutputTimestamp *op) { write(op->size); }
    };


    namespace detail
    {

        template< typename D, typename T >
        struct inject {
            Operation *visit(T *, uint64_t id)
            {
                auto &self = static_cast< D & >( *this );
                return self.template make_op< T >(id, self.template read< unsigned >());
            }
        };

        template< typename D, typename ... Ts >
        struct unfolder {};

        template< typename D, typename T >
        struct unfolder< D, T > : inject< D, T > { using inject< D, T >::visit; };

        template< typename D, typename T, typename ...Ts >
        struct unfolder< D, T, Ts... > : inject< D, T >, unfolder< D, Ts... >
        {
          using inject< D, T >::visit;
          using unfolder< D, Ts... >::visit;
        };

        template< typename D, typename L > struct inject_visitors {};
        template< typename D, typename ... Ts >
        struct inject_visitors< D, tl::TL< Ts ... > > : unfolder< D, Ts ... > {};

    } // namespace detail

    // Inject deserializers of former llvm-operation
    template< typename D >
    using DeserializeComputational = detail::inject_visitors< D, llvm_ops_t >;

    struct DeserializeVisitor : FileConfig, DVisitor< DeserializeVisitor >,
                                DeserializeComputational< DeserializeVisitor >
    {
        using Selector = FileConfig::Selector;

        using DeserializeComputational< DeserializeVisitor >::visit;

        std::istream &is;
        std::unordered_map<uint64_t, Operation *> id_to_op;
        std::unique_ptr< Circuit > circuit;

        explicit DeserializeVisitor(std::istream &is_)
            : is(is_)
        {}

        Circuit *get_circuit() { check(circuit); return circuit.get(); }
        std::unique_ptr< Circuit > take_circuit() { check(circuit); return std::move(circuit); }

        void Read(Selector &sel)
        {
            std::underlying_type_t< Selector > out;
            Read(out);
            sel = static_cast< Selector >(out);
        }

        void Read(std::string &str)
        {
            uint32_t size = 0u;
            Read(size);
            str.resize(size);
            for (auto i = 0u; i < size; ++i)
                Read(str[i]);
        }

        template< typename I > requires (std::is_integral_v< I >)
        void Read(I &data)
        {
            auto bytes = reinterpret_cast< uint8_t * >(&data);
            for (auto i = 0ull; i < sizeof(data); ++i)
                is >> bytes[i];
        }

        void Read(Operation::kind_t &kind)
        {
            raw_op_code_t raw = 0;
            Read(raw);
            auto maybe_kind = reconstruct_kind(raw);
            // TODO(lukas): We cannot recover right now.
            check(maybe_kind, [&]() { return "Cannot deserialize " + std::to_string(raw); });
            kind = *maybe_kind;
        }

        template< typename ...Args >
        std::tuple< Args ... > read()
        {
            std::tuple< Args ... > out;

            auto read_ = [&](Args &... args) { (Read(args), ... ); };
            std::apply(read_, out);
            return out;
        }


        void ReadOps(Operation *elems)
        {
            auto [size] = read< std::size_t >();
            for (auto i = 0u; i < size; ++i)
                elems->add_operand(Read());
        }

        Operation *read_new_op(Selector sel)
        {
            // Same for all.
            auto [hash, op_code] = read< raw_id_t, Operation::kind_t >();

            // Op specific.
            auto op = Decode(hash, op_code);
            id_to_op[hash] = op;

            // Operands last.
            if (sel == Selector::Operation)
                ReadOps(op);

            return op;
        }

        Operation *Read()
        {
          auto [sel] = read< Selector >();

          if (is_op_definition(sel))
                return read_new_op(sel);

          if (sel == Selector::Reference) {
              auto [hash] = read< raw_id_t >();

              auto op_it = id_to_op.find(hash);
              check(op_it != id_to_op.end()) << "Could not reference with id: " << hash;
              return op_it->second;
          }
          if (sel == Selector::Metadatum) {
              auto [id, key, val] = read< raw_id_t, std::string, std::string >();
              check(id_to_op.count(id))
                  << "Trying to attach metadata [ " << key << ": " << val
                  << "] to operation with id" << id << "that is not present.";
              id_to_op[id]->set_meta(std::move(key), std::move(val));
              return nullptr;
          }
          unreachable() << "Unexpected tag for an operation reference: "
                        << this->to_string(sel);
        }

        template< typename T >
        Operation *visit(T *op, uint64_t id)
        {
            unreachable() << "Cannot deserialize "
                          << fragment_as_str(T::kind)
                          << ". Most likely cause is missing impl.";
        }

        Operation *Decode(raw_id_t id, Operation::kind_t op_code)
        {
            return this->dispatch(op_code, id);
        }

        template< typename T, typename ...Args >
        auto make_op(uint64_t id, std::tuple< Args... > &&args)
        {
            auto make = [&](Args &&... args) {
                return circuit->adopt< T >(id, std::forward< Args >(args)... );
            };
            return std::apply(make, std::forward< std::tuple< Args ... > >(args));
        }

        template< typename T >
        auto reg_like(uint64_t id)
        {
            return make_op< T >(id, read< unsigned, std::string >());
        }

        template< typename T >
        auto reg_like_(uint64_t id)
        {
          return make_op< T >(id, read< std::string, unsigned >());
        }

        Operation *visit(Circuit *, uint64_t id)
        {
            check(!circuit) << "Found multiple Circuit * while deserializing!";

            auto [ptr_size] = read< uint32_t >();
            circuit = std::make_unique< Circuit >(ptr_size);
            return circuit.get();
        }

        Operation *visit(InputRegister *, uint64_t id)
        {
            return reg_like_< InputRegister >(id);
        }
        Operation *visit(OutputRegister *, uint64_t id)
        {
            return reg_like_< OutputRegister >(id);
        }

        Operation *visit(InputImmediate *, uint64_t id)
        {
            return make_op< InputImmediate >(id, read< unsigned >());
        }

        Operation *visit(Memory *, uint64_t id)
        {
            auto [size, mem_id] = read< unsigned, unsigned >();
            check(size == Memory::expected_size(circuit->ptr_size));
            return circuit->adopt< Memory >(id, size, mem_id);
        }

        Operation *visit(Advice *, uint64_t id)
        {
            auto [size, advice_idx] = read< unsigned, std::size_t >();
            return circuit->adopt<Advice>(id, size, advice_idx);
        }

        Operation *visit(Constant *, uint64_t id)
        {
            auto [size, bits] = read< unsigned, std::string >();
            return circuit->adopt< Constant >(id, std::move(bits), size);
        }

        Operation *visit(Extract *, uint64_t id) {
            auto [high, low] = read< unsigned, unsigned >();
            return circuit->adopt< Extract >(id, low, high);
        }

        Operation *visit(Select *, uint64_t id)
        {
            auto [size, bits] = read<unsigned, unsigned>();
            return circuit->adopt< Select >(id, bits, size);
        }

        template< typename T >
        auto make_leaf(uint64_t id)
        {
            auto [size] = read< unsigned >();
            return circuit->adopt< T >(id, size);
        }

        Operation *visit(InputErrorFlag *, uint64_t id)
        {
            return make_leaf< InputErrorFlag >(id);
        }
        Operation *visit(OutputErrorFlag *, uint64_t id) {
            return make_leaf< OutputErrorFlag >(id);
        }
        Operation *visit(InputTimestamp *, uint64_t id)
        {
            return make_leaf< InputTimestamp >(id);
        }
        Operation *visit(OutputTimestamp *, uint64_t id)
        {
            return make_leaf< OutputTimestamp >(id);
        }

        template< typename T >
        auto make_generic(uint64_t id) { return make_op< T >(id, read< unsigned >()); }

        template< typename T >
        auto make_condition(uint64_t id)
        {
            auto [size] = read< unsigned >();
            check(size == 1);
            return circuit->adopt< T >(id);
        }

        #define DECODE_GENERIC(cls) \
        Operation *visit(cls *, uint64_t id) { return make_generic< cls >(id); }

        #define DECODE_CONDITION(cls) \
        Operation *visit(cls *, uint64_t id) { return make_condition< cls >(id); }

        DECODE_GENERIC(Undefined)
        DECODE_GENERIC(Not)
        DECODE_GENERIC(Concat)
        DECODE_GENERIC(PopulationCount)
        DECODE_CONDITION(Parity)
        DECODE_GENERIC(CountLeadingZeroes)
        DECODE_GENERIC(CountTrailingZeroes)

        DECODE_CONDITION(AdviceConstraint)
        DECODE_CONDITION(RegConstraint)
        DECODE_CONDITION(ReadConstraint)
        DECODE_CONDITION(WriteConstraint)
        DECODE_CONDITION(UnusedConstraint)

        DECODE_GENERIC(InputInstructionBits)
        DECODE_CONDITION(DecoderResult)
        DECODE_CONDITION(DecodeCondition)
        DECODE_CONDITION(VerifyInstruction)
        DECODE_CONDITION(OnlyOneCondition)

    };

}  // namespace


void Circuit::serialize(std::ostream &os)
{
    SerializeVisitor vis(os);
    // TODO(lukas): `Write` should be called on Circuit.
    check(this->operands_size() == 1);
    vis.serialize(this);

    auto write_metadata = [&](auto op) { vis.write_metadata(op); };
    for_each_operation(write_metadata);

    os.flush();
}

void Circuit::serialize(std::string_view filename)
{
    std::ofstream file(std::string(filename), std::ios::binary | std::ios::trunc);
    return serialize(file);
}

auto Circuit::deserialize(std::istream &is) -> circuit_ptr_t
{
    // TODO(lukas): Configurable.
    DeserializeVisitor vis(is);

    auto old_flags = is.flags();
    is.unsetf(std::ios::skipws);

    while (is.good() && !is.eof() && EOF != is.peek())
        std::ignore = vis.Read();

    is.flags(old_flags);
    return vis.take_circuit();
}

auto Circuit::deserialize(std::string_view filename) -> circuit_ptr_t
{
    std::ifstream file(std::string{filename}, std::ios::binary);
    return deserialize(file);
}

}  // namespace circ
