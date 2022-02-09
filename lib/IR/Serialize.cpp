/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Printers.h>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

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
            Invalid = 0x1,
            Metadatum = 0x2,
            Reference = 0xff
        };

        using raw_op_code_t = uint32_t;
        using raw_id_t = uint64_t;

        static std::string to_string(const Selector &sel)
        {
            switch(sel) {
                case Selector::Operation : return "Operation";
                case Selector::Invalid   : return "Invalid";
                case Selector::Metadatum : return "Metadatum";
                case Selector::Reference : return "Reference";
            }
        }
    };

    /* Format in the ir file is following
     * 32 - `ptr_size` of Circuit.
     * 8: selector, 64: id, 64: opcode, ... rest of the data dependeing on opcode
     * 8: selector, 64: id it references
     * NOTE(lukas): In attempt to make this deterministic I have chosen to replace
     *              pointer hashes with deterministic ids. If you want, feel free to
     *              tweak it, but please make sure `x == store(load(x))` -- with respect
     *              to semantics (not neccessary ids or other internals).
     */
    struct SerializeVisitor : public Visitor< SerializeVisitor >, FileConfig
    {
        using Selector = FileConfig::Selector;

        std::ostream &os;
        std::unordered_set<uint64_t> written;

        ~SerializeVisitor()
        {
            os.flush();
        }

        explicit SerializeVisitor(std::ostream &os_) : os(os_) {}

        void serialize(Operation *op) { return Write(op); }

        void Write(Operation *op)
        {
            if (!written.count(op->id()))
            {
                Write(Selector::Operation);
                Write(op->id());
                check(0u < op->op_code) << "Weird opcode" << op->op_code;
                raw_op_code_t op_code = op->op_code;
                Write(op_code);
                written.insert(op->id());
                this->Dispatch(op);
            } else {
                Write(Selector::Reference);
                Write< raw_id_t >(op->id());
            }
        }

        void Write(Selector sel)
        {
            Write(static_cast< std::underlying_type_t< Selector > >(sel));
        }

        void Write(uint8_t byte) { os << byte; }
        void Write(int8_t byte) { os << static_cast< uint8_t >(byte); }

        template< typename T >
        void Write(const UseList< T > &elems)
        {
            Write< uint32_t >(static_cast< uint32_t >(elems.size()));
            for (const auto &elem : elems)
                Write(elem);
        }

        void Write(const std::string &str)
        {
            Write< uint32_t >(static_cast< uint32_t >(str.size()));
            for (auto ch : str)
                Write(ch);
        }

        template< typename T > requires (!std::is_pointer_v< T >)
        void Write(const T &data)
        {
            auto bytes = reinterpret_cast< const uint8_t * >(&data);
            for (auto i = 0ull; i < sizeof(data); ++i)
                Write(static_cast< uint8_t >(bytes[i]));
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
        void Visit(Circuit *op) { write(op->ptr_size, op->operands);  }

        void Visit(InputRegister *op) { write(op->reg_name, op->size); }
        void Visit(OutputRegister *op) { write(op->reg_name, op->size); }

        void Visit(Operation *op) { write(op->size, op->operands); }
        void Visit(Constant *op) { write(op->size, op->bits); }
        void Visit(InputImmediate *op) { write(op->size, op->operands); }

        void Visit(Extract *op) { write(op->high_bit_exc, op->low_bit_inc, op->operands[0]); }
        void Visit(Select *op) { write(op->size, op->bits, op->operands); }
        void Visit(Memory *op) { write(op->size, op->mem_idx); }
        void Visit(Advice *op) { write(op->size, op->advice_idx); }
        void Visit(InputErrorFlag *op) {}
        void Visit(OutputErrorFlag *op) {}
        void Visit(InputTimestamp *op) {}
        void Visit(OutputTimestamp *op) {}
    };


    namespace detail
    {

        template< typename D, typename T >
        struct inject {
            Operation *Visit(T *, uint64_t id)
            {
                auto &self = static_cast< D & >( *this );
                return self.template with_ops< T >(id, self.template read< unsigned >());
            }
        };

        template< typename D, typename ... Ts >
        struct unfolder {};

        template< typename D, typename T >
        struct unfolder< D, T > : inject< D, T > { using inject< D, T >::Visit; };

        template< typename D, typename T, typename ...Ts >
        struct unfolder< D, T, Ts... > : inject< D, T >, unfolder< D, Ts... >
        {
          using inject< D, T >::Visit;
          using unfolder< D, Ts... >::Visit;
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

        using DeserializeComputational< DeserializeVisitor >::Visit;

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

        void Read(uint8_t &byte)
        {
            check(!is.eof() && is.good());
            const auto old_offset = static_cast< long long >(is.tellg());
            is >> byte;
            if (!is.eof())
                check(old_offset + 1 == static_cast< long long >(is.tellg()));
        }

        void Read(std::string &str)
        {
            uint32_t size = 0u;
            Read(size);
            str.resize(size);
            for (auto i = 0u; i < size; ++i)
                Read(str[i]);
        }

        template< typename T > requires (!std::is_base_of_v< Operation, T >)
        void Read(T &data)
        {
            auto bytes = reinterpret_cast< uint8_t * >(&data);
            for (auto i = 0ull; i < sizeof(data); ++i)
                Read(bytes[i]);
        }

        void Read(int8_t &byte)
        {
            uint8_t b;
            Read(b);
            byte = static_cast< int8_t >(b);
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
            auto [size] = read< uint32_t >();
            for (auto i = 0u; i < size; ++i)
                elems->AddUse(Read());
        }

        Operation *Read()
        {
          auto [sel] = read< Selector >();

          if (sel == Selector::Operation) {
              auto [hash, op_code] = read< raw_id_t, raw_op_code_t >();

              auto op = Decode(hash, op_code);
              id_to_op[hash] = op;
              return op;
          }
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
        Operation *Visit(T *op, uint64_t id)
        {
            unreachable() << "Cannot deserialize "
                          << T::kind << ". Most likely cause is missing impl.";
        }

        Operation *Decode(raw_id_t id, raw_op_code_t op_code)
        {
            return this->Dispatch(op_code, id);
        }

        template< typename T, typename ...Args >
        auto make_op(uint64_t id, std::tuple< Args... > &&args)
        {
            auto make = [&](Args &&... args) {
                return circuit->Adopt< T >(id, std::forward< Args >(args)... );
            };
            return std::apply(make, std::forward< std::tuple< Args ... > >(args));
        }

        template< typename T, typename U >
        auto with_ops(uint64_t id, U &&u)
        {
            auto self = make_op< T >(id, std::forward<U>(u));
            ReadOps(self);
            return self;
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

        Operation *Visit(Circuit *, uint64_t id)
        {
            check(!circuit) << "Found multiple Circuit * while deserializing!";

            auto [ptr_size] = read< uint32_t >();
            circuit = std::make_unique< Circuit >(ptr_size);
            ReadOps(circuit.get());
            return circuit.get();
        }

        Operation *Visit(InputRegister *, uint64_t id)
        {
            return reg_like_< InputRegister >(id);
        }
        Operation *Visit(OutputRegister *, uint64_t id)
        {
            return reg_like_< OutputRegister >(id);
        }

        Operation *Visit(InputImmediate *, uint64_t id)
        {
            auto op = make_op< InputImmediate >(id, read< unsigned >());
            ReadOps(op);
            return op;
        }

        Operation *Visit(Memory *, uint64_t id)
        {
            auto [size, mem_id] = read< unsigned, unsigned >();
            check(size == Memory::expected_size(circuit->ptr_size));
            return circuit->Adopt< Memory >(id, size, mem_id);
        }

        Operation *Visit(Advice *, uint64_t id)
        {
            auto [size, advice_idx] = read< unsigned, unsigned >();
            return circuit->Adopt<Advice>(id, size, advice_idx);
        }

        Operation *Visit(Constant *, uint64_t id)
        {
            auto [size, bits] = read< unsigned, std::string >();
            return circuit->Adopt< Constant >(id, std::move(bits), size);
        }

        Operation *Visit(Extract *, uint64_t id) {
            auto [high, low] = read< unsigned, unsigned >();
            auto op = circuit->Adopt< Extract >(id, low, high);
            op->AddUse(Read());
            return op;
        }

        Operation *Visit(Select *, uint64_t id)
        {
            auto [size, bits] = read<unsigned, unsigned>();
            auto op = circuit->Adopt< Select >(id, bits, size);
            ReadOps(op);
            return op;
        }

        Operation *Visit(InputErrorFlag *, uint64_t id)
        {
            return circuit->Adopt<InputErrorFlag>(id);
        }
        Operation *Visit(OutputErrorFlag *, uint64_t id)
        {
            return circuit->Adopt<OutputErrorFlag>(id);
        }
        Operation *Visit(InputTimestamp *, uint64_t id)
        {
            return circuit->Adopt<InputTimestamp>(id);
        }
        Operation *Visit(OutputTimestamp *, uint64_t id)
        {
            return circuit->Adopt<OutputTimestamp>(id);
        }

        template< typename T >
        auto make_generic(uint64_t id) { return with_ops< T >(id, read< unsigned >()); }

        template< typename T >
        auto make_condition(uint64_t id)
        {
            auto [size] = read< unsigned >();
            check(size == 1);
            auto self = circuit->Adopt< T >(id);
            ReadOps(self);
            return self;
        }

        #define DECODE_GENERIC(cls) \
        Operation *Visit(cls *, uint64_t id) { return make_generic< cls >(id); }

        #define DECODE_CONDITION(cls) \
        Operation *Visit(cls *, uint64_t id) { return make_condition< cls >(id); }

        DECODE_GENERIC(Undefined)
        DECODE_GENERIC(Not)
        DECODE_GENERIC(Concat)
        DECODE_GENERIC(PopulationCount)
        DECODE_CONDITION(Parity)
        DECODE_GENERIC(CountLeadingZeroes)
        DECODE_GENERIC(CountTrailingZeroes)

        DECODE_CONDITION(AdviceConstraint)
        DECODE_CONDITION(RegConstraint)
        DECODE_CONDITION(PreservedConstraint)
        DECODE_CONDITION(CopyConstraint)
        DECODE_CONDITION(ReadConstraint)
        DECODE_CONDITION(WriteConstraint)
        DECODE_CONDITION(UnusedConstraint)

        DECODE_GENERIC(InputInstructionBits)
        DECODE_CONDITION(DecodeCondition)
        DECODE_CONDITION(Or)
        DECODE_CONDITION(And)
        DECODE_CONDITION(VerifyInstruction)
        DECODE_CONDITION(OnlyOneCondition)

    };

}  // namespace

void Circuit::serialize(std::ostream &os)
{
    SerializeVisitor vis(os);
    // TODO(lukas): `Write` should be called on Circuit.
    check(this->operands.size() == 1);
    vis.serialize(this);

    auto write_metadata = [&](auto op) { vis.write_metadata(op); };
    ForEachOperation(write_metadata);

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
    std::ifstream file(std::string(filename), std::ios::binary);
    return deserialize(file);
}

}  // namespace circ
