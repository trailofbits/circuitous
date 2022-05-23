/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>

namespace circ
{

    struct Circuit : CircuitStorage, Operation
    {
        static constexpr Operation::kind_t kind = Operation::kind_t::kCircuit;
        virtual ~Circuit() = default;

        using circuit_ptr_t = std::unique_ptr<Circuit>;

        void serialize(std::ostream &os);
        void serialize(std::string_view filename);

        static circuit_ptr_t deserialize(std::istream &is);
        static circuit_ptr_t deserialize(std::string_view filename);

        static std::string op_code_str() { return "circuit"; }

        std::string Name() const override { return "circuit"; }
        // TODO(lukas): Will be deprecated in the future.
        Circuit(uint32_t ptr_size_=64) : Operation(this->bool_size, kind),
                                         ptr_size(ptr_size_)
        {}

        uint32_t ptr_size;
    };

    using all_nodes_list_t = tl::push_front< Circuit, subnode_list_t >;

    // TODO(lukas): Generalise comparator and move to `tl::`.
    template< typename F, typename H, typename ... Tail >
    auto runtime_find_(Operation::kind_t rkind, F &&f)
    {
        if (H::kind == rkind)
            return f(static_cast< H * >(nullptr));

        if constexpr (sizeof...(Tail) != 0) {
            return runtime_find_< F, Tail ... >(rkind, f);
        } else {
            unreachable() << "runtime find on: " << to_string(rkind) << " failed";
        }
    }

    // These may be quite expensive to re-compute, if they are called
    // often, consider refactor or at least caching.
    template< typename F, typename ... Ts >
    auto runtime_find( tl::TL< Ts ... >, Operation::kind_t rkind, F &&f)
    {
        return runtime_find_< F, Ts... >(rkind, std::forward< F >(f));
    }

    static inline std::string op_code_str(Operation::kind_t rkind)
    {
        auto get_name = [](auto x) {
            using raw_t = std::remove_pointer_t< std::decay_t< decltype( x ) > >;
            return raw_t::op_code_str();
        };

        static std::unordered_map< uint32_t, std::string > cache;
        if (!cache.count(rkind))
            cache[rkind] = runtime_find(all_nodes_list_t{}, rkind, get_name);

        return cache[rkind];
    }


    // TODO(lukas): This is just a hotfix, ideally we want the type list
    //              will all the nodes to carry names.
    static inline std::string fragment_as_str(uint32_t kind)
    {
        std::stringstream ss;
        ss << op_code_str(kind);
        return ss.str();
    }

    template<typename Kind>
    static inline std::string to_string(Kind kind)
    {
        return fragment_as_str(kind);
    }

    template< bool with_meta = true >
    static inline std::string pretty_print(const Operation *op)
    {
        std::stringstream ss;
        ss << op->id() << ": " << to_string(op->op_code);
        if constexpr (with_meta)
        {
            if (op->meta_size())
                ss << std::endl << op->dump_meta();
        }
        return ss.str();
    }

} // namespace circ
