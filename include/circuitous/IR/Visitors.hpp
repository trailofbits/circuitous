/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>

namespace circ
{
    template< typename D, typename L > struct Visitor_ {};

    template< typename Derived, typename ... Ops >
    struct Visitor_< Derived, tl::TL< Ops ... > >
    {
        void Visit(Operation *op) { op->Traverse(*this); }

        Derived &self() { return static_cast<Derived &>(*this); }

        template< typename T, typename ...Tail, typename ... Args >
        auto Visit_(Operation *op, Args &&...args)
        {
            if (is_specialization< T >(op->op_code))
                return self().Visit(dynamic_cast< T * >(op), std::forward< Args >(args)...);

            if constexpr (sizeof...(Tail) != 0) {
                return this->Visit_< Tail ... >(op, std::forward< Args >(args)...);
            } else {
                return self().Visit(op, std::forward< Args >(args)...);
            }
        }

        template<typename ...Args>
        auto Dispatch(Operation *op, Args &&...args)
        {
            return this->Visit_< Ops ... >(op, std::forward< Args >(args)...);
        }
    };

    template< typename Derived >
    using Visitor = Visitor_< Derived, all_nodes_list_t >;

    template< typename D, typename L > struct NonRecursiveVisitor_ {};

    template< typename Derived, typename ... Ops >
    struct NonRecursiveVisitor_< Derived, tl::TL< Ops ... > >
    {
        Derived &self() { return static_cast<Derived &>(*this); }

        template< typename T, typename ...Tail, typename ... Args >
        auto Visit_(Operation *op, Args &&...args)
        {
            if (is_specialization< T >(op->op_code))
                return self().Visit(dynamic_cast< T * >(op), std::forward<Args>(args)...);

            if constexpr (sizeof...(Tail) != 0) {
                return this->Visit_< Tail ... >(op, std::forward< Args >(args)...);
            } else {
                unreachable() << "unhandled operation";
            }
        }

        template< typename ...Args >
        auto Dispatch(Operation *op, Args &&...args)
        {
          return this->Visit_< Ops ... >(op, std::forward< Args >(args)...);
        }
    };

    template< typename Derived >
    using NonRecursiveVisitor = NonRecursiveVisitor_< Derived, all_nodes_list_t >;

    template< typename D, typename L > struct DVisitor_ {};

    template< typename Derived, typename ... Ops >
    struct DVisitor_< Derived, tl::TL< Ops ... > >
    {
        Derived &self() { return static_cast<Derived &>(*this); }

        template< typename T, typename ...Tail, typename ...Args >
        auto Visit_(uint32_t kind, Args &&... args)
        {
            if (is_specialization< T >(kind))
                return self().Visit(static_cast< T * >(nullptr), std::forward< Args >(args)...);

            if constexpr (sizeof...(Tail) != 0) {
                return this->Visit_< Tail ... >(kind, std::forward< Args >(args)...);
            } else {
                UNREACHABLE() << "Kind: " << kind << " does not correspond to known Operation!";
            }
        }

        template< typename ... Args >
        auto Dispatch(uint32_t kind, Args &&...args)
        {
            return this->Visit_< Ops ... >(kind, std::forward< Args >(args)...);
        }
    };

    template< typename Derived >
    using DVisitor = DVisitor_< Derived, all_nodes_list_t >;

    template< typename Derived >
    struct UniqueVisitor : public Visitor< Derived >
    {
        using parent = Visitor<Derived>;

        void Dispatch(Operation *op)
        {
            if (seen_ops.count(op))
                return;
            seen_ops.insert(op);
            this->parent::Dispatch(op);
        }

        void Reset() { seen_ops.clear(); }

        std::unordered_set< Operation * > seen_ops;
    };

} // namespace circ
