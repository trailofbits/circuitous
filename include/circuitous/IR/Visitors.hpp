/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>

#include <type_traits>

namespace circ
{
    // Visitors to allow some structured work on the operation tree.
    // There are several methods provided:
    // `Dispatch()` - top-level to be called in user code to start the walk
    // `Visit_()` - tries to match it's argument to any known operation and if it matches
    //              casts it and calls `Visit(X)`. If not type is matched, calls
    //              `DefaultVisit()` instead.
    // `DefaultVisit()` - method to be called if no type was matched in `Visit_`.
    // `Visit(X)` - method that is called with correctly casted type. Usually implemented
    //              in user code.
    // For recursive traversal there is `Operation::Traverse()`.
    // `IsConst` tells the class whether `Visit(X)` is operating on `const Operation *` or
    // only `Operation *`.
    template< typename Derived, bool IsConst, typename List > struct VisitorBase;

    // Slightly different implementation, as it operates on tags rather than objects
    // themselves (e.g. `uint32_t` vs `Operation *`.
    // Specialized `Visit(X)` methods are however still of type
    // `Visit( X *, Args && ... )`, however the first pointer is **nullptr** and serves
    // as type dispatch. Do not dereference it.
    template< typename Derived, typename List > struct DVisitorBase {};

    // `DefaultVisit` simply calls `Visit(Operation *op)`.
    template< typename Derived, bool IsConst, typename List > struct Visitor_ {};
    // `DefaultVisit` calls `unreachable()`.
    template< typename Derived, bool IsConst, typename List > struct NonDefaultingVisitor_ {};


    template< typename Derived, bool IsConst, typename ... Ops >
    struct VisitorBase< Derived, IsConst, tl::TL< Ops ... > >
    {
        template< typename T >
        using adjust_constness_t = std::conditional_t< IsConst, const T, T >;
        using operation_t = adjust_constness_t< Operation * >;

        Derived &self() { return static_cast< Derived & >(*this); }
        const Derived &self() const { return static_cast< const Derived & >(*this); }

        template< typename ... Args >
        auto Dispatch(operation_t op, Args && ... args)
        {
            return this->Visit_< Ops ... >(op, std::forward< Args >(args)...);
        }

        // This method is not supposed to be overwritten by Derived class!
        template< typename T, typename ... Tail, typename ... Args >
        auto Visit_(operation_t op, Args && ...args)
        {

            if (is_specialization< T >(op->op_code))
            {
                auto casted = dynamic_cast< adjust_constness_t< T * > >(op);
                return self().Visit(casted, std::forward< Args >(args) ... );
            }

            if constexpr (sizeof ... (Tail) != 0) {
                return this->Visit_< Tail ... >(op, std::forward< Args >(args)...);
            } else {
                return self().DefaultVisit(op, std::forward< Args >(args) ...);
            }
        }
    };

    template< typename Derived, bool IsConst, typename ... Ops >
    struct Visitor_< Derived, IsConst, tl::TL< Ops ... > >
        : VisitorBase< Derived, IsConst, tl::TL< Ops ... > >
    {
        using parent_t = VisitorBase< Derived, IsConst, tl::TL< Ops ... > >;
        using operation_t = typename parent_t::operation_t;

        template< typename ... Args >
        auto DefaultVisit(operation_t op, Args && ... args)
        {
            return this->self().Visit(op, std::forward< Args >(args) ...);
        }
    };

    template< typename Derived, bool IsConst = false >
    using Visitor = Visitor_< Derived, IsConst, all_nodes_list_t >;

    template< typename Derived, bool IsConst, typename ... Ops >
    struct NonDefaultingVisitor_< Derived, IsConst, tl::TL< Ops ... > >
        : VisitorBase< Derived, IsConst, tl::TL< Ops ... > >
    {
        using parent_t = VisitorBase< Derived, IsConst, tl::TL< Ops ... > >;
        using operation_t = typename parent_t::operation_t;

        using parent_t::Visit_;

        template< typename ... Args >
        auto DefaultVisit(operation_t op, Args && ... args )
        -> decltype(this->template Visit_< Ops ... >(op, std::forward< Args >(args) ...))
        {
            unreachable() << "Missing Visitor::Visit(X) for " << pretty_print(op);
        }
    };

    template< typename Derived, bool IsConst = false >
    using NonDefaultingVisitor = NonDefaultingVisitor_< Derived, IsConst, all_nodes_list_t >;

    // NOTE(lukas): This probably could be unified with the rest of the stack, but it would
    //              be quite messy and overall not that worth.
    template< typename Derived, typename ... Ops >
    struct DVisitorBase< Derived, tl::TL< Ops ... > >
    {
        Derived &self() { return static_cast< Derived & >(*this); }

        template< typename T, typename ...Tail, typename ...Args >
        auto Visit_(uint32_t kind, Args &&... args)
        {
            if (is_specialization< T >(kind))
                return self().Visit(static_cast< T * >(nullptr), std::forward< Args >(args)...);

            if constexpr (sizeof...(Tail) != 0) {
                return this->Visit_< Tail ... >(kind, std::forward< Args >(args)...);
            } else {
                unreachable() << "Kind: " << kind << " does not correspond to known Operation!";
            }
        }

        template< typename ... Args >
        auto Dispatch(uint32_t kind, Args &&...args)
        {
            return this->Visit_< Ops ... >(kind, std::forward< Args >(args)...);
        }
    };

    template< typename Derived >
    using DVisitor = DVisitorBase< Derived, all_nodes_list_t >;

    template< typename Derived, bool IsConst = false >
    struct UniqueVisitor : Visitor< Derived, IsConst >
    {
        using parent_t = Visitor< Derived, IsConst >;
        using operation_t = typename parent_t::operation_t;

        auto Dispatch(operation_t op)
        {
            if (seen_ops.count(op))
                return;
            seen_ops.insert(op);
            return this->parent_t::Dispatch(op);
        }

        void reset() { seen_ops.clear(); }

        std::unordered_set< Operation * > seen_ops;
    };

} // namespace circ
