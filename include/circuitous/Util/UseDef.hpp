/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <memory>
#include <unordered_set>
#include <vector>

#include <circuitous/Support/Check.hpp>

namespace circ
{

    // Class that owns all the memory for nodes of type `Value`.
    // Other classes should operate on raw pointers
    // It is not strictly enforced, but it is expected `Value` inherits
    // from `Node` class.
    template< typename Value >
    struct DefList
    {
        using storage_t = std::unordered_set< std::unique_ptr< Value > >;

        // Iterate as if there were no unique_ptrs.
        template< typename SI >
        struct It
        {
            using value_type = Value *;

            It(SI it_) : it(it_) {}

            auto &operator++() noexcept { ++it; return *this; }
            bool operator==(const It<SI> &other) const { return it == other.it; }
            bool operator!=(const It<SI> &other) const { return !(*this == other); }

            value_type operator*() const noexcept { return it->get(); }

            SI it;
        };

        using iterator = It< typename storage_t::iterator >;

        auto begin() { return iterator(defs.begin()); }
        auto  end()  { return iterator(defs.end()); }

        using value_type = Value *;

        template< typename ...Args >
        auto create(Args &&...args)
        -> std::enable_if_t< std::is_constructible_v< Value, Args ... >, Value * >
        {
            auto new_def = new Value(std::forward<Args>(args)...);
            defs.emplace(new_def);
            return new_def;
        }

        Value *adpot(Value &&val)
        {
            auto [it, _] = defs.insert(std::make_unique< Value >(std::move(val)));
            return it->get();
        }

        std::size_t size() const { return defs.size(); }
        bool empty() const noexcept { return defs.size() == 0; }

        value_type operator[](uint32_t idx) { return std::next(defs.begin(), idx)->get(); }

        // CB should have type `void()(std::uniqe_ptr<T> &&)` -- it will be given
        // ownership of the object.
        template< typename CB >
        std::size_t remove_unused(CB cb)
        {
            std::size_t num = 0;
            for (auto it = defs.begin(); it != defs.end();)
            {
                if ((*it)->users.size() == 0) {
                    cb(std::move(*it));
                    it = defs.erase(it);
                    ++num;
                } else {
                    ++it;
                }
            }
            return num;
        }

        storage_t defs;
    };

    template< typename T >
    struct Use
    {
        T *user;
        T *use;
    };

    template< typename T >
    struct UseList : std::vector< T * >
    {
        using impl = std::vector< T * >;
    };

    // Value can be used by others, `users` are used to travel up the data flow.
    // `add(x, y)` -- `add` is in `users` of both `x` and `y`
    template< typename T >
    struct Value
    {
        UseList< T > users;

        std::size_t remove_user(T *user)
        {
            auto num = std::erase(users, user);
            check(num > 0) << "Trying to remove user that is not part of the list.";
            return num;
        }
    };

    // add(x, y) -> `add` node uses `x` and `y`.
    template< typename T >
    struct User
    {
        UseList< T > operands;

        std::size_t remove_operand(T *op)
        {
            auto num = std::erase(operands, op);
            check(num > 0) << "Trying to remove operand that is not part of the list.";
            return num;
        }
    };

    // The node can be both user and value.
    // The values should always be in consistent state e.g. if `x in y.operands`, then
    // `y in yx.users`.
    template< typename T >
    struct Node : Value< T >, User< T >
    {
        T *Raw() { return static_cast< T * >(this); }

        void add_use(T *other)
        {
            this->operands.push_back(other);
            other->users.push_back(Raw());
        }

        void remove_use(T *parent)
        {
            std::size_t operands_removed = parent->remove_operand(Raw());
            std::size_t users_removed = this->remove_user(parent);
            check(operands_removed == users_removed);
        }

        /*
         * we have node x(A,B) and want to change it into x(C,B)
         * To do so we let A know x is not their parent any more
         * we can remove x as a parent if it uses A only once
         * but if x(A,A) then we need to keep x as parent for the second A
         *
         * As AFAICT A does not know which User List entry corresponds to which entry in Operand list
         * We can't know which to keep or which to replace.
         */
        void replace_use(T *other, std::size_t at)
        {
            check(this->operands.size() > at);

            auto removed = this->operands[at]->remove_user(Raw());
            check(removed == 1) << "Parent has multiple references to children, not supported";

            this->operands[at] = other;
            other->users.push_back(Raw());
        }

        void replace_all_uses_with(T *other)
        {
            check(other != this) << "Trying to replace all uses of X with X, probably error.";

            // this is used to retrieve which indexes in the user of the current node
            // points to this node, if we have N(A,A) then N points to A twice
            // And hence should return all indices
            auto fetch = [&](const auto &where) -> std::vector<std::size_t>
            {
                std::vector<std::size_t> retval;
                for (std::size_t i = 0; i < where.size(); ++i)
                    if (where[i] == this)
                        retval.emplace_back(i);

                check(!retval.empty()) << "User and uses are out of sync.";

                return retval;
            };

            for (auto user : this->users)
            {
                // NOTE(lukas): I actually do not expect these vectors to grow much.
                //              And given the cache friendliness of vector this should
                //              not be a bottle-neck.
                for(auto idx : fetch(user->operands)){
                    user->operands[idx] = other;
                    other->users.push_back(user);
                }
            }
            this->users.clear();
        }

        template< typename U, typename CB >
        void for_each_use(CB cb)
        {
            std::vector< U * > frozen;
            for (auto user : this->users)
                if (auto casted = dynamic_cast< U * >(user))
                    frozen.push_back(casted);

            for (auto op : frozen)
                cb(op);
        }
    };
} // namespace circ
