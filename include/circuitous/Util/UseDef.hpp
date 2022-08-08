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

#include <gap/core/generator.hpp>

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
                if ((*it)->users_size() == 0) {
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
    struct UseList : std::vector< T * >
    {
        using impl = std::vector< T * >;
    };

    template< typename T >
    struct Node
    {
      protected:
        std::vector< T * > _operands;
        // For each user we also need to keep track of how many times value
        // is used by it.
        std::vector< std::tuple< T *, std::size_t > > _users;

      private:
        auto user_comparator(T *other)
        {
            return [=](const auto &x) { return std::get< 0 >(x) == other; };
        }

        auto fetch_user(T *other)
        {
            return std::find_if(_users.begin(), _users.end(), user_comparator(other));
        }

        void remove_user(T *other)
        {
            auto it = fetch_user(other);

            --it->second;
            if (it->second == 0)
                _users.erase(it);
        }

        void purge_user(T *other)
        {
            std::erase_if(_users, user_comparator(other));
        }

        void add_user(T *other, std::size_t times = 1)
        {
            if (auto it = fetch_user(other); it != _users.end())
                std::get< 1 >(*it) += times;
            else
                _users.emplace_back(other, times);

        }

        T *self() { return static_cast< T * >(this); }

      public:
        // TODO(lukas): Replace with generator.
        gap::generator< T * > users()
        {
            for (auto &[x, _] : _users)
                co_yield x;
        }

        gap::generator< const T * > users() const
        {
            for (const auto &[x, _] : _users)
                co_yield x;
        }

        std::size_t users_size() const { return _users.size(); }

        // TODO(lukas): Also replace with generator to have the same return type as `users()`?
        const std::vector< T * > &operands() const
        {
            return _operands;
        }

        std::size_t unique_operands_count() const
        {
            return std::unordered_set< T * >(_operands.begin(), _operands.end()).size();
        }

        void remove_operand(std::size_t idx)
        {
            auto op = _operands[idx];
            _operands.erase(idx);
            op->remove_user(self());
        }

        void add_operand(T *value)
        {
            _operands.emplace_back(value);
            value->add_user(self());
        }

        void replace_operand(std::size_t idx, T *value)
        {
            _operands[idx]->remove_user(self());
            _operands[idx] = value;
            _operands[idx]->add_user(self());
        }

        void remove_all_operands(T *value)
        {
            value->purge_user(self());
            std::erase(_operands, value);
        }

        void replace_all_operands_with(T *old, T *value)
        {
            old->purge_user(self());
            std::size_t total = 0;
            for (std::size_t idx = 0; idx < _operands.size(); ++idx)
            {
                if (_operands[idx] == old)
                {
                    _operands[idx] = value;
                    ++total;
                }
            }
            value->add_user(self(), total);

        }

        void replace_all_uses_with(T *other)
        {
            for (auto &[op, _] : _users)
                op->replace_all_operands_with(self(), other);
        }

        void destroy()
        {
            for (auto &op : _operands)
                op->purge_user(self());
        }
    };
} // namespace circ
