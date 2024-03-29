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
#include <gap/core/ranges.hpp>

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

        // Postprocess should have type `void()(std::uniqe_ptr<T> &&)` -- it will be given
        // ownership of the object.
        template< typename Predicate, typename Postprocess >
        std::size_t remove_if( Predicate &&should_be_removed, Postprocess &&process )
        {
            std::size_t num = 0;
            for (auto it = defs.begin(); it != defs.end();)
            {
                if ( !should_be_removed( (*it).get() ) )
                {
                    ++it;
                    continue;
                }

                process( std::move( *it ) );
                it = defs.erase( it );
                ++num;
             }
             return num;
        }

        storage_t defs;
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

            --std::get< 1 >(*it);
            if (std::get< 1 >(*it) == 0)
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

        /* Generic iteration */

        auto begin() { return _operands.begin(); }
        auto end() { return _operands.end(); }

        auto begin() const { return _operands.begin(); }
        auto end()   const { return _operands.end(); }

        /* Users */
        gap::generator< T * > users()
        {
            for (auto &[x, _] : _users)
                co_yield x;
        }

        gap::generator< const T * > users() const
        {
            for (auto &[x, _] : _users)
                co_yield x;
        }

        std::size_t users_size() const { return _users.size(); }

        /* Operands */
        gap::generator< const T * > operands() const
        {
            for (const T *x : _operands)
                co_yield x;
        }

        gap::generator< T * > operands()
        {
            for (auto x : _operands)
                co_yield x;
        }

        std::size_t operands_size() const { return _operands.size(); }

        std::size_t unique_operands_count() const
        {
            return std::unordered_set< T * >(_operands.begin(), _operands.end()).size();
        }

        T * operand(std::size_t idx) { return _operands[idx]; }
        const T * operand(std::size_t idx) const { return _operands[idx]; }

        bool is_leaf() const { return _operands.empty(); }

        /* Modifiers */

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

        void add_operands(gap::ranges::range auto vs)
        {
            for (auto v : vs)
                add_operand(v);
        }

        template< typename H, typename ... Ts >
        void add_operands(H op, Ts ... ops)
        {
            add_operand(op);
            if constexpr (sizeof ... (Ts) != 0)
                return add_operands< Ts ... >(ops ...);
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
            auto it_num = _users.size();
            for ( std::size_t i = 0; i < it_num; ++i )
                std::get< 0 >( _users.back() )->replace_all_operands_with(self(), other);
        }

        void destroy()
        {
            for ( auto &op : _operands )
                op->purge_user( self() );
            for ( auto &[ user, _ ] : _users )
                std::erase( user->_operands, self() );
            this->_operands.clear();
            this->_users.clear();
        }
    };

    template< typename T, gap::ranges::range R >
    requires ( std::is_same_v< typename T::value_type, gap::ranges::range_value_t< R > > )
    T freeze( R &&range )
    {
        T out;
        for ( auto op : range )
            out.insert( out.end(), op );
        return out;
    }

    template< template< typename ... > class T, gap::ranges::range R >
    auto freeze( R &&range )
    {
        T< gap::ranges::range_value_t< R > > out;
        for ( auto op : range )
            out.insert( out.end(), op );
        return out;
    }
} // namespace circ
