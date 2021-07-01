 // Copyright 2021, Trail of Bits. All rights reserved.

#pragma once

#include <circuitous/Util/ConstExprVector.hpp>

#include <cstddef>
#include <array>
#include <string_view>

namespace circ::meta
{
  template< std::size_t N >
  struct fixed_string
  {
    using value_type  = char;
    using container_t = std::array< value_type, N >;

    using reference  			 = value_type&;
    using const_reference  = const value_type&;
    using pointer    			 = value_type*;
    using const_pointer    = const value_type*;
    using iterator	 			 = typename container_t::iterator;
    using const_iterator	 = typename container_t::const_iterator;

    constexpr fixed_string() = default;

    constexpr fixed_string(const char (&input)[N + 1]) noexcept
    {
      std::copy(input, std::next(input, N), data.begin());
    }

    constexpr fixed_string(const fixed_string &other) = default;
    constexpr fixed_string(fixed_string &other) = default;

    constexpr fixed_string& operator=(const fixed_string &other) = default;
    constexpr fixed_string& operator=(fixed_string &&other) = default;

    constexpr iterator begin() noexcept { return data.begin(); }
    constexpr const_iterator begin() const noexcept { return data.begin(); }

    constexpr iterator end() noexcept { return data.end(); }
    constexpr const_iterator end() const noexcept { return data.end(); }

    constexpr reference front() noexcept { return data.front(); }
    constexpr const_reference front() const noexcept { return data.front(); }

    constexpr reference back() noexcept { return data.back(); }
    constexpr const_reference back()  const noexcept { return data.back(); }

    constexpr std::size_t size() const noexcept { return data.size(); }
    constexpr bool empty() const noexcept { return data.empty(); }

    constexpr bool contains(char c) const noexcept { return std::find(begin(), end(), c) != end(); }

    constexpr reference operator[](std::size_t idx) noexcept { return data[idx]; }
    constexpr const_reference operator[](std::size_t idx) const noexcept { return data[idx]; }

    constexpr operator std::basic_string_view<char>() const noexcept
    {
      return std::basic_string_view<char>{data.begin(), data.size()};
    }

    template< std::size_t From >
    constexpr auto substr() const noexcept
    {
      fixed_string< N - From > result{};
      std::copy( std::next(begin(), From), end(), result.begin() );
      return result;
    }

    constexpr auto operator<=>(const fixed_string& other) const = default;

    container_t data{};
  };

  template< std::size_t N, std::size_t M >
  constexpr auto operator+(const fixed_string<N> &a, const fixed_string<M> &b)
  {
    fixed_string< N + M > result{};
    std::copy(a.begin(), a.end(), result.begin());
    std::copy(b.begin(), b.end(), std::next(result.begin(), N));
    return result;
  }

  template< std::size_t N, std::size_t M > requires (N != M)
  constexpr bool operator==(const fixed_string<N> &a, const fixed_string<M> &b)
  {
    return false;
  }

  template< std::size_t N >
  fixed_string(const char (&)[N]) -> fixed_string<N - 1>;

  template< std::size_t N >
  fixed_string(fixed_string<N>) -> fixed_string<N>;

  namespace {
    static inline void tests()
    {
      static_assert(fixed_string("abc").size() == 3);
      static_assert(fixed_string("abc").back() == 'c');
      static_assert(fixed_string("abc")[1] == 'b');

      static_assert(fixed_string("a") + fixed_string("b") == fixed_string("ab"));
      static_assert(fixed_string("a") != fixed_string("aa"));
    }
  }
} // namespace circ::meta