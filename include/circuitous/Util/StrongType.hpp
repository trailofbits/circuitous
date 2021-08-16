/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <compare>
#include <string>
#include <type_traits>

namespace circ {

  template< typename underlying, typename tag >
  struct strong_type
  {
    using underlying_t = underlying;
    constexpr strong_type() : _value() {}

    explicit constexpr strong_type(const underlying_t &value) : _value(value) {}
    explicit constexpr strong_type(underlying_t &&value)
        noexcept( std::is_nothrow_move_constructible_v<underlying_t> )
      : _value(std::move(value))
    {}

    explicit constexpr operator underlying_t&() noexcept { return _value; }
    explicit constexpr operator const underlying_t&() const noexcept { return _value; }
    explicit constexpr operator underlying_t&&() noexcept { return std::move(_value); }

    constexpr underlying_t& ref() noexcept { return _value; }
    constexpr const underlying_t& ref() const noexcept { return _value; }

    friend void swap(strong_type &a, strong_type &b) noexcept
    {
      using std::swap;
      swap(static_cast<underlying_t&>(a), static_cast<underlying_t&>(b));
    }

    friend bool operator==(const strong_type &a, const strong_type &b) = default;
    friend bool operator!=(const strong_type &a, const strong_type &b) = default;

    friend bool operator==(const strong_type &a, const underlying_t &b) { return a._value == b; }
    friend bool operator!=(const strong_type &a, const underlying_t &b) { return !(a == b); }

    friend bool operator==(const underlying_t &a, const strong_type &b) { return a == b._value; }
    friend bool operator!=(const underlying_t &a, const strong_type &b) { return !(a == b); }

    friend bool operator<(const strong_type &a, const strong_type &b)  { return a._value < b._value; }
    friend bool operator<(const strong_type &a, const underlying_t &b) { return a._value < b; }
    friend bool operator<(const underlying_t &a, const strong_type &b) { return a < b._value; }

    template< typename stream >
    friend auto operator<<(stream &out, const strong_type &a) noexcept -> decltype( out << "" )
    {
      if constexpr ( std::is_integral_v< underlying_t > ) {
        return out << std::to_string(a._value);
      } else {
        return out << a._value;
      }
    }

  private:
    underlying_t _value;
  };
} // namespace circ

namespace std {
  template< typename underlying_t, typename tag >
  struct hash< circ::strong_type< underlying_t, tag > >
  {
    using underlying_hash = std::hash<underlying_t>;

    size_t operator()(const circ::strong_type< underlying_t, tag > &a) const noexcept
    {
      return underlying_hash{}(static_cast< const underlying_t& >(a));
    }
  };
}
