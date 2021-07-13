 // Copyright 2021, Trail of Bits. All rights reserved.

#pragma once

#include <cstddef>
#include <string_view>
#include <type_traits>
#include <optional>
#include <variant>
#include <concepts>
#include <tuple>

#include <circuitous/Util/FixedString.hpp>
#include <circuitous/Util/ConstExprVector.hpp>

// This file implements parsec-like parser utilities for constexpr context.
// Its main building blocks are parser combinators that are high-order functions
// that combine smaller combinators (parsers) to build more complex ones.

namespace circ::parser
{
  template< typename T >
  concept integral = std::is_integral_v<T>;

  template< typename F, typename... Args >
  concept invocable = requires(F &&f, Args&& ...args) {
    std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  };

  using parse_input_t = std::string_view;

  // A parser is callable that takes string and return
  // optional result and remaining (unparsed) string.

  template< typename P >
  concept Parser = invocable<P, parse_input_t >;

  template< typename T >
  using parse_result_t = std::optional< std::pair< T, std::string_view > >;

  template< Parser P >
  using parse_invoke_result = std::invoke_result_t< P, parse_input_t >;

  template< Parser P >
  using parse_result_type = typename parse_invoke_result< P >::value_type;

  template< Parser P >
  using parse_type = typename parse_result_type< P >::first_type;

  template< typename P, typename T >
  concept parser = Parser< P > &&
    std::is_same_v< parse_invoke_result< P >, parse_result_t< T > >;

  template< typename T >
  using parser_t = auto (*)( parse_input_t ) -> parse_result_t< T >;

  constexpr const auto& result(const auto &p) { return p->first; }
  constexpr auto& result(auto &p) { return p->first; }
  constexpr auto result(auto &&p) { return std::move(p->first); }

  constexpr parse_input_t rest(const auto &p) { return p->second; }

  // --- monad parser functions ---

  // fmap applies function 'f' to parser result
  template< typename F, typename P, typename T = std::invoke_result_t< F, parse_type<P> > >
  constexpr parser<T> auto fmap(F &&f, P &&p)
  {
    return [f = std::forward< F >(f), p = std::forward< P >(p)] (parse_input_t in)
      -> parse_result_t< T >
    {
      if (auto res = p(in)) {
        const auto [val, rest] = res.value();
        return {{f(val), rest}};
      }
      return std::nullopt;
    };
  }

  template< typename P, typename F,
            typename T =  std::invoke_result_t< F, parse_type<P>, parse_input_t > >
  constexpr parser<T> auto bind(P&& p, F&& f)
  {
    return [=] (parse_input_t in) -> T {
      if (auto res = p(in)) {
        const auto [val, rest] = res.value();
        return f(val, rest);
      }
      return std::nullopt;
    };
  }

  // lifts a value into parser
  template< typename T >
  constexpr parser<T> auto lift(T &&t)
  {
    return [t = std::forward< T >(t)] (parse_input_t in) -> parse_result_t< T > {
      return {{t, in}};
    };
  }

  // always failing parser
  template< typename T >
  constexpr parser<T> auto fail(T)
  {
    return [=] (parse_input_t) -> parse_result_t< T > {
      return std::nullopt;
    };
  }

  template< typename T, typename Err >
  constexpr parser<T> auto fail(T&&, Err &&err)
  {
    return [=] (parse_input_t) -> parse_result_t< T > {
      err();
      return std::nullopt;
    };
  }

  // --- parser combinators ---

  // alternation: frirst try P1, and if it fails, try P2.
  // Both parsers have to return the same type.
  template< typename P1, typename P2,
            typename T = std::enable_if_t< std::is_same_v< parse_type<P1>, parse_type<P2> > > >
  constexpr parser<T> auto operator|(P1&& p1, P2&& p2)
  {
    return [=] (parse_input_t in) {
      if (auto r1 = p1(in))
        return r1;
      return p2(in);
    };
  }

  // accumulation: combine results of sequential application of both parsers
  template< typename P1, typename P2, typename F,
            typename T = std::invoke_result_t< F, parse_type<P1>, parse_type<P2> > >
  constexpr parser<T> auto combine(P1 &&p1, P2 &&p2, F&& f)
  {
    return [=] (parse_input_t in) -> parse_result_t< T > {
      if (auto r1 = p1(in))
        if (auto r2 = p2(rest(r1)))
          return {{f(result(r1), result(r2)), rest(r2)}};
      return std::nullopt;
    };
  }

  // accumulate: parser results to tuple
  template< typename P, typename T = std::tuple< parse_type<P> > >
  constexpr parser<T> auto combine(P &&p)
  {
    return [=] (parse_input_t in) -> parse_result_t< T > {
      if (auto r = p(in))
        return {{std::make_tuple(result(r)), rest(r)}};
      return std::nullopt;
    };
  }

  template< typename P, typename ...Ps
          , typename T = std::tuple< parse_type<P>, parse_type<Ps>... > >
  constexpr parser<T> auto combine(P &&p, Ps&&... ps)
  {
    return [=] (parse_input_t in) -> parse_result_t< T > {
      if (auto r1 = combine(p)(in))
        if (auto r2 = combine(ps...)(rest(r1)))
          return {{std::tuple_cat(result(r1), result(r2)), rest(r2)}};
      return std::nullopt;
    };
  }

  template< typename P1, typename P2, typename T = std::tuple< parse_type<P1>, parse_type<P2> > >
  constexpr parser<T> auto operator&(P1 &&p1, P2 &&p2)
  {
    return combine(std::forward<P1>(p1), std::forward<P2>(p2));
  }

  // combine two parsers and return the result of the second one
  template< typename P1, typename P2,
            typename L = parse_type<P1>, typename R = parse_type<P2> >
  constexpr parser<R> auto operator<(P1 &&p1, P2 &&p2)
  {
    return combine(std::forward<P1>(p1),
                   std::forward<P2>(p2),
                   [] (auto, const auto& r) { return r; });
  }

  // combine two parsers and return the result of the first one
  template< typename P1, typename P2,
            typename L = parse_type<P1>, typename R = parse_type<P2> >
  constexpr parser<L> auto operator>(P1 &&p1, P2 &&p2)
  {
    return combine(std::forward<P1>(p1),
                   std::forward<P2>(p2),
                   [] (const auto& l, auto) { return l; });
  }

  // try to apply a parser, and if it fails, return a default value
  template< typename P, typename T = parse_type<P> >
  constexpr parser<T> auto option(T &&def, P &&p)
  {
    return [p = std::forward<P>(p), def = std::forward<T>(def)] (parse_input_t in)
      -> parse_result_t< T >
    {
      if (auto res = p(in))
        return res;
      return {{def, in}};
    };
  }

  template <typename P, typename T, typename F>
  constexpr auto accumulate(parse_input_t in, P &&p, T init, F &&f)
    -> std::pair<T, parse_input_t>
  {
    while (!in.empty()) {
      if (auto res = p(in)) {
        auto [val, rest] = res.value();
        init = f(init, val);
        in = rest;
      } else {
        return {init, in};
      }
    }
    return {init, in};
  }

  // apply parser 'p' zero or more times, accumulating the results according to a
  // function 'f' with initial accumulator value 'init'
  template <typename P, typename T, typename F>
  constexpr parser<T> auto many(P &&p, T &&init, F &&f)
  {
    return [p = std::forward<P>(p), init = std::forward<T>(init),
            f = std::forward<F>(f)] (parse_input_t in)
      -> parse_result_t< std::decay_t< T > >
    {
      return {accumulate(in, p, init, f)};
    };
  }

  // apply parser 'p' at least once
  template <typename P, typename T, typename F>
  constexpr parser<T> auto many1(P &&p, T &&init, F &&f)
  {
    return [p = std::forward<P>(p), init = std::forward<T>(init),
            f = std::forward<F>(f)] (parse_input_t in)
      -> parse_result_t< std::decay_t< T > >
    {
      if (auto res = p(in)) {
        auto [val, rest] = res.value();
        return {accumulate(rest, p, f(init, val), f)};
      }
      return std::nullopt;
    };
  }

  // parse character 'c'
  constexpr parser<char> auto char_parser(char c)
  {
    return [=] (parse_input_t in) -> parse_result_t<char> {
      if (in.empty() || in.front() != c)
        return std::nullopt;
      return {{c, in.substr(1)}};
    };
  }

  // parse string 'pattern'
  constexpr parser<std::string_view> auto string_parser(std::string_view pattern)
  {
    return [=] (parse_input_t in) -> parse_result_t<std::string_view> {
      if (in.starts_with(pattern))
        return {{pattern, in.substr(pattern.size())}};
      return std::nullopt;
    };
  }

  // parse character ∈ chars
  constexpr parser<char> auto one_of(std::string_view chars)
  {
    return [=] (parse_input_t in) -> parse_result_t<char> {
      if (in.empty())
        return std::nullopt;
      if (chars.find(in.front()) != chars.npos)
        return {{in.front(), in.substr(1)}};
      return std::nullopt;
    };
  }

  // parse character ∉ chars
  constexpr parser<char> auto none_of(std::string_view chars)
  {
    return [=] (parse_input_t in) -> parse_result_t<char> {
      if (in.empty())
        return std::nullopt;
      if (chars.find(in.front()) == chars.npos)
        return {{in.front(), in.substr(1)}};
      return std::nullopt;
    };
  }

  // accumulate parsed values by parser p1, that are separated by
  // strings parseble by p2
  template< typename P1, typename P2, typename T, typename F >
  constexpr parser<T> auto separated(P1 &&p1, P2 &&p2, T &&init, F &&f)
  {
    return [p1 = std::forward< P1 >(p1), p2 = std::forward< P2 >(p2),
            init = std::forward< T >(init), f = std::forward< F >(f)]
            (parse_input_t s)
      -> parse_result_t<T>
    {
      if (auto r = p1(s)) {
        auto p = p2 < p1;
        return {accumulate(rest(r), p, f(init, result(r)), f)};
      }
      return {{init, s}};
    };
  }

  // --- parser constructor helpers ---

  template< typename T, typename P >
  constexpr parser<T> auto from_tuple(P &&p)
  {
    return fmap([] (auto &&t) {
      return std::make_from_tuple< T >(std::forward<decltype(t)>(t));
    }, std::forward<P>(p));
  }

  template< typename T, typename P >
  constexpr parser<T> auto construct(P &&p)
  {
    return fmap([] (auto &&arg) { return T{std::forward<decltype(arg)>(arg)}; }, std::forward<P>(p) );
  }

  namespace detail
  {
    template< integral I >
    constexpr parser<I> auto digit_parser(std::string_view allowed_digits)
    {
      using namespace std::literals;
      auto to_digit = [](char c) { return static_cast<I>(c - '0'); };
      return fmap(to_digit, one_of(allowed_digits));
    }
  } // namespace detail

  template< integral I >
  constexpr parser<I> auto digit_parser()
  {
    using namespace std::literals;
    return detail::digit_parser<I>("0123456789"sv);
  }

  template< integral I >
  constexpr parser<I> auto nonzero_parser()
  {
    using namespace std::literals;
    return detail::digit_parser<I>("123456789"sv);
  }

  template< integral I >
  constexpr parser<I> auto zero_parser()
  {
    using namespace std::literals;
    return detail::digit_parser<I>("0"sv);
  }

  template< integral I >
  constexpr parser<I> auto value_parser()
  {
    return bind(
      nonzero_parser<I>(),
      [] (I head, parse_input_t rest) {
        auto accum = [] (I result, I d) {
          return (result * 10) + d;
        };
        return many(digit_parser<I>(), head, accum)(rest);
      }
    );
  }

  template< integral I >
  constexpr parser<I> auto number_parser()
  {
    auto sign_parser = option('+', char_parser('-'));
    return combine(
      sign_parser,
      zero_parser<I>() | value_parser<I>(),
      [] (auto sign, auto value) {
        return sign == '+' ? value : -value;
      }
    );
  }

  constexpr bool isspace(char c) noexcept
  {
    return c == ' ' || c == '\f' || c == '\n' || c == '\r' || c == '\t' || c == '\v';
  }

  constexpr bool islower(char c) noexcept { return 'a' <= c && c <= 'z'; }
  constexpr bool isupper(char c) noexcept { return 'A' <= c && c <= 'Z'; }
  constexpr bool isalpha(char c) noexcept { return islower(c) || isupper(c); }
  constexpr bool isdigit(char c) noexcept { return '0' <= c && c <= '9'; }
  constexpr bool isalphanum(char c) noexcept { return isalpha(c) || isdigit(c); }

  template< typename Pred >
  constexpr parser<char> auto char_parser(Pred &&predicate)
  {
    return [predicate = std::forward<Pred>(predicate)] (parse_input_t in)
      -> parse_result_t<char>
    {
      if (in.empty())
        return std::nullopt;
      if (predicate(in.front()))
        return {{in.front(), in.substr(1)}};
      return std::nullopt;
    };

  }

  constexpr parser<char> auto letter_parser()
  {
    return char_parser(isalpha);
  }

  template< typename Pred >
  constexpr auto skip(Pred &&predicate)
  {
    auto parser = char_parser(std::forward<Pred>(predicate));
    return many(parser, std::monostate{}, [] (auto m, auto) { return m; });
  }

  template< typename Pred >
  constexpr parser<std::size_t> auto length_parser(Pred &&pred)
  {
    auto accum = [](std::size_t res, auto) -> std::size_t { return ++res; };
    return many(char_parser(std::forward<Pred>(pred)), std::size_t(0), accum);
  }

  template< typename Pred >
  constexpr parser<std::string_view> auto word_parser(Pred &&pred)
  {
    return [pred = std::forward<Pred>(pred)] (parse_input_t in)
      -> parse_result_t<std::string_view>
    {
      if (auto l = length_parser(pred)(in)) {
        auto length = result(l);
        if (length > 0)
          return {{in.substr(0, length), in.substr(length)}};
      }

      return std::nullopt;
    };
  }

  namespace {
    constexpr static inline void tests()
    {
      using namespace std::literals;
      using namespace circ::meta;
      {
        constexpr auto p = char_parser('+')("+"sv);
        static_assert( p && result(p) == '+' );
      }

      {
        constexpr auto p = digit_parser<int>()("7"sv);
        static_assert( p && result(p) == 7 );
      }

      {
        constexpr auto p = letter_parser()("a"sv);
        static_assert( p && result(p) == 'a' );
      }

      {
        constexpr auto p = value_parser<int>()("123"sv);
        static_assert( p && result(p) == 123 );
      }

      {
        constexpr auto p = number_parser<int>()("-12"sv);
        static_assert( p && result(p) == -12 );
      }

      {
        constexpr auto p = number_parser<int>()("0"sv);
        static_assert( p && result(p) == 0 );
      }

      {
        constexpr auto p = string_parser("test"sv)("test"sv);
        static_assert( p && result(p) == "test"sv );
      }

      {
        constexpr auto p = length_parser(isalpha)("test abc"sv);
        static_assert( p && result(p) == 4 );
      }

      {
        constexpr auto p = word_parser(isalpha)("word abc"sv);
        static_assert( p && result(p) == "word"sv );
      }

      {
        constexpr auto parser = separated(
          number_parser<int>(), char_parser(','), 0, std::plus<>{}
        );
        constexpr auto p = parser("3,7,12");
        static_assert( p && result(p) == 22);
      }

      {
        constexpr auto parser = separated(
          number_parser<int>(),
          (skip(isspace) < char_parser(',') > skip(isspace)),
          0, std::plus<>{}
        );
        constexpr auto p = parser("3, 7,  12 , 5");
        static_assert( p && result(p) == 27);
      }
    }
  } // anonynmous namespace

} // namespace circ::parser