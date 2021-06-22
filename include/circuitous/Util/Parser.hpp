 // Copyright 2020, Trail of Bits. All rights reserved.

#pragma once

#include <cstddef>
#include <string_view>
#include <type_traits>
#include <optional>
#include <variant>
#include <concepts>
#include <tuple>

namespace circuitous::parser
{
  using parse_input_t = std::string_view;

  template< typename T >
  using parse_result_t = std::optional< std::pair< T, std::string_view > >;

  template< typename P >
  using opt_pair_parse_t = std::invoke_result_t< P, parse_input_t >;

  template< typename P >
  using pair_parse_t = typename opt_pair_parse_t< P >::value_type;

  template< typename P >
  using parse_t = typename pair_parse_t< P >::first_type;

  template< typename P, typename T >
  concept parser = requires(P &&p) {
    { p(std::declval< parse_input_t >()) } -> std::same_as< parse_result_t< T > >;
  };

  template< typename T >
  using parser_t = auto (*)( parse_input_t ) -> parse_result_t< T >;

  template < class T >
  concept integral = std::is_integral_v<T>;

  template< typename P >
  constexpr const auto& result(const P &p) { return p->first; }

  template< typename P >
  constexpr auto& result(P &p) { return p->first; }

  template< typename P >
  constexpr parse_input_t rest(const P &p) { return p->second; }

  template< typename F, typename P, typename T = std::result_of_t<F(parse_t<P>)> >
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
            typename T =  std::result_of_t< F(parse_t<P>, parse_input_t) > >
  constexpr auto bind(P&& p, F&& f)
  {
    return [=] (parse_input_t in) -> T {
      if (auto res = p(in)) {
        const auto [val, rest] = res.value();
        return f(val, rest);
      }
      return std::nullopt;
    };
  }

  template< typename T >
  constexpr parser<T> auto lift(T &&t)
  {
    return [t = std::forward< T >(t)] (parse_input_t in) -> parse_result_t< T > {
      return {{std::move(t), in}};
    };
  }

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

  template< typename P1, typename P2,
            typename T = std::enable_if_t< std::is_same_v< parse_t<P1>, parse_t<P2> > > >
  constexpr parser<T> auto operator|(P1&& p1, P2&& p2)
  {
    return [=] (parse_input_t in) {
      if (auto r1 = p1(in))
        return r1;
      return p2(in);
    };
  }

  template< typename P1, typename P2, typename F,
            typename T = std::result_of_t< F(parse_t<P1>, parse_t<P2>) > >
  constexpr parser<T> auto combine(P1 &&p1, P2 &&p2, F&& f)
  {
    return [=] (parse_input_t in) -> parse_result_t< T > {
      if (auto r1 = p1(in))
        if (auto r2 = p2(rest(r1)))
          return {{f(result(r1), result(r2)), rest(r2)}};
      return std::nullopt;
    };
  }

  template< typename P1, typename P2,
            typename L = parse_t<P1>, typename R = parse_t<P2> >
  constexpr parser<R> auto operator<(P1 &&p1, P2 &&p2)
  {
    return combine(std::forward<P1>(p1),
                   std::forward<P2>(p2),
                   [] (auto, const auto& r) { return r; });
  }

  template< typename P1, typename P2,
            typename L = parse_t<P1>, typename R = parse_t<P2> >
  constexpr parser<L> auto operator>(P1 &&p1, P2 &&p2)
  {
    return combine(std::forward<P1>(p1),
                   std::forward<P2>(p2),
                   [] (const auto& l, auto) { return l; });
  }

  template< typename P, typename T = parse_t<P> >
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

  constexpr parser<char> auto char_parser(char c)
  {
    return [=] (parse_input_t in) -> parse_result_t<char> {
      if (in.empty() || in.front() != c)
        return std::nullopt;
      return {{c, in.substr(1)}};
    };
  }

  constexpr parser<std::string_view> auto string_parser(std::string_view pattern)
  {
    return [=] (parse_input_t in) -> parse_result_t<std::string_view> {
      if (in.starts_with(pattern))
        return {{pattern, in.substr(pattern.size())}};
      return std::nullopt;
    };
  }

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

  constexpr parser<char> auto char_parser(auto &&predicate)
  {
    return [predicate = std::move(predicate)] (parse_input_t in)
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

  constexpr auto skip(auto &&predicate)
  {
    auto parser = char_parser(std::move(predicate));
    return many(parser, std::monostate{}, [] (auto m, auto) { return m; });
  }

  constexpr parser<std::size_t> auto word_length_parser()
  {
    auto accum = [](std::size_t res, auto) -> std::size_t { return ++res; };
    return many(letter_parser(), std::size_t(0), accum);
  }

  constexpr parser<parse_input_t> auto word_parser()
  {
    return [] (parse_input_t in) -> parse_result_t<parse_input_t> {
      if (auto l = word_length_parser()(in)) {
        auto length = result(l);
        return {{in.substr(0, length), in.substr(length)}};
      }

      return std::nullopt;
    };
  }

  namespace {
    constexpr static inline void tests()
    {
      using namespace std::literals;

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
        constexpr auto p = word_length_parser()("test abc"sv);
        static_assert( p && result(p) == 4 );
      }

      {
        constexpr auto p = word_parser()("word abc"sv);
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

} // namespace circuitous::parser