/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/StrongType.hpp>
#include <glog/logging.h>

#include <circuitous/Util/Parser.hpp>
#include <circuitous/Util/ConstExprVector.hpp>
#include <circuitous/Util/FixedString.hpp>

#include <algorithm>
#include <cctype>
#include <cstring>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>
#include <optional>
#include <variant>
#include <string>
#include <span>
#include <charconv>
#include <unordered_map>
#include <string_view>
#include <iostream>
#include <concepts>
#include <unordered_set>

namespace circ::eqsat {

  // helper for pattern visitor
  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

  using namespace circ::parser;

  /* Pattern atoms */

  struct constant_tag;
  using constant = strong_type< std::int64_t, constant_tag >;

  struct op_tag;
  using operation = strong_type< std::string_view, op_tag >;

  struct placeholder_tag;
  using place = strong_type< std::string_view, placeholder_tag >;

  using atom = std::variant< constant, operation, place >;

  template< typename stream >
  stream& operator<<(stream& os, const atom& a)
  {
    std::visit( [&os] (const auto &v) { os << v; }, a);
    return os;
  }

  struct expr : std::variant< atom, std::vector< expr > >
  {
    using variant::variant;
  };

  using expr_list = std::vector< expr >;

  template< typename stream >
  stream& operator<<(stream& os, const expr_list& e)
  {
    os << "( ";
    for (const auto &v : e)
      os << v << ' ';
    os << ')';
    return os;
  }

  template< typename stream >
  stream& operator<<(stream& os, const expr& e)
  {
    std::visit( overloaded { [&] (const auto &a) { os << a; }, }, e);
    return os;
  }

  // TODO(Heno): constexpr
  static expr wrap(expr e) {
    if (std::holds_alternative<atom>(e)) {
      std::vector<expr> vec{};
      vec.push_back(std::move(e));
      return vec;
    }
    return e;
  }

  template< typename P, typename T >
  concept parser = requires(P &&p) {
    { p(std::declval< parse_input_t >()) } -> std::same_as< parse_result_t< T > >;
  };

  constexpr parser<constant> auto constant_parser()
  {
    using value_t = constant::underlying_t;
    auto value = [] (value_t v) { return constant(v); };
    return fmap( value, number_parser<value_t>() );
  }

  constexpr parser<operation> auto operation_parser()
  {
    auto name = [] (std::string_view name) { return operation(name); };
    return fmap( name, (string_parser("op_") < word_parser()) );
  }

  constexpr parser<place> auto place_parser()
  {
    auto name = [] (std::string_view name) { return place(name); };
    return fmap( name, (char_parser('?') < word_parser()) );
  }

  constexpr parser<atom> auto atom_parser()
  {
    auto to_atom_parser = [] (auto &&parser) {
      auto to_atom = [] (auto &&v) { return atom(v); };
      return fmap( to_atom, std::move(parser) );
    };

    auto con = to_atom_parser(constant_parser());
    auto op  = to_atom_parser(operation_parser());
    auto plc = to_atom_parser(place_parser());

    return con | op | plc;
  }

  template< typename P, typename T = parse_type<P> >
  constexpr parser<T> auto parenthesized(P &&p)
  {
    return char_parser('(') < p > char_parser(')');
  }

  // TODO(Heno): make constexpr
  struct expr_parser
  {
    using parser_result = parse_result_t< expr >;
    using parser_t = auto (*)(parse_input_t) -> parser_result;

    static auto atom()
    {
      return fmap([] (auto a) { return expr(a); }, atom_parser() );
    }

    static inline auto push = [] (expr a, expr b) -> expr {
      auto vec = std::get<expr_list>(wrap(a));
      vec.push_back(b);
      return vec;
    };

    static auto list()
    {
      return parenthesized(
        separated(element_parser(), skip(isspace), expr(std::vector<expr>{}), push)
      );
    }

    static auto element_parser() -> parser_t
    {
      return [] (parse_input_t in) -> parser_result {
        return (atom() | list())(in);
      };
    }

    static auto parser() { return list(); }
  };

  atom root(const auto& e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> atom { return a; },
      [] (const expr_list &vec) -> atom { return std::get<atom>(vec.front()); }
    }, e);
  }

  expr_list children(const auto& e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> expr_list { return {}; },
      [] (const expr_list &vec) -> expr_list {
        return { std::next(vec.begin()), vec.end() };
      }
    }, e);
  }

  using pattern = expr;

  // TODO(Heno): constexpr
  static inline parser<pattern> auto pattern_parser() { return expr_parser::parser(); }

  // TODO(Heno): constexpr
  static inline pattern make_pattern(std::string_view pat)
  {
    return result(pattern_parser()(pat));
  }

  std::unordered_set<place> places(const auto &vec)
  {
    std::unordered_set<place> res;
    for (const auto &v : vec)
      res.merge(places(v));
    return res;
  }

  static inline std::unordered_set<place> places(const expr &e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> std::unordered_set<place> {
        if (auto p = std::get_if<place>(&a))
          return {*p};
        return {};
      },
      [] (const expr_list &vec) {
        return places(vec);
      }
    }, e);
  }

  struct pattern_with_places
  {
    using places_map = std::unordered_map< eqsat::place, unsigned >;
    explicit pattern_with_places(const pattern &pat)
      : expr(pat)
    {
      unsigned id = 0;
      for (const auto &plc : eqsat::places(pat))
        places[plc] = id++;
    }

    explicit pattern_with_places(const pattern &pat, const places_map &map)
      : expr(pat), places(map)
    {}

    pattern expr;
    places_map places;
  };

  namespace {
    static inline void tests()
    {
      using namespace std::literals;

      {
        constexpr auto p = constant_parser()("17");
        static_assert( p && result(p).ref() == 17 );
      }

      {
        constexpr auto p = operation_parser()("op_add");
        static_assert( p && result(p).ref() == "add" );
      }

      static_assert( !operation_parser()("add") );

      {
        constexpr auto p = atom_parser()("op_mul");
        static_assert( p && std::holds_alternative<operation>( result(p) ) );
      }

      {
        constexpr auto p = atom_parser()("-10");
        static_assert( p && std::holds_alternative<constant>( result(p) ) );
      }

      {
        constexpr auto p = atom_parser()("?x");
        static_assert( p && std::holds_alternative<place>( result(p) ) );
      }
    }
  } // anonymous namespace

} // namespace circ::eqsat
