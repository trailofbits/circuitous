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
#include <stdexcept>
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
  // operation has to be named with prefix 'op_'
  using operation = strong_type< std::string_view, op_tag >;

  struct placeholder_tag;
  // place has to be named with prefix '?'
  using place = strong_type< std::string_view, placeholder_tag >;

  struct label_tag;
  // subexpression label has to be named with prefix '$'
  using label = strong_type< std::string_view, label_tag >;

  struct context_tag;
  using context = strong_type< std::string_view, context_tag >;

  using atom = std::variant< constant, operation, place, label >;

  template< typename stream >
  stream& operator<<(stream& os, const atom& a)
  {
    std::visit( [&os] (const auto &v) { os << v; }, a);
    return os;
  }

  // expression of form: (match [labels])
  // serves to match multiple labeled expressions
  struct match_expr { std::vector<label> labels; };

  // expression of form: (union [labels])
  // serves to unify matched labels on the right hand side of the rule
  struct union_expr { std::vector<label> labels; };

  // expression of form: (disjoint [context])
  // serves to specify disjoint contexts
  struct disjoint_expr { std::vector<context> contexts; };

  using contexts_constraints = disjoint_expr;

  using context_name = std::string_view;

  struct expr : std::variant< atom, std::vector< expr >, match_expr, union_expr >
  {
    using variant::variant;

    expr(const expr &e) = default;
    expr(expr &&e) = default;

    expr(const variant &e, context_name c) : variant(e), context(c) {}
    expr(variant &&e, context_name c) : variant(std::move(e)), context(c) {}

    expr& operator=(const expr &) = default;
    expr& operator=(expr &&) = default;

    constexpr const variant &get() const { return *this; }
    constexpr variant &get() { return *this; }

    std::optional<context_name> context = std::nullopt;
  };

  struct named_expr : expr
  {
    using name_type = std::string_view;

    named_expr(name_type n, const expr &e) : expr(e), name(n) {}
    named_expr(name_type n, expr &&e) : expr(std::move(e)), name(n) {}

    label name;
  };

  struct pattern : expr
  {
    using named_exprs = std::unordered_map< label, expr >;

    explicit pattern(const expr &e) : expr(e) {}
    explicit pattern(expr &&e) : expr(std::move(e)) {}

    pattern(const named_exprs &subs, const contexts_constraints &c, const expr &e)
      : expr(e), constraints(c), subexprs(subs)
    {}

    pattern(named_exprs &&subs, contexts_constraints &&c, expr &&e)
      : expr(std::move(e)), constraints(std::move(c)), subexprs(std::move(subs))
    {}

    contexts_constraints constraints;
    named_exprs subexprs;
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

  constexpr parser<std::string_view> auto name_parser()
  {
    return [] (parse_input_t in) -> parse_result_t<std::string_view> {
      if (auto prefix = length_parser(isalpha)(in); prefix && result(prefix) > 0) {
        if (auto suffix = length_parser(isdigit)(rest(prefix))) {
          auto length = result(prefix) + result(suffix);
          return {{in.substr(0, length), in.substr(length)}};
        }
      }

      return std::nullopt;
    };
  }


  constexpr parser<operation> auto operation_parser()
  {
    return construct< operation >(string_parser("op_") < name_parser());
  }

  constexpr parser<place> auto place_parser()
  {
    return construct< place >(char_parser('?') < name_parser());
  }

  constexpr parser<label> auto label_parser()
  {
    return construct< label >(char_parser('$') < name_parser());
  }

  constexpr parser<context> auto context_parser()
  {
    return construct< context >(name_parser());
  }

  constexpr parser<atom> auto atom_parser()
  {
    auto con = construct< atom >(constant_parser());
    auto op  = construct< atom >(operation_parser());
    auto plc = construct< atom >(place_parser());
    auto lab = construct< atom >(label_parser());

    return con | op | plc | lab;
  }

  template< typename P, typename T = parse_type<P> >
  constexpr parser<T> auto parenthesized(P &&p)
  {
    return char_parser('(') < p > char_parser(')');
  }

  // TODO(Heno): make constexpr
  namespace detail
  {
    struct expr_parser
    {
      using parser_result = parse_result_t< expr >;
      using parser_t = auto (*)(parse_input_t) -> parser_result;

      static auto atom() { return construct< expr >(atom_parser()); }

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
  } // namespace detail

  static inline parser<expr> auto expr_parser()
  {
    return detail::expr_parser::parser();
  }

  // expression can be suffixed with context name as: <expr>:context
  static inline parser<expr> auto expr_with_context_parser()
  {
    auto context_suffix = char_parser(':') < name_parser();
    auto expression = detail::expr_parser::parser();
    return from_tuple< expr >(expression & context_suffix) | expression;
  }

  // TODO(Heno): constexpr
  // named expression has form: (let <name> <expr>)
  static inline parser<named_expr> auto named_expr_parser()
  {
    auto name_p = (string_parser("let") & skip(isspace)) < name_parser();
    auto expr_p = (skip(isspace) < expr_with_context_parser());

    return from_tuple< named_expr >( parenthesized( name_p & expr_p ) );
  }

  template< typename P >
  static inline parser< std::vector<parse_type<P>> > auto to_vector_parser(P &&p)
  {
    using parsed_type = parse_type<P>;
    auto push = [] (std::vector<parsed_type> vec, parsed_type v) {
      vec.push_back(std::move(v));
      return std::move(vec);
    };

    return separated(std::forward<P>(p), skip(isspace), std::vector<parsed_type>(), push);
  }

  static inline parser<std::vector<label>> auto labels_parser()
  {
    return to_vector_parser(label_parser());
  }

  template< typename Expr >
  static inline parser<Expr> auto match_label_list(std::string_view prefix)
  {
    auto match_prefix = (string_parser(prefix) & skip(isspace));
    return construct< Expr >(parenthesized(match_prefix < labels_parser()));
  }

  // TODO(Heno): constexpr
  // match expression has form: (match [labels])
  static inline parser<match_expr> auto match_expr_parser()
  {
    return match_label_list<match_expr>("match");
  }

  // TODO(Heno): constexpr
  // match expression has form: (union [labels])
  static inline parser<union_expr> auto union_expr_parser()
  {
    return match_label_list<union_expr>("union");
  }

  static inline parser<std::vector<context>> auto contexts_parser()
  {
    return to_vector_parser(context_parser());
  }

  // TODO(Heno): constexpr
  // match expression has form: (disjoint [labels])
  static inline parser<disjoint_expr> auto disjoint_expr_parser()
  {
    auto match_prefix = (string_parser("disjoint") & skip(isspace));
    return construct< disjoint_expr >(parenthesized(match_prefix < contexts_parser()));
  }

  atom root(const auto& e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> atom { return a; },
      [] (const expr_list &vec) -> atom { return std::get<atom>(vec.front()); },
      [] (const auto&) -> atom {
        throw "unsupported expression type";
      }
    }, e.get());
  }

  expr_list children(const auto& e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> expr_list { return {}; },
      [] (const expr_list &vec) -> expr_list {
        return { std::next(vec.begin()), vec.end() };
      },
      [] (const auto&) -> expr_list {
        throw "unsupported expression type";
      }
    }, e.get());
  }

  // TODO(Heno): constexpr
  static inline parser<pattern> auto pattern_parser()
  {
    using named_exprs = pattern::named_exprs;

    // TODO(Heno): do not copy exprs
    auto insert_named_expr = [] (auto &&exprs, auto &&e) {
      exprs[e.name] = e;
      return exprs;
    };

    auto subexpr_parser = many(
      (named_expr_parser() > skip(isspace)), named_exprs{}, insert_named_expr
    );

    auto constraints = option(contexts_constraints{}, (disjoint_expr_parser() > skip(isspace)));

    return
      // pattern is either expression
      construct< pattern >( expr_parser() ) |
      // or list of named expressions and final anonymous expression
      // that we are matching against
      from_tuple< pattern >( parenthesized( combine(subexpr_parser, constraints, expr_parser()) ) ) |
      // or list of named expressions and final match expression
      // that allows to specify multi-pattern rules
      from_tuple< pattern >( parenthesized( combine(subexpr_parser, constraints, match_expr_parser()) ) ) |
      // or union expression that allows to specify unification of matched rules
      construct< pattern >( union_expr_parser() );
  }

  // TODO(Heno): constexpr
  static inline pattern make_pattern(std::string_view pat)
  {
    auto parser = pattern_parser();
    if (auto p = parser(pat))
      return result(std::move(p));
    throw std::runtime_error("syntax error in pattern");
  }

  using places_t = std::unordered_set<place>;
  inline places_t places(const expr &e, const auto &subexprs)
  {
    auto foreach_places = [&] (const auto &vec) {
        places_t res;
        for (const auto &v : vec)
          res.merge(places(v, subexprs));
        return res;
    };

    return std::visit( overloaded {
      [&] (const atom &a) -> places_t {
        return std::visit( overloaded {
          [&] (const label &lab) -> places_t { return places(subexprs.at(lab), subexprs); },
          [&] (const place &val) -> places_t { return {val}; },
          [&] (const auto &)     -> places_t { return {}; }
        }, a);
      },
      [&] (const expr_list &list) -> places_t { return foreach_places(list); },
      [&] (const match_expr &e)   -> places_t { return foreach_places(e.labels); },
      [&] (const union_expr &e)   -> places_t { return foreach_places(e.labels); },
      [] (const auto&) -> places_t { throw "unsupported expression type"; }
    }, e.get());
  }

  inline places_t places(const pattern &pat)
  {
    return places(pat, pat.subexprs);
  }

  using indexed_places = std::unordered_map< eqsat::place, unsigned >;
  inline indexed_places get_indexed_places(const pattern &pat)
  {
    indexed_places places;
    unsigned id = 0;
    for (const auto &plc : eqsat::places(pat))
      places[plc] = id++;
    return places;
  }

  template< typename stream >
  auto operator<<(stream &os, const pattern &pat) -> decltype(os << "")
  {
    os << '(' << root(pat);
    for (const auto &ch : children(pat))
      os << ' ' << ch;
    return os << ')';
  }

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
