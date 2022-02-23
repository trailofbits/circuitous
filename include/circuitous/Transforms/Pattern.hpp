/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <algorithm>
#include <cassert>
#include <cctype>
#include <charconv>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/ConstExprVector.hpp>
#include <circuitous/Util/FixedString.hpp>
#include <circuitous/Util/Logging.hpp>
#include <circuitous/Util/Overloads.hpp>
#include <circuitous/Util/Parser.hpp>
#include <circuitous/Util/StrongType.hpp>
#include <concepts>
#include <cstring>
#include <iostream>
#include <memory>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

namespace circ::eqsat {


  using namespace circ::parser;

  /* Pattern atoms */

  struct constant_tag;
  using constant = strong_type< std::int64_t, constant_tag >;

  // operation has to be named with prefix 'op_<name>:bitwidth'
  // with optional bitwidth
  struct operation
  {
    using bitwidth_t = uint32_t;

    operation(std::string_view name) : name(name) {}
    operation(std::string_view name, bitwidth_t bw) : name(name), bitwidth(bw) {}

    std::string full_name() const
    {
      return bitwidth ? std::string(name) + ":" + std::to_string(*bitwidth) : std::string(name);
    }

    bool operator==(const operation&) const = default;

    std::string_view name;
    std::optional< bitwidth_t > bitwidth;
  };

  template< typename stream >
  auto operator<<(stream &out, const operation &op) noexcept -> decltype( out << "" )
  {
    return out << op.full_name();
  }

  struct placeholder_tag;
  // place has to be named with prefix '?'
  using place = strong_type< std::string_view, placeholder_tag >;

  struct label_tag;
  // subexpression label has to be named with prefix '$'
  using unary_label = strong_type< std::string_view, label_tag >;

  struct variadic_label_tag;
  // variadic label has to be named with prefix '$' and suffixed with '...'
  using variadic_label = strong_type< std::string_view, variadic_label_tag >;

  using anonymous_label = std::monostate;

  using label = std::variant< unary_label, variadic_label, anonymous_label >;

  static inline std::string_view label_name(const label &lab)
  {
    return std::visit( overloaded {
      [] (const unary_label &l)     -> std::string_view { return l.ref(); },
      [] (const variadic_label &l)  -> std::string_view { return l.ref(); },
      [] (const anonymous_label &l) -> std::string_view { return "none"; }
    }, lab);
  }

  template< typename stream >
  stream& operator<<(stream& os, const label& lab)
  {
    std::visit( [&os] (const auto &l) { os << label_name(l); }, lab );
    return os;
  }

  inline bool operator==(const label &a, const label &b)
  {
    return label_name(a) == label_name(b);
  }

  using context_name = std::string_view;

  struct single_context_tag;
  using single_context = strong_type<context_name, single_context_tag>;

  struct variadic_context_tag;
  // variadic context has to be suffixed with '...'
  using variadic_context = strong_type<context_name, variadic_context_tag>;

  using context = std::variant<single_context, variadic_context>;

  using atom = std::variant< constant, operation, place, label >;

  template< typename stream >
  stream& operator<<(stream& os, const atom& a)
  {
    std::visit( [&os] (const auto &v) { os << v; }, a);
    return os;
  }

  // expression of form: (match [labels])
  // serves to match multiple labeled expressions
  struct basic_match_expr
  {
    std::vector<label> labels;
  };

  struct commutative_match_expr
  {
    std::vector<label> labels;
  };

  using match_expr = std::variant< basic_match_expr, commutative_match_expr >;

  static inline const auto& labels(const match_expr &e)
  {
    return std::visit([] (const auto &m) -> const auto& { return m.labels; }, e);
  }

  // expression of form: (union [labels])
  // serves to unify matched labels on the right hand side of the rule
  struct union_expr { std::vector<label> labels; };

  static inline const auto& labels(const union_expr &e) { return e.labels; }

  // expression of form: (disjoint [context])
  // serves to specify disjoint contexts
  struct disjoint_expr { std::vector<context> contexts; };

  // expression of form: (equiv [places])
  // serves to specify semantically equivalend places
  struct equiv_expr { std::vector<place> places; };

  // expression of form: (equiv [places])
  // serves to specify semantically equivalend places
  struct bond_expr { std::vector<label> labels; };

  static inline const auto& labels(const bond_expr &e) { return e.labels; }

  using context_constraint = disjoint_expr;
  using place_constraint = equiv_expr;

  using constraint  = std::variant< context_constraint, place_constraint >;
  using constraints = std::vector<constraint>;

  struct expr : std::variant< atom, std::vector< expr >, match_expr, union_expr, bond_expr >
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

  struct named_expr : expr
  {
    named_expr(label n, const expr &e) : expr(e), name(n) {}
    named_expr(label n, expr &&e) : expr(std::move(e)), name(n) {}

    label name;
  };

  static void validate_no_variadic_variables(const label &lab, const auto &subexprs)
  {
    if (!std::holds_alternative< variadic_label >(lab))
      return;

    auto plcs = places( subexprs.at(label_name(lab)), subexprs );
    if ( !plcs.empty() ) {
      throw std::runtime_error("syntax error: pattern contains variables in the variadic expression");
    }
  }

  static void validate_no_variadic_variables(const auto &expr, const auto &subexprs)
  {
    // check that there is no variadic expr with variables
    for (const auto &lab : labels(expr)) {
      validate_no_variadic_variables(lab, subexprs);
    }
  }

  static void validate(const match_expr &e, const auto &subexprs)
  {
    validate_no_variadic_variables(e, subexprs);
  }

  static void validate(const expr_list &e, const auto &subexprs)
  {
    for (const auto &ch : e)
      validate(ch, subexprs);
  }

  static void validate(const atom &e, const auto &subexprs)
  {
    if (std::holds_alternative<label>(e)) {
      validate_no_variadic_variables(std::get<label>(e), subexprs);
    }
  }

  static void validate(const bond_expr &e, const auto &subexprs)
  {
    validate_no_variadic_variables(e, subexprs);
  }

  static void validate(const union_expr &e, const auto &subexprs)
  {
    validate_no_variadic_variables(e, subexprs);
  }

  static void validate(const expr &e, const auto &subexprs)
  {
    std::visit([&] (const auto &a) { validate(a, subexprs); }, e.get());
  }

  struct Pattern : expr
  {
    using named_exprs = std::unordered_map< std::string_view, expr >;

    explicit Pattern(const expr &e) : expr(e) {}
    explicit Pattern(expr &&e) : expr(std::move(e)) {}

    Pattern(const named_exprs &subs, const constraints &c, const expr &e)
      : expr(e), subexprs(subs)
    { filter_constraints(c); validate(*this, subexprs); }

    Pattern(named_exprs &&subs, constraints &&c, expr &&e)
      : expr(std::move(e)), subexprs(std::move(subs))
    { filter_constraints(c); validate(*this, subexprs); }

    void filter_constraints(const constraints &cons)
    {
      for (const auto &con : cons) {
        std::visit( overloaded {
          [&] (const context_constraint &e) { context_constraints.push_back(e); },
          [&] (const place_constraint &e)   { place_constraints.push_back(e); }
        }, con);
      }
    }

    const expr &subexpr(const label &lab) const
    {
      assert(!std::holds_alternative<anonymous_label>(lab));
      return subexprs.at(label_name(lab));
    }

    std::vector< place_constraint > place_constraints;
    std::vector< context_constraint > context_constraints;

    named_exprs subexprs;
  };

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

  constexpr parser<unary_label> auto unary_label_name_parser()
  {
    return construct< unary_label >(name_parser());
  }

  constexpr parser<variadic_label> auto variadic_label_name_parser()
  {
    return construct< variadic_label >(name_parser() > string_parser("..."));
  }

  constexpr parser<label> auto label_name_parser()
  {
    auto single   = construct< label >(unary_label_name_parser());
    auto variadic = construct< label >(variadic_label_name_parser());
    return variadic | single;
  }

  constexpr parser<operation> auto operation_parser()
  {
    auto name = string_parser("op_") < name_parser();
    auto bw   = string_parser(":") < number_parser< uint32_t >();
    return from_tuple< operation >( name & bw ) | construct< operation >( name );
  }

  constexpr parser<place> auto place_parser()
  {
    return construct< place >(char_parser('?') < name_parser());
  }

  constexpr parser<unary_label> auto unary_label_parser()
  {
    return construct< unary_label >(char_parser('$') < name_parser());
  }

  constexpr parser<variadic_label> auto variadic_label_parser()
  {
    return construct< variadic_label >( name_parser() > string_parser("..."));
  }

  constexpr parser<label> auto label_parser()
  {
    return char_parser('$') < label_name_parser();
  }

  constexpr parser<single_context> auto single_context_parser() {
    return construct<single_context>(name_parser());
  }

  constexpr parser<variadic_context> auto variadic_context_parser() {
    return construct<variadic_context>(name_parser() > string_parser("..."));
  }

  constexpr parser<context> auto context_parser()
  {
    auto single = construct<context>(single_context_parser());
    auto variadic = construct<context>(variadic_context_parser());
    return variadic | single;
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
  static inline parser<basic_match_expr> auto basic_match_expr_parser()
  {
    return match_label_list< basic_match_expr >("match");
  }

  static inline parser<commutative_match_expr> auto commutative_match_expr_parser()
  {
    return match_label_list< commutative_match_expr >("commutative-match");
  }

  static inline parser<match_expr> auto match_expr_parser()
  {
    auto basic = construct< match_expr >( basic_match_expr_parser() );
    auto commutative = construct< match_expr >( commutative_match_expr_parser() );
    return basic | commutative;
  }

  // TODO(Heno): constexpr
  // match expression has form: (union [labels])
  static inline parser<union_expr> auto union_expr_parser()
  {
    return match_label_list<union_expr>("union");
  }

  // TODO(Heno): constexpr
  // match expression has form: (union [labels])
  static inline parser<bond_expr> auto bond_expr_parser()
  {
    return match_label_list<bond_expr>("bond");
  }

  static inline parser<std::vector<context>> auto contexts_parser()
  {
    return to_vector_parser(context_parser());
  }

  static inline parser<std::vector<place>> auto places_parser()
  {
    return to_vector_parser(place_parser());
  }

  // TODO(Heno): constexpr
  // match expression has form: (disjoint [labels])
  static inline parser<disjoint_expr> auto disjoint_expr_parser()
  {
    auto match_prefix = (string_parser("disjoint") & skip(isspace));
    return construct< disjoint_expr >(parenthesized(match_prefix < contexts_parser()));
  }

  // TODO(Heno): constexpr
  // match expression has form: (equiv [places])
  static inline parser<equiv_expr> auto equiv_expr_parser()
  {
    auto match_prefix = (string_parser("equiv") & skip(isspace));
    return construct< equiv_expr >(parenthesized(match_prefix < places_parser()));
  }

  static inline parser<constraint> auto constraint_parser()
  {
    auto dis = construct< constraint >(disjoint_expr_parser());
    auto eqv = construct< constraint >(equiv_expr_parser());
    return dis | eqv;
  }

  static inline parser<constraints> auto constraints_parser()
  {
    return to_vector_parser(constraint_parser() > skip(isspace));
  }

  static inline atom root(const expr& e)
  {
    return std::visit( overloaded {
      [] (const atom &a) -> atom { return a; },
      [] (const expr_list &vec) -> atom { return std::get<atom>(vec.front()); },
      [] (const auto&) -> atom {
        throw "unsupported expression type";
      }
    }, e.get());
  }

  static inline expr_list children(const expr& e)
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

  static inline parser<expr> auto expr_parser()
  {
    auto simple = detail::expr_parser::parser();
    auto union_ = construct< expr >( union_expr_parser() );
    auto bond   = construct< expr >( bond_expr_parser() );
    return simple | union_ | bond;
  }

  // expression can be suffixed with context name as: <expr>:context
  static inline parser<expr> auto expr_with_context_parser()
  {
    auto context_suffix = char_parser(':') < name_parser();
    auto expression = expr_parser();
    return from_tuple< expr >(expression & context_suffix) | expression;
  }

  // TODO(Heno): constexpr
  // named expression has form: (let <name> <expr>)
  static inline parser<named_expr> auto named_expr_parser()
  {
    auto name_p = (string_parser("let") & skip(isspace)) < label_name_parser();
    auto expr_p = (skip(isspace) < expr_with_context_parser());

    return from_tuple< named_expr >( parenthesized( name_p & expr_p ) );
  }

  // TODO(Heno): constexpr
  static inline parser<Pattern> auto pattern_parser()
  {
    using named_exprs = Pattern::named_exprs;

    // TODO(Heno): do not copy exprs
    auto insert_named_expr = [] (auto &&exprs, auto &&e) {
      exprs[label_name(e.name)] = e;
      return exprs;
    };

    auto subexpr_parser = many(
      (named_expr_parser() > skip(isspace)), named_exprs{}, insert_named_expr
    );

    auto cons = option(constraints{}, (constraints_parser() > skip(isspace)));

    return
      // Pattern is either expression
      construct< Pattern >( expr_parser() ) |
      // or list of named expressions and final anonymous expression
      // that we are matching against
      from_tuple< Pattern >( parenthesized( combine(subexpr_parser, cons, expr_parser()) ) ) |
      // or list of named expressions and final match expression
      // that allows to specify multi-pattern rules
      from_tuple< Pattern >( parenthesized( combine(subexpr_parser, cons, match_expr_parser()) ) );
  }

  // TODO(Heno): constexpr
  static inline Pattern make_pattern(std::string_view pat)
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
      [&] (const atom &a)         -> places_t { return atom_places(a, subexprs); },
      [&] (const expr_list &list) -> places_t { return foreach_places(list); },
      [&] (const match_expr &e)   -> places_t { return foreach_places(labels(e)); },
      [&] (const union_expr &e)   -> places_t { return foreach_places(e.labels); },
      [&] (const bond_expr  &e)   -> places_t { return foreach_places(e.labels); },
      [] (const auto&) -> places_t { throw "unsupported expression type"; }
    }, e.get());
  }

  inline places_t atom_places(const atom &a, const auto &subexprs)
  {
    return std::visit( overloaded {
      [&] (const label &lab) -> places_t { return places(subexprs.at(label_name(lab)), subexprs); },
      [&] (const place &val) -> places_t { return {val}; },
      [&] (const auto &)     -> places_t { return {}; }
    }, a);
  }

  inline places_t places(const Pattern &pat)
  {
    return places(pat, pat.subexprs);
  }

  using indexed_places = std::unordered_map< eqsat::place, unsigned >;
  inline indexed_places get_indexed_places(const Pattern &pat)
  {
    indexed_places places;
    unsigned id = 0;
    for (const auto &plc : eqsat::places(pat))
      places[plc] = id++;
    return places;
  }

  template< typename stream >
  auto operator<<(stream &os, const Pattern &pat) -> decltype(os << "")
  {
    os << '(' << root(pat);
    for (const auto &ch : children(pat))
      os << ' ' << ch;
    return os << ')';
  }

} // namespace circ::eqsat
