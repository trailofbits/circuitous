/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <gap/core/bigint.hpp>
#include <gap/core/overloads.hpp>
#include <gap/core/parser.hpp>
#include <gap/core/recursive_generator.hpp>
#include <gap/core/strong_type.hpp>

#include <optional>
#include <unordered_set>
#include <variant>
#include <vector>
#include <iostream>
#include <sstream>

#include <fmt/format.h>
#include <spdlog/spdlog.h>

namespace eqsat
{

    //
    // label ::= anonymous | simple | variadic
    //
    using name_t          = std::string;
    using anonymous_label = std::monostate;

    static inline name_t label_name(const anonymous_label&) { return "anonymous"; }

    // subexpression label has to be named with prefix '$'
    struct unary_label_tag;
    using unary_label = gap::strong_type< name_t, unary_label_tag >;

    static inline name_t label_name(const unary_label& label) { return label.ref(); }

    struct variadic_label_tag;
    // variadic label has to be named with prefix '$' and suffixed with '...'
    using variadic_label = gap::strong_type< name_t, variadic_label_tag >;

    static inline name_t label_name(const variadic_label& label) { return label.ref(); }

    using label_t = std::variant< unary_label, variadic_label, anonymous_label >;

    static inline name_t label_name(const label_t& label) {
        return std::visit([](const auto& l) { return label_name(l); }, label);
    }

    static inline bool operator==(const label_t &label, std::string_view other) {
        return label_name(label) == other;
    }

    static inline bool operator==(std::string_view other, const label_t &label) {
        return (label == other);
    }

    static inline bool operator!=(const label_t &label, std::string_view other) {
        return !(label == other);
    }

    static inline bool operator!=(std::string_view other, const label_t &label) {
        return !(other == label);
    }

    template< typename stream >
    decltype(auto) operator<<(stream& os, const label_t& label) {
        return os << label_name(label);
    }

    //
    // context ::= single | variadic
    //
    struct single_context_tag;
    using single_context = gap::strong_type< name_t, single_context_tag >;

    struct variadic_context_tag;
    // variadic context has to be suffixed with '...'
    using variadic_context = gap::strong_type< name_t, variadic_context_tag >;

    using context_t = std::variant< single_context, variadic_context >;

    template< typename stream >
    stream& operator<<(stream& os, const context_t& ctx) {
        return std::visit([&](const auto& c) -> stream& { return os << c; }, ctx);
    }

    //
    // atom ::= constant | operation | place | label
    //
    struct constant_tag;
    using constant_t = gap::strong_type< gap::bigint, constant_tag >;

    // operation has to be named with prefix 'op_'
    struct operation_tag;
    using operation_t = gap::strong_type< name_t, operation_tag >;

    // place has to be named with prefix '?'
    struct placeholder_tag;
    using place_t = gap::strong_type< name_t, placeholder_tag >;

    using atom_base = std::variant< constant_t, operation_t, place_t, label_t >;

    struct atom_t : atom_base {
        using base = atom_base;
        using base::base;

        atom_t(const base& atom, bitwidth_t bw)
            : base(std::move(atom))
            , _bitwidth(bw) {}

        atom_t(base&& atom, bitwidth_t bw)
            : base(std::move(atom))
            , _bitwidth(bw) {}

        std::optional< bitwidth_t > bitwidth() const {
            if (auto con = std::get_if< constant_t >(this)) {
                return con->ref().bits;
            }

            return _bitwidth;
        }
    private:
        std::optional< bitwidth_t > _bitwidth;
    };

    static inline std::string atom_name(const atom_t &atom) {
        const atom_base& base = atom;
        return std::visit( gap::overloaded{
            [&](const constant_t& c)  { return c.ref().to_string(2); },
            [&](const operation_t& o) { return o.ref(); },
            [&](const place_t& p)     { return p.ref(); },
            [&](const label_t& l)     { return label_name(l); },
        }, base);
    }

    template< typename stream >
    stream& operator<<(stream& os, const atom_t& atom) {
        const atom_base& base = atom;
        std::visit( gap::overloaded{
            [&](const constant_t& c) { os << c; },
            [&](const operation_t& o) { os << o; },
            [&](const place_t& p) { os << p; },
            [&](const label_t& l) { os << l; },
        }, base);

        if (auto bw = atom.bitwidth()) {
            os << ":" << std::to_string(bw.value());
        }
        return os;
    }

    static inline std::string to_string(const atom_t &atom) {
        std::stringstream ss;
        ss << atom;
        return ss.str();
    }

    // match pattern:
    //
    // match-labels ::= basic | commutative
    //
    // expr  ::= atom | list< expr >
    //
    // named ::= (let label expr):context
    //
    // action  ::= expr | match-labels
    //
    // pattern ::= list< named > list< constaint > action

    // expression of form: (match [labels])
    // serves to match multiple labeled expressions
    struct match_base {
        std::vector< label_t > labels;
    };

    struct basic_match_expr : match_base {};
    struct commutative_match_expr : match_base {};

    using match_expr = std::variant< basic_match_expr, commutative_match_expr >;

    static inline const auto& labels(const match_expr& e) {
        return std::visit([](const auto& m) -> const auto& { return m.labels; }, e);
    }

    static inline std::string kind_name(const match_expr& expr) {
        return std::visit( gap::overloaded {
            [](const basic_match_expr&) { return "match"; },
            [](const commutative_match_expr&) { return "commutative-match"; }
        }, expr);
    }

    template< typename stream >
    stream& operator<<(stream& os, const match_expr& expr) {
        os << "( " << kind_name(expr) << " ";
        for (const auto& label : labels(expr)) {
            os << label << ' ';
        }
        os << ')';
        return os;
    }

    // expression is either atom or compound expression
    // without labeled parts and contexts
    struct simple_expr;
    using expr_list = std::vector< simple_expr >;
    using simple_expr_base = std::variant< atom_t, expr_list >;

    struct simple_expr : simple_expr_base {
        using variant::variant;
    };

    static inline bool is_nested_list(const expr_list &list) {
        return std::all_of(std::begin(list), std::end(list), [] (const auto &e) {
            return std::holds_alternative< expr_list >(e);
        });
    }

    template< typename stream >
    stream& operator<<(stream& os, const expr_list& list) {
        os << "( ";
        for (const auto& element : list) {
            os << element << ' ';
        }
        os << ')';
        return os;
    }

    template< typename stream >
    stream& operator<<(stream& os, const simple_expr& expr) {
        return std::visit([&](const auto& e) -> stream& { return os << e; }, expr);
    }

    // expression of form: (expr):ctx;
    struct expr_with_context {
        explicit expr_with_context(simple_expr expr)
            : expr(expr) {}
        expr_with_context(simple_expr expr, context_t context)
            : expr(expr)
            , context(context) {}

        simple_expr expr;
        std::optional< context_t > context = std::nullopt;
    };

    template< typename stream >
    stream& operator<<(stream& os, const expr_with_context& e) {
        os << e.expr << " : ";
        if (e.context) {
            return os << e.context.value();
        }
        return os << "no-context";
    }

    // expression of form: (let name (expr):ctx);
    struct named_expr {
        named_expr(label_t name, expr_with_context expr)
            : name(name)
            , expr(expr) {}

        label_t name;
        expr_with_context expr;
    };

    template< typename stream >
    stream& operator<<(stream& os, const named_expr& e) {
        return os << e.name << " : " << e.expr;
    }

    using named_exprs = std::vector< named_expr >;

    template< typename stream >
    stream& operator<<(stream& os, const named_exprs& e) {
        os << "( ";
        for (const auto& v : e)
            os << v << ' ';
        os << ')';
        return os;
    }

    // Action is the last part of a match pattern that specifies
    // how to match the pattern.
    //
    // It is either generic expression or match expression.
    using match_action = std::variant< simple_expr, match_expr >;

    template< typename stream >
    stream& operator<<(stream& os, const match_action& action) {
        return std::visit([&](const auto& a) -> stream& { return os << a; }, action);
    }

    //
    // constaint ::= disjoint | equiv
    //

    // disjoint_expr ::= (disjoint list< context >)
    // serves to specify disjoint contexts
    struct disjoint_expr {
        std::vector< context_t > contexts;
    };

    // expression of form: (equiv list< place >)
    // serves to specify semantically equivalend places
    struct equiv_expr {
        std::vector< place_t > places;
    };

    using constraint_t = std::variant< disjoint_expr, equiv_expr >;

    using constraints_t = std::vector< constraint_t >;

    struct match_pattern {
        match_pattern(named_exprs list, constraints_t constraints, match_action action)
            : action(action)
            , constraints(constraints)
            , list(list) {}

        explicit match_pattern(match_action action) : action(action) {}

        match_action action;
        constraints_t constraints;
        named_exprs list;
    };

    template< typename pattern_type >
    named_expr get_expr_with_name(const label_t label, const pattern_type &pat) {
        auto it = std::find_if(std::begin(pat.list), std::end(pat.list), [&] (const named_expr &e) {
            return e.name == label;
        });

        if (it == std::end(pat.list)) {
            spdlog::error("label {} does not have expression definition", to_string(label));
        }

        return *it;
    }

    template< typename stream >
    stream& operator<<(stream& os, const match_pattern& m) {
        return os << m.action;
    }

    // apply pattern:
    //
    // action ::= union | bond | simple
    //
    // pattern ::= list< named > action

    // expression of form: (union [labels])
    // serves to unify matched labels on the right hand side of the rule
    struct union_expr {
        std::vector< label_t > labels;
    };

    static inline const auto& labels(const union_expr& e) { return e.labels; }

    template< typename stream >
    stream& operator<<(stream& os, const union_expr& action) {
        os << "(union ";
        for (const auto& label : action.labels) {
            os << label << ' ';
        }
        os << ')';
        return os;
    }

    // expression of form: (bond [labels])
    // serves to specify nodes to be bonded
    struct bond_expr {
        std::vector< label_t > labels;
    };

    static inline const auto& labels(const bond_expr& e) { return e.labels; }

    template< typename stream >
    stream& operator<<(stream& os, const bond_expr& action) {
        os << "(bond ";
        for (const auto& label : action.labels) {
            os << label << ' ';
        }
        os << ')';
        return os;
    }

    // Action is the last part of an apply pattern that specifies
    // how to apply the pattern.
    //
    // It is either simple expression or match expression.
    using apply_action = std::variant< union_expr, bond_expr, simple_expr >;

    template< typename stream >
    stream& operator<<(stream& os, const apply_action& action) {
        return std::visit([&](const auto& a) -> stream& { return os << a; }, action);
    }

    struct apply_pattern {
        explicit apply_pattern(apply_action action)
            : action(action) {}

        apply_pattern(named_exprs list, apply_action action)
            : action(action)
            , list(list) {}

        apply_action action;
        named_exprs list;
    };

    template< typename stream >
    stream& operator<<(stream& os, const apply_pattern& m) {
        return os << m.action;
    }

    std::optional< atom_t > parse_atom(std::string_view str);

    std::optional< constant_t > parse_constant(std::string_view str);

    std::optional< simple_expr > parse_simple_expr(std::string_view str);

    std::optional< named_expr > parse_named_expr(std::string_view str);

    std::optional< match_expr > parse_match_expr(std::string_view str);

    std::optional< constraint_t > parse_constraint(std::string_view str);

    std::optional< apply_action > parse_apply_action(std::string_view str);

    std::optional< match_pattern > parse_match_pattern(std::string_view str);

    std::optional< apply_pattern > parse_apply_pattern(std::string_view str);

    const atom_t& root(const simple_expr& expr);
    const atom_t& root(const named_expr& expr);
    const atom_t& root(const expr_with_context& expr);

    const atom_t& root(const match_action& expr);

    using expr_list = std::vector< simple_expr >;

    expr_list children(const simple_expr& expr);
    expr_list children(const named_expr& expr);
    expr_list children(const match_action& expr);
    expr_list children(const expr_with_context& expr);

    using places_t = std::vector< place_t >;

    static inline auto place_index(const place_t &place, const places_t &places) {
        return std::distance(places.begin(), std::find(places.begin(), places.end(), place));
    }

    using places_generator = gap::recursive_generator< place_t >;

    places_generator places(const label_t &label, const match_pattern &pattern, auto &filter) {
        co_yield places(get_expr_with_name(label, pattern), pattern, filter);
    }

    places_generator places(const atom_t &atom, const match_pattern &pattern, auto &filter) {
        const atom_base &base = atom;
        co_yield std::visit( gap::overloaded {
            [&](const place_t& p) -> places_generator {
                if (!filter(p)) {
                    co_yield place_t(p);
                }
            },
            [&] (const label_t& l) -> places_generator {
                co_yield places(l, pattern, filter);
            },
            [&] (const auto &) -> places_generator { co_return; /* noop */ }
        }, base);
    }

    places_generator places(const simple_expr &expr, const match_pattern &pattern, auto &filter) {
        const simple_expr_base &base = expr;
        co_yield std::visit( gap::overloaded {
            [&] (const atom_t &a) -> places_generator {
                co_yield places(a, pattern, filter);
            },
            [&] (const expr_list &list) -> places_generator {
                for (const auto &elem: list) {
                    co_yield places(elem, pattern, filter);
                }
            }
        }, base);
    }

    places_generator places(const expr_with_context &expr, const match_pattern &pattern, auto &filter) {
        co_yield places(expr.expr, pattern, filter);
    }

    places_generator places(const named_expr &expr, const match_pattern &pattern, auto &filter) {
        co_yield places(expr.expr, pattern, filter);
    }

    places_generator places(const match_expr &expr, const match_pattern &pattern, auto &filter) {
        for (const auto &label : labels(expr)) {
            co_yield places(label, pattern, filter);
        }
    }

    places_generator places(const match_action &action, const match_pattern &pattern, auto &filter) {
        co_yield std::visit([&] (const auto &a) -> places_generator {
            co_yield places(a, pattern, filter);
        }, action);
    }

    places_generator places(const match_pattern &pat, auto &filter) {
        co_yield places(pat.action, pat, filter);
    }

    places_t gather_places(const match_pattern &expr);

} // namespace eqsat
