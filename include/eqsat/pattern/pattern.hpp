/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <gap/core/overloads.hpp>
#include <gap/core/parser.hpp>
#include <gap/core/strong_type.hpp>
#include <optional>
#include <unordered_set>
#include <variant>
#include <vector>

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
    stream& operator<<(stream& os, const label_t& label) {
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

    //
    // atom ::= constant | operation | place | label
    //
    struct constant_tag;
    using constant_t = gap::strong_type< std::int64_t, constant_tag >;

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
            , bitwidth(bw) {}

        atom_t(base&& atom, bitwidth_t bw)
            : base(std::move(atom))
            , bitwidth(bw) {}

        std::optional< bitwidth_t > bitwidth;
    };

    template< typename stream >
    stream& operator<<(stream& os, const atom_t& atom) {
        std::visit( gap::overloaded{
            [&](const constant_t& c) { os << c; },
            [&](const operation_t& o) { os << o; },
            [&](const place_t& p) { os << p; },
            [&](const label_t& l) { os << l; },
        }, atom);
        return os;
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
        return std::visit(
            [](const auto& m) -> const auto& { return m.labels; }, e);
    }

    // expression is either atom or compound expression
    // without labeled parts and contexts
    struct simple_expr;
    using expr_list = std::vector< simple_expr >;

    struct simple_expr : std::variant< atom_t, expr_list > {
        using variant::variant;
    };

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
        std::visit([&](const auto& e) { os << e; }, expr);
        return os;
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

    // expression of form: (let name (expr):ctx);
    struct named_expr {
        named_expr(label_t name, expr_with_context expr)
            : name(name)
            , expr(expr) {}

        label_t name;
        expr_with_context expr;
    };

    using named_exprs = std::vector< named_expr >;

    // Action is the last part of a match pattern that specifies
    // how to match the pattern.
    //
    // It is either generic expression or match expression.
    using match_action = std::variant< simple_expr, match_expr >;

    // template< typename stream >
    // stream& operator<<(stream& os, const named_exprs& e) {
    //     os << "( ";
    //     for (const auto& v : e)
    //         os << v << ' ';
    //     os << ')';
    //     return os;
    // }

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

        match_action action;
        constraints_t constraints;
        named_exprs list;
    };

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

    // expression of form: (bond [labels])
    // serves to specify nodes to be bonded
    struct bond_expr {
        std::vector< label_t > labels;
    };

    static inline const auto& labels(const bond_expr& e) { return e.labels; }

    // Action is the last part of an apply pattern that specifies
    // how to apply the pattern.
    //
    // It is either simple expression or match expression.
    using apply_action = std::variant< union_expr, bond_expr, simple_expr >;

    struct apply_pattern {
        apply_pattern(named_exprs list, apply_action action)
            : action(action)
            , list(list) {}

        apply_action action;
        named_exprs list;
    };

    struct rule_t {
        rule_t(match_pattern lhs, apply_pattern rhs)
            : lhs(lhs)
            , rhs(rhs) {}

        match_pattern lhs;
        apply_pattern rhs;
    };

    std::optional< atom_t > parse_atom(std::string_view str);

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

    places_t gather_places(const simple_expr &expr);

} // namespace eqsat
