/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <eqsat/pattern/pattern.hpp>
#include <gap/core/overloads.hpp>
#include <iostream>
#include <spdlog/spdlog.h>

namespace eqsat
{

    // TODO: pattern interpreter
    template< typename P, typename T >
    concept parser = gap::parser::parser< P, T >;

    using parse_input_t = gap::parser::parse_input_t;

    template< typename T >
    using parse_result_t = gap::parser::parse_result_t< T >;

    using gap::parser::parser_function;
    using gap::parser::parse_type;

    using gap::parser::rest;
    using gap::parser::result;

    using gap::parser::construct;
    using gap::parser::from_tuple;

    using gap::parser::char_parser;
    using gap::parser::bigint_parser;
    using gap::parser::length_parser;
    using gap::parser::number_parser;
    using gap::parser::string_parser;
    using gap::parser::to_vector_parser;

    using gap::parser::separated;
    using gap::parser::skip;

    template< typename T >
    constexpr parser< T > auto report(std::string_view msg) {
        return [=](parse_input_t in) -> parse_result_t< T > {
            spdlog::debug("can't parse {} from {}", msg, in);
            return std::nullopt;
        };
    }

    template< typename P, typename T = parse_type< P > >
    constexpr parser< T > auto paren(P &&p) {
        auto left = (char_parser('(') | report< char >("`(`"));
        auto right = (char_parser(')') | report< char >("`)`"));
        return  left < p > right;
    }

    parser< constant_t > auto constant_parser() {
        using value_t = constant_t::underlying_t;
        auto value    = [](value_t v) { return constant_t(v); };
        return fmap(value, bigint_parser());
    }

    constexpr parser< name_t > auto name_parser() {
        return [](parse_input_t in) -> parse_result_t< name_t > {
            if (auto prefix = length_parser(isalpha)(in); prefix && result(prefix) > 0) {
                if (auto suffix = length_parser(isdigit)(rest(prefix))) {
                    auto length = result(prefix) + result(suffix);
                    return {
                        {std::string(in.substr(0, length)), in.substr(length)}
                    };
                }
            }

            return std::nullopt;
        };
    }

    constexpr parser< unary_label > auto unary_label_parser() {
        return construct< unary_label >(name_parser());
    }

    constexpr parser< variadic_label > auto variadic_label_parser() {
        return construct< variadic_label >(name_parser() > string_parser("..."));
    }

    constexpr parser< label_t > auto label_name_parser() {
        auto single   = construct< label_t >(unary_label_parser());
        auto variadic = construct< label_t >(variadic_label_parser());
        return variadic | single;
    }

    constexpr parser< operation_t > auto operation_parser() {
        return construct< operation_t >(string_parser("op_") < name_parser());
    }

    constexpr parser< place_t > auto place_parser() {
        return construct< place_t >(char_parser('?') < name_parser());
    }

    constexpr parser< label_t > auto label_reference_parser() {
        return char_parser('$') < (label_name_parser() | report< label_t >("label reference"));
    }

    constexpr parser< single_context > auto single_context_parser() {
        return construct< single_context >(name_parser());
    }

    constexpr parser< variadic_context > auto variadic_context_parser() {
        return construct< variadic_context >(name_parser() > string_parser("..."));
    }

    constexpr parser< context_t > auto context_parser() {
        auto single   = construct< context_t >(single_context_parser());
        auto variadic = construct< context_t >(variadic_context_parser());
        return variadic | single;
    }

    parser< std::vector< context_t > > auto contexts_parser() {
        return to_vector_parser(context_parser());
    }

    parser< atom_t > auto atom_parser() {
        auto con = construct< atom_t >(constant_parser());
        auto op  = construct< atom_t >(operation_parser());
        auto plc = construct< atom_t >(place_parser());
        auto lab = construct< atom_t >(label_reference_parser());

        auto bw = string_parser(":") < number_parser< bitwidth_t >();

        auto atom = con | op | plc | lab;
        return from_tuple< atom_t >(atom & bw) | atom;
    }

    simple_expr wrap(simple_expr e) {
        if (std::holds_alternative< atom_t >(e)) {
            std::vector< simple_expr > vec{};
            vec.push_back(std::move(e));
            return { vec };
        }
        return e;
    }

    struct expr_parser_impl {
        using expr_parser_result = parse_result_t< simple_expr >;
        using expr_parser_t      = auto (*)(parse_input_t) -> expr_parser_result;

        static auto expr_list_parser() {
            auto push = [](simple_expr a, simple_expr b) -> simple_expr {
                auto vec = std::get< expr_list >(wrap(a));
                vec.push_back(b);
                return { vec };
            };

            return parenthesized(
                separated(element_parser(), skip(isspace), simple_expr{ expr_list() }, push));
        }

        static auto element_parser() -> expr_parser_t {
            return [](parse_input_t in) -> expr_parser_result {
                auto atom = construct< simple_expr >(atom_parser());
                auto list = construct< simple_expr >(expr_list_parser());
                return (atom | list)(in);
            };
        }
    };

    parser< simple_expr > auto expr_parser() { return expr_parser_impl::expr_list_parser(); }

    parser< std::vector< label_t > > auto labels_parser() {
        return to_vector_parser(label_reference_parser());
    }

    template< typename Expr >
    parser< Expr > auto match_label_list(std::string_view prefix) {
        auto match_prefix = (string_parser(prefix) & skip(isspace));
        return construct< Expr >(parenthesized(match_prefix < labels_parser()));
    }

    parser< basic_match_expr > auto basic_match_expr_parser() {
        return match_label_list< basic_match_expr >("match");
    }

    parser< commutative_match_expr > auto commutative_match_expr_parser() {
        return match_label_list< commutative_match_expr >("commutative-match");
    }

    parser< match_expr > auto match_expr_parser() {
        auto basic       = construct< match_expr >(basic_match_expr_parser());
        auto commutative = construct< match_expr >(commutative_match_expr_parser());
        return basic | commutative;
    }

    parser< union_expr > auto union_expr_parser() {
        return match_label_list< union_expr >("union");
    }

    parser< bond_expr > auto bond_expr_parser() { return match_label_list< bond_expr >("bond"); }

    parser< std::vector< place_t > > auto places_parser() {
        return to_vector_parser(place_parser());
    }

    parser< expr_with_context > auto expr_with_context_parser() {
        auto expr = expr_parser();
        auto ctx  = char_parser(':') < context_parser();
        return from_tuple< expr_with_context >(expr & ctx) | construct< expr_with_context >(expr);
    }

    parser< named_expr > auto named_expr_parser() {
        auto let = string_parser("let")
            | report< std::string_view >("let");

        auto name = ((let & skip(isspace)) < label_name_parser())
            | report< label_t >("expr name");

        auto expr = (skip(isspace) < expr_with_context_parser())
            | report< expr_with_context >("expr");

        return from_tuple< named_expr >(paren(name & expr))
            | report< named_expr >("named expression");
    }

   static auto push_element = [] (auto &&container, auto &&element) {
        container.push_back(std::move(element));
        return std::move(container);
   };

    parser< named_exprs > auto named_exprs_parser() {
        return many((named_expr_parser() > skip(isspace)), named_exprs{}, push_element)
            | report< named_exprs >("named expression list");
    }

    parser< disjoint_expr > auto disjoint_expr_parser() {
        auto prefix = (string_parser("disjoint") & skip(isspace));
        return construct< disjoint_expr >(parenthesized(prefix < contexts_parser()));
    }

    parser< equiv_expr > auto equiv_expr_parser() {
        auto prefix = (string_parser("equiv") & skip(isspace));
        return construct< equiv_expr >(parenthesized(prefix < places_parser()));
    }

    parser< constraint_t > auto constraint_parser() {
        auto dis = construct< constraint_t >(disjoint_expr_parser());
        auto eqv = construct< constraint_t >(equiv_expr_parser());
        return dis | eqv;
    }

    parser< constraints_t > auto constraints_parser() {
        return to_vector_parser(constraint_parser() > skip(isspace));
    }

    parser< match_action > auto match_action_parser() {
        auto match = construct< match_action >(match_expr_parser());
        auto expr  = construct< match_action >(expr_parser());
        return expr | match | report< match_action >("match action");
    }

    parser< match_pattern > auto match_pattern_parser() {
        auto action = match_action_parser();
        auto cons   = option(constraints_t{}, constraints_parser());
        auto list   = option(named_exprs{}, named_exprs_parser());
        return from_tuple< match_pattern >(paren(combine(list, cons, action)))
            | construct< match_pattern >(match_action_parser())
            | report< match_pattern >("match pattern");
    }

    parser< apply_action > auto apply_action_parser() {
        auto simple = construct< apply_action >(expr_parser());
        auto unione = construct< apply_action >(union_expr_parser());
        auto bond   = construct< apply_action >(bond_expr_parser());
        return simple | unione | bond | report< apply_action >("apply action");
    }

    parser< apply_pattern > auto apply_pattern_parser() {
        auto action = apply_action_parser();
        auto list   = option(named_exprs{}, named_exprs_parser());
        return from_tuple< apply_pattern >(list & action)
            | report< apply_pattern >("apply pattern");
    }

    template< parser_function parser_t >
    auto make_parse(parser_t parser, std::string_view str)
        -> std::optional< parse_type< parser_t > >
    {
        if (auto value = parser(str)) {
            return result(value);
        }
        return std::nullopt;
    }

    std::optional< atom_t > parse_atom(std::string_view str) {
        return make_parse(atom_parser(), str);
    }

    std::optional< constant_t > parse_constant(std::string_view str) {
        return make_parse(constant_parser(), str);
    }

    std::optional< simple_expr > parse_simple_expr(std::string_view str) {
        return make_parse(expr_parser(), str);
    }

    std::optional< named_expr > parse_named_expr(std::string_view str) {
        return make_parse(named_expr_parser(), str);
    }

    std::optional< match_expr > parse_match_expr(std::string_view str) {
        return make_parse(match_expr_parser(), str);
    }

    std::optional< constraint_t > parse_constraint(std::string_view str) {
        return make_parse(constraint_parser(), str);
    }

    std::optional< apply_action > parse_apply_action(std::string_view str) {
        return make_parse(apply_action_parser(), str);
    }

    std::optional< match_pattern > parse_match_pattern(std::string_view str) {
        return make_parse(match_pattern_parser(), str);
    }

    std::optional< apply_pattern > parse_apply_pattern(std::string_view str) {
        return make_parse(apply_pattern_parser(), str);
    }

    const atom_t &root(const simple_expr &expr) {
        return std::visit( gap::overloaded {
            [] (const atom_t &atom) -> const atom_t& { return atom; },
            [] (const expr_list &list) -> const atom_t& { return std::get<atom_t>(list.front()); }
        }, expr);
    }

    const atom_t& root(const expr_with_context& expr) { return root(expr.expr); }
    const atom_t& root(const named_expr& expr) { return root(expr.expr); }
    const atom_t& root(const match_action& action) {
        return std::visit( gap::overloaded {
            [] (const simple_expr &e) -> const atom_t& { return root(e); },
            [] (const match_expr &) -> const atom_t& { __builtin_unreachable(); },
        }, action);
    }

    expr_list children(const simple_expr &expr) {
        return std::visit( gap::overloaded {
            [] (const atom_t &) -> expr_list { return {}; },
            [] (const expr_list &vec) -> expr_list {
                return { std::next(vec.begin()), vec.end() };
            }
        }, expr);
    }

    expr_list children(const expr_with_context &expr) { return children(expr.expr); }

    expr_list children(const named_expr &expr) { return children(expr.expr); }

    expr_list children(const match_action &action) {
        return std::visit( [] (const auto &a) { return children(a); }, action);
    }

} // namespace eqsat
