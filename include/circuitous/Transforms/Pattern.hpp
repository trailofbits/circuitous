/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <algorithm>
#include <cctype>
#include <memory>
#include <vector>
#include <optional>
#include <variant>
#include <list>
#include <string>
#include <charconv>
#include <string_view>
#include <iostream>

namespace circuitous {

  template< typename Constant_ >
  struct PatternParser
  {

    using Constant = Constant_;
    using Name = std::string;
    struct Place
    {
      Place(std::string name) : name(std::move(name)) {}

      bool operator==(const Place &) const = default;

      std::string name;
    };

    struct Op
    {
      Op(std::string name) : name(std::move(name)) {}

      bool operator==(const Op &) const = default;

      std::string name;
    };

    using Value = std::variant< Constant, Op, Name, Place >;

    struct ASTNode;
    using ASTNodePtr = std::unique_ptr< ASTNode >;

    struct ASTNode
    {
      ASTNode(const Value &value) : value(value) {}
      ASTNode(Value &&value) : value(value) {}

      Value value;
      std::vector< ASTNodePtr > children;
    };

    using ParseResult = std::pair< ASTNodePtr, std::string_view >;

    ParseResult parse_error(std::string_view msg = "") const
    {
      return { nullptr, msg };
    }

    static bool is_sign(char c) { return std::strchr("-", c); }
    static bool is_placeholder(char c) { return c == '?'; }
    static bool is_operation(std::string_view str)
    {
      return str.starts_with("op_") || std::strchr("+-", str[0]);
    }

    static std::string_view ltrim(std::string_view str)
    {
      std::string_view res = str;
      res.remove_prefix(std::min(res.find_first_not_of(" "), res.size()));
      return res;
    }

    ASTNodePtr parse(std::string_view pattern) const
    {
      auto &&[val, str] = parse_pattern(pattern);
      if (ltrim(str).empty())
        return std::move(val);
      return nullptr;
    }

    template< typename Kind, typename ...Args >
    auto make_node( Args && ...args ) const
    {
      return std::make_unique< ASTNode >( Kind( std::forward<Args>(args)... ) );
    }

    ParseResult parse_pattern(std::string_view str) const
    {
      str = ltrim(str);
      if (str.empty())
        return parse_error();

      // parse argument list
      if (str[0] == '(')
        return parse_list(str.substr(1));
      // parse constant number
      if (std::isdigit(str[0]) || (is_sign(str[0]) && str.size() > 1 && std::isdigit(str[1])))
        return parse_constant(str);
      // parse operations
      if (is_operation(str))
        return parse_operation(str);
      // parse pattern placeholders (variables)
      if (is_placeholder(str[0]))
        return parse_placeholder(str);

      return parse_error();
    }

    ParseResult parse_list(std::string_view str) const
    {
      str = ltrim(str);
      if (str.empty())
        return parse_error();
      if (str.starts_with(')'))
        return parse_error("empty list");

      auto [head, tail] = parse_pattern(str);
      if (tail.empty() || (!std::isblank( tail[0] ) && tail[0] != ')') )
        return parse_error();

      if (!head)
        return parse_error();

      auto rest = tail;
      while (!rest.empty() && rest[0] != ')') {
        auto &&[child, next] = parse_pattern(rest);
        if (!child || next.empty())
          return parse_error();
        head->children.push_back(std::move(child));
        rest = next;
      }

      return { std::move(head), rest.substr(1) };
    }

    ParseResult parse_constant(std::string_view str) const
    {
      Constant con;
      auto [rest, err] = std::from_chars(str.data(), str.data() + str.size(), con);
      if (err == std::errc()) {
        auto count = size_t(std::distance(str.data(), rest));
        return { make_node< Constant >(con), str.substr(count)};
      }
      return parse_error();
    }

    using ParseNameResult = std::optional< std::pair< std::string, std::string_view > >;
    ParseNameResult parse_name(std::string_view str) const
    {
      if (str.empty())
        return std::nullopt;
      if (!std::isalpha(str[0]))
        return std::nullopt;

      unsigned i = 0;
      for (; i < str.size(); ++i) {
        if (std::isblank(str[i]) || str[i] == ')')
          break;
        if (!std::isalnum(str[i]))
          return std::nullopt;
      }

      return {{ std::string(str, 0, i), str.substr(i) }};
    }

    ParseResult parse_operation(std::string_view str) const
    {
      str.remove_prefix(3); // remove 'op_' prefix
      if (auto parsed_name = parse_name(str)) {
        auto &&[name, rest] = parsed_name.value();
        return { make_node< Op >(name), rest };
      }
      return parse_error();
    }

    ParseResult parse_placeholder(std::string_view str) const
    {
      str.remove_prefix(1); // remove '?' prefix
      if (auto parsed_name = parse_name(str)) {
        auto &&[name, rest] = parsed_name.value();
        return { make_node< Place >(name), rest };
      }
      return parse_error();
    }
  };

  template < typename Graph >
  struct Pattern
  {
    using Parser = PatternParser<int>;
    using ASTNodePtr = Parser::ASTNodePtr;
    using Value = Parser::Value;
    using Place = Parser::Place;
    using Constant = Parser::Constant;
    using Op = Parser::Op;

    Pattern(std::string_view pattern) : value(parse(pattern)) { assert(value); }

    static ASTNodePtr parse(std::string_view pattern)
    {
      return Parser().parse(pattern);
    }

    ASTNodePtr value;
  };

} // namespace circuitous
