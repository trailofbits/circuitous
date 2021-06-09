/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/StrongType.h>

#include <algorithm>
#include <cctype>
#include <cstring>
#include <memory>
#include <vector>
#include <optional>
#include <variant>
#include <string>
#include <charconv>
#include <string_view>
#include <iostream>

namespace circuitous::eqsat {

  struct ASTNode;
  using ASTNodePtr = std::unique_ptr< ASTNode >;

  struct ASTNode
  {
    struct constant_tag;
    using Constant = strong_type< int64_t, constant_tag >;

    struct placeholder_tag;
    using Place = strong_type< size_t, placeholder_tag >;

    struct name_tag;
    using Name = strong_type< std::string, name_tag >;

    struct op_tag;
    using Op = strong_type< std::string, op_tag >;

    using Places = std::vector< ASTNode::Place >;

    using Value = std::variant< Constant, Op, Name, Place >;

    ASTNode(const Value &value) : value(value) {}
    ASTNode(Value &&value) : value(std::move(value)) {}

    Value value;
    std::vector< ASTNodePtr > children;
  };

  template< typename Kind, typename ...Args >
  ASTNodePtr make_node( Args && ...args )
  {
    return std::make_unique< ASTNode >( Kind( std::forward<Args>(args)... ) );
  }

  struct PatternParser
  {
    using Constant = ASTNode::Constant;
    using Place    = ASTNode::Place;
    using Op       = ASTNode::Op;

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

    ASTNodePtr parse(std::string_view pattern)
    {
      _places.clear(); // reset places memoty on each parse

      auto [val, str] = parse_pattern(pattern);
      if (ltrim(str).empty())
        return std::move(val);
      return nullptr;
    }

    ParseResult parse_pattern(std::string_view str)
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

    ParseResult parse_list(std::string_view str)
    {
      str = ltrim(str);
      if (str.empty())
        return parse_error();
      if (str.starts_with(')'))
        return parse_error("empty list");

      auto [head, tail] = parse_pattern(str);
      if (tail.empty() || (!std::isblank(tail[0]) && tail[0] != ')') )
        return parse_error();

      if (!head)
        return parse_error();

      auto rest = tail;
      while (!rest.empty() && rest[0] != ')') {
        auto [child, next] = parse_pattern(rest);
        if (!child || next.empty())
          return parse_error();
        head->children.push_back(std::move(child));
        rest = next;
      }

      return { std::move(head), rest.substr(1) };
    }

    ParseResult parse_constant(std::string_view str) const
    {
      ASTNode::Constant con;
      auto [rest, err] = std::from_chars(str.data(), str.data() + str.size(), con.ref());
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
        auto [name, rest] = parsed_name.value();
        return { make_node< Op >(name), rest };
      }
      return parse_error();
    }

    ParseResult parse_placeholder(std::string_view str)
    {
      str.remove_prefix(1); // remove '?' prefix
      if (auto parsed_name = parse_name(str)) {
        auto [name, rest] = parsed_name.value();
        auto [it, _] = _places.try_emplace(name, _places.size());
        return { make_node< Place >(it->second), rest };
      }
      return parse_error();
    }

    std::size_t seen_places() const { return _places.size(); }

  private:
    std::unordered_map< std::string, ASTNode::Place > _places;
  };

  struct Pattern
  {
    using Parser = PatternParser;
    using Place  = ASTNode::Place;
    using Places = ASTNode::Places;

    Pattern(std::string_view pattern)
    {
      Parser parser;
      value = parser.parse(pattern);
      assert( value );
      places = parser.seen_places();
    }

    ASTNodePtr value;
    std::size_t places;
  };

} // namespace circuitous::eqsat
