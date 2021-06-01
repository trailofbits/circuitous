/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <algorithm>
#include <cctype>
#include <vector>
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

    struct Error {};
    using Constant = Constant_;
    using Name = std::string;
    struct Symbol { std::string name; };
    struct Place { std::string name; };
    struct Op { std::string name; };

    template< typename T > using ListT = std::vector< T >;

    struct Value : std::variant< Constant, Symbol, Op, Name, Place, ListT< Value >, Error >
    {
      using base = std::variant< Constant, Symbol, Op, Name, Place, ListT< Value >, Error >;
      using base::base;

      using List = ListT< Value >;
    };

    using List = typename Value::List;

    using ParseResult = std::pair< Value, std::string_view >;

    static inline const ParseResult parse_error = { Error(), "" };

    static bool is_error(const Value &v) { return std::holds_alternative< Error >(v); }

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

    Value parse(std::string_view pattern) const
    {
      auto [val, str] = parse_pattern(pattern);
      if (ltrim(str).empty())
        return val;
      return Error();
    }

    std::string unwrap(const Value &value) const
    {
      return std::get<Name>(value);
    }

    ParseResult parse_pattern(std::string_view str) const
    {
      str = ltrim(str);
      if (str.empty())
        return parse_error;

      // parse argument list
      if (str[0] == '(')
        return parse_list(str.substr(1));
      // parse constant number
      if (std::isdigit(str[0]) || (is_sign(str[0]) && str.size() > 1 && std::isdigit(str[1])))
        return parse_constant(str);
      // parse operations
      if (is_operation(str))
        return parse_operation(str);
      // parse variables
      if (std::isalpha(str[0]))
        return parse_symbol(str);
      // parse pattern placeholders
      if (is_placeholder(str[0]))
        return parse_placeholder(str);

      return parse_error;
    }

    ParseResult parse_list(std::string_view str) const
    {
      str = ltrim(str);
      if (str.empty())
        return parse_error;
      if (str.starts_with(')'))
        return { List(), str.substr(1) };

      auto [head, rest_1] = parse_pattern(str);
      if (rest_1.empty() || (!std::isblank( rest_1[0] ) && rest_1[0] != ')') )
        return parse_error;
      auto [tail, rest_2] = parse_list(rest_1);

      if (is_error(head) || is_error(tail))
        return parse_error;

      auto list = std::get< List >(tail);
      list.insert(list.begin(), head);
      return { list, rest_2 };
    }

    ParseResult parse_constant(std::string_view str) const
    {
      Constant con;
      auto [rest, err] = std::from_chars(str.data(), str.data() + str.size(), con);
      if (err == std::errc()) {
        auto count = size_t(std::distance(str.data(), rest));
        return {con, str.substr(count)};
      }
      return parse_error;
    }

    ParseResult parse_name(std::string_view str) const
    {
      if (str.empty())
        return parse_error;
      if (!std::isalpha(str[0]))
        return parse_error;

      unsigned i = 0;
      for (; i < str.size(); ++i) {
        if (std::isblank(str[i]) || str[i] == ')')
          break;
        if (!std::isalnum(str[i]))
          return parse_error;
      }

      return { std::string(str, 0, i), str.substr(i) };
    }

    ParseResult parse_symbol(std::string_view str) const
    {
      auto [name, rest] = parse_name(str);
      if (is_error(name))
        return parse_error;
      return { Symbol{unwrap(name)}, rest };
    }

    ParseResult parse_operation(std::string_view str) const
    {
      str.remove_prefix(3); // remove 'op_' prefix
      auto [name, rest] = parse_name(str);
      if (is_error(name))
        return parse_error;
      return { Op{unwrap(name)}, rest };
    }

    ParseResult parse_placeholder(std::string_view str) const
    {
      str.remove_prefix(1); // remove '?' prefix
      auto [name, rest] = parse_name(str);
      if (is_error(name))
        return parse_error;
      return { Place{unwrap(name)}, rest };
    }
  };

  template < typename Graph >
  struct Pattern
  {
    using Parser = PatternParser<int>;
    using Value = Parser::Value;

    Pattern(std::string_view pattern)
      : value(parse(pattern))
    {
      assert(!Parser::is_error(value));
    }

    static Value parse(std::string_view pattern)
    {
      return Parser().parse(pattern);
    }

    Value value;
  };


} // namespace circuitous