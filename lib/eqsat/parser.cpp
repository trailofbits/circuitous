/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#include <eqsat/pattern/parser.hpp>

#include <spdlog/spdlog.h>

#include <fstream>
#include <optional>

namespace eqsat
{
    using maybe_rule_set      = std::optional< rule_set >;
    using maybe_rule_set_name = std::optional< std::string_view >;

    using maybe_rewrite_rule  = std::optional< rewrite_rule >;

    std::string_view ltrim(std::string_view line) {
        line.remove_prefix(std::min(line.find_first_not_of(" \n\r\t"), line.size()));
        return line;
    }

    bool is_commented(std::string_view line) { return ltrim(line).starts_with('#'); }

    std::optional< std::string > get_nonempty_line(std::istream &is) {
        std::string line;
        while (std::getline(is, line)) {
            if (line.empty()) {
                /* noop */
            } else if (is_commented(line)) {
                /* noop */
            } else {
                return line;
            }
        }

        return std::nullopt;
    }


    maybe_rule_set_name parse_ruleset_name(std::string_view line) {
        if (!line.starts_with('[')) {
            return std::nullopt;
        }
        if (!line.ends_with(']')) {
            spdlog::error("[eqsat] missing closing bracket: {}", line);
            return std::nullopt;
        }
        return line.substr(1, line.size() - 2);
    }


    maybe_rule_set maybe_new_ruleset(std::string_view line) {
        if (auto name = parse_ruleset_name(line))
            return rule_set{ std::string{ name.value() }, rewrite_rules{} };
        return std::nullopt;
    }

    std::optional< std::string > parse_rule_name(std::string_view line) {
        if (line.ends_with(':'))
            return std::string(line.substr(0, line.size() - 1));
        return std::nullopt;
    }

    std::optional< std::string > parse_pattern(std::string_view line) {
        line = ltrim(line);
        if (line.starts_with('-'))
            return std::string(line.substr(2));
        return std::nullopt;
    }

    maybe_rewrite_rule parse_rule(std::string_view name_line, std::istream &is) {
        auto pattern = [&]() -> std::optional< std::string > {
            if (auto line = get_nonempty_line(is)) {
                if (auto pat = parse_pattern(*line)) {
                    return pat;
                } else {
                    spdlog::error("[eqsat] expected a pattern: {}", *line);
                    return std::nullopt;
                }
            }

            spdlog::error("[eqsat] missing pattern");
            return std::nullopt;
        };

        if (auto name = parse_rule_name(name_line)) {
            spdlog::debug("[eqsat] rule: {}", *name);
            auto lhs = pattern();
            auto rhs = pattern();
            if (lhs && rhs) {
                spdlog::debug("[eqsat] lhs: {}", *lhs);
                spdlog::debug("[eqsat] rhs: {}", *rhs);
                return rewrite_rule{ *name, *lhs, *rhs };
            }
        } else {
            spdlog::error("[eqsat] expected rule name: {}", name_line);
            return std::nullopt;
        }

        return std::nullopt;
    }

    std::vector< rule_set > parse_rules(const std::string &filename) {
        spdlog::debug("[eqsat] parse rules from: {}", filename);
        std::ifstream file(filename, std::ios::in);
        return parse_rules(file);
    }

    std::vector< rule_set > parse_rules(std::istream &is) {
        std::vector< rule_set > rulesets;

        auto add_to_current_ruleset = [&](auto &&rule) {
            rulesets.back().rules.push_back(std::move(rule));
        };

        while (auto line = get_nonempty_line(is)) {
            if (auto ruleset = maybe_new_ruleset(*line)) {
                spdlog::debug("[eqsat] new set of rules: {}", ruleset->name);
                rulesets.push_back(std::move(*ruleset));
            } else if (auto rule = parse_rule(*line, is)) {
                add_to_current_ruleset(std::move(*rule));
            } else {
                spdlog::error("[eqsat] syntax error: {}", *line);
            }
        }

        return rulesets;
    }

} // namespace eqsat
