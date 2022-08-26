/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/egraph.hpp>

#include <spdlog/spdlog.h>

#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/os.h>

namespace eqsat
{
    namespace detail {

        //
        // graph formatter
        //
        template< typename node_format, typename edge_format >
        struct graph_printer {
            graph_printer(const std::string &path, node_format &format_node, edge_format &format_edge)
                : out(fmt::output_file(path))
                , format_node(format_node)
                , format_edge(format_edge)
            {
                out.print(R"(
                    digraph egraph {{
                        compound=true
                        clusterrank=local
                )");
            }

            ~graph_printer() { out.print("}}"); }

            void print(const gap::graph::node_like auto &node) {
                format_node(out, node);
            }

            void print(const gap::graph::edge_like auto &edge) {
                format_edge(out, edge);
            }

            template< typename... args_t >
            void print(std::string_view fmt, args_t &&... args) {
                out.print(fmt, std::forward< args_t >(args)...);
            }

            fmt::ostream out;

            node_format &format_node;
            edge_format &format_edge;
        };


        //
        // eclass formatter
        //
        template< typename printer, typename eclass >
        struct print_eclass {
            print_eclass(printer &out, const eclass &pair)
                : out(out)
            {
                const auto &[handle, cls] = pair;
                out.print(R"(subgraph cluster_{} {{
                    style=dotted
                )", handle.id.ref());

                for (auto node : cls.nodes) {
                    out.print(*node);
                }
            }

            ~print_eclass() { out.print("}}\n"); }

            printer &out;
        };

    } // namespace detail

    void to_dot(const auto &egraph, const std::string &path) {
        spdlog::info("[eqsat] printing to dot {}", path);

        auto node_identifier = [&] (const gap::graph::node_like auto &node) {
            return fmt::format("{}.{}"
                , egraph.find(&node).id.ref()
                , reinterpret_cast< std::uintptr_t >(&node)
            );
        };

        auto class_identifier = [&] (const auto &eclass) {
            return node_identifier(*eclass->nodes.front());
        };

        auto format_node = [&] (fmt::ostream &out, const gap::graph::node_like auto &node) {
            out.print("{} [label =\"{}\" ]\n"
                , node_identifier(node)
                , node_name(node)
            );

            // TODO: print bitwidth
        };

        auto format_edge = [&] (fmt::ostream &out, const gap::graph::edge_like auto &edge) {
            out.print("{} -> {}\n"
                , node_identifier(*edge.src)
                , class_identifier(edge.tgt)
            );
        };

        detail::graph_printer out(path, format_node, format_edge);

        for (const auto &cls : egraph.eclasses()) {
            detail::print_eclass(out, cls);
        }

        for (const auto &edge : egraph.edges()) {
            out.print(edge);
        }
    }

} // namespace eqsat
