/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

#include <ostream>
#include <unordered_map>


CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    namespace
    {

        static const char *const kBeginDOTNode =
            "[label=<<TABLE cellpadding=\"0\" cellspacing=\"0\" border=\"1\"><TR>";
        static const char *const kEndDOTNode = "</TR></TABLE>>];\n";

        class DOTPrinter : public UniqueVisitor<DOTPrinter> {
            using value_map_t = std::unordered_map<Operation *, std::string>;
            public:
            explicit DOTPrinter(std::ostream &os_, const value_map_t &vals)
                : os(os_), node_values(vals) {}

            void PrintOperands(Operation *op) {
                if (!op->operands.empty()) {
                    os << "</TR><TR>";
                    for (auto sub_op : op->operands) {
                        os << "<TD port=\"s";
                        os << sub_op->id();
                        os << "\"> &nbsp; </TD>";
                    }
                }
                os << kEndDOTNode;
                for (auto sub_op : op->operands) {
                    os << 'o' << op->id() << ":s" << sub_op->id()
                       << " -> o" << sub_op->id() << ":id;\n";
                }
            }

            void PrintNodeName(Operation *op) {
                os << "o" << op->id() << " " << kBeginDOTNode << "<TD port=\"id\"";
                if (!op->operands.empty()) {
                    os << " colspan=\"" << op->operands.size() << "\"";
                }
                os << ">" << op->name();
                if (node_values.count(op)) {
                    os << " = " << node_values.find(op)->second;
                }
                os << "</TD>";
            }

            void visit(Operation *op) {
                op->traverse(*this);
                PrintNodeName(op);
                PrintOperands(op);
            }

            void visit(Circuit *op) {
                os << "digraph {\n"
                    << "node [shape=plain];\n";
                op->traverse(*this);
                PrintNodeName(op);
                PrintOperands(op);
                os << "}\n";
            }

            private:
            std::ostream &os;
            const value_map_t &node_values;
        };

    }  // namespace
} // namespace circ

namespace circ::dot
{
    struct Printer : UniqueVisitor<Printer> {
        using value_map_t = std::unordered_map<Operation *, std::string>;
        using highlight_names_t = std::vector< std::string >;
        using opt_highlight_name_t = std::optional<std::reference_wrapper<std::string>>;
        using color_styles_t = std::array< std::string_view, 5 >;


        explicit Printer(std::ostream &os_, const value_map_t &vals, const highlight_names_t &highlight_nodes)
            : os(os_), node_values(vals), highlight_nodes(highlight_nodes) {}

        std::string Operand(Operation *of, std::size_t i) {
            return NodeID(of) + ':' + NodeID(of) + std::to_string(i);
        }

        void Edge(Operation *from, Operation *to, std::size_t i) {
            os << Operand(from, i)
                << " -> "
                << NodeID(to)
                << ";\n";
        }

        std::string NodeID(Operation *op) {
            return "v" + std::to_string(op->id()) + "v";
        }

        std::string AsID(const std::string &what) {
            return "<" + what + ">";
        }

        void Node(Operation *op) {
            os << NodeID(op) << "[";
            auto opt_highlight_name = highlight_name_for_op( op );
            if(opt_highlight_name.has_value()) {
                if(!node_to_color_map.contains( opt_highlight_name->get())){
                    node_to_color_map[opt_highlight_name->get()] = color_counter % colors.size();
                    color_counter++;
                }
                os << colors[node_to_color_map[opt_highlight_name->get()]];
            }
            os << "label = \" { " << AsID(NodeID(op)) << " " << op->name();
            if (node_values.count(op)) {
                os << " " << node_values.find(op)->second << " ";
            }

            if (op->operands.size() == 0) {
                os << " }" << '"' << "];\n";
                return;
            }

            os << "| {";
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                os << AsID(NodeID(op) + std::to_string(i));
                if (i != op->operands.size() - 1) {
                    os << " | ";
                }
            }
            os << " }}" << '"' << "];\n";
        }

        void Init() {
            os << "digraph {" << std::endl;
            os << "node [shape=record];";
        }

        void visit(Operation *op) {
            op->traverse(*this);
            Node(op);
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                Edge(op, op->operands[i], i);
            }
        }

        void visit(Circuit *op) {
            Init();
            op->traverse(*this);
            Node(op);
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                Edge(op, op->operands[i], i);
            }
            os << "}";
        }

        std::ostream &os;
        const value_map_t &node_values;

       private:
        highlight_names_t highlight_nodes;
        uint32_t color_counter = 0;
        std::unordered_map< std::string_view, uint32_t > node_to_color_map;
        static constexpr const color_styles_t colors = color_styles_t{
                "fillcolor=red;fontcolor=white;style=filled;",
                "fillcolor=yellow;fontcolor=black;style=filled;",
                "fillcolor=green;fontcolor=black;style=filled;",
                "fillcolor=blue;fontcolor=yellow;style=filled;",
                "fillcolor=violet;fontcolor=white;style=filled;"
        };

        opt_highlight_name_t highlight_name_for_op(Operation *op) {
            auto is_prefix_to_op_name = [&](const std::string &lhs) {
                return std::equal( lhs.begin(), lhs.end(), op->name().begin(),
                                   [](char a, char b) {
                                       return std::tolower( a ) == std::tolower( b );
                                   } );
            };
            auto highlight = std::find_if( highlight_nodes.begin(), highlight_nodes.end(),
                                           is_prefix_to_op_name );
            if ( highlight != highlight_nodes.end()) {
                return opt_highlight_name_t{*highlight};
            }
            return std::nullopt;
        }
    };
} // namespace circ::dot

namespace circ
{
    void print_dot(std::ostream &os, Circuit *circuit,
                  const std::unordered_map<Operation *, std::string> &node_values, const std::vector< std::string > &highlights = std::vector< std::string >())
    {
      circ::dot::Printer dot_os(os, node_values, highlights);
      dot_os.visit(circuit);
    }
} // namespace circ
