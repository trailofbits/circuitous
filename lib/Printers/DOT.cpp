/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Printers/Dot.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

#include <ostream>
#include <unordered_map>


CIRCUITOUS_RELAX_WARNINGS
#include <circuitous/Diff/SemanticsTainter.hpp>

CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::print
{
    std::string color_to_dot( Color c )
    {
        switch ( c )
        {
            case Color::None: return "";
            case Color::RedWhite: return make_style( "red", "white" );
            case Color::YellowBlack: return make_style( "yellow", "black" );
            case Color::GreenBlack: return make_style( "green", "black" );
            case Color::BlueYellow: return make_style( "blue", "yellow" );
            case Color::VioletWhite: return make_style( "violet", "white" );
            case Color::GrayWhite: return make_style( "gray", "white" );
            case Color::OrangeBlack: return make_style( "orange", "black" );
        }
    }

    HighlightColorer::opt_name_t HighlightColorer::name_for_op(Operation *op) {
        auto is_prefix_to_op_name = [&](const std::string &lhs) {
            return std::equal( lhs.begin(), lhs.end(), op->name().begin(),
                               [](char a, char b) {
                                   return std::tolower( a ) == std::tolower( b );
                               } );
        };
        auto highlight = std::find_if( highlight_nodes.begin(), highlight_nodes.end(),
                                       is_prefix_to_op_name );
        if ( highlight != highlight_nodes.end()) {
            return opt_name_t { *highlight};
        }
        return std::nullopt;
    }

    HighlightColorer::HighlightColorer(const std::vector< std::string > &highlight_nodes)
            : highlight_nodes( highlight_nodes) {
        for(auto& hl: highlight_nodes){
            node_to_color_map[hl] = color_counter % color_defaults.size();
            color_counter++;
        }
    }

    Color HighlightColorer::operator()(Operation *op) {
        if(auto opt_highlight_name = name_for_op( op ))
            return color_defaults[node_to_color_map[opt_highlight_name.value()]];

        return Color::None;
    }

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


    Color sem_taint_coloring(Operation *op) {
        using namespace inspect;
        if ( !op->has_meta( SemanticsTainterVisitor::meta_key ))
            return Color::None;

        switch (read_semantics(op)) {
            case sem_taint::None: return Color::None;
            case sem_taint::State: return Color::GreenBlack;
            case sem_taint::Decode: return  Color::BlueYellow;
            case sem_taint::Semantics: return Color::OrangeBlack;
            case sem_taint::Config: return Color::RedWhite;
            case sem_taint::Delete: return Color::GrayWhite;
        }
    }

    Color diff_coloring(Operation *op) {
        using namespace inspect;

        if ( !op->has_meta( meta_key ))
            return Color::None;

        auto value = diffmarker_read(op);
        if(value == DiffMarker::Overlapping){
            return Color::GreenBlack;
        } else if(value == DiffMarker::Left){
            return Color::BlueYellow;
        }
        else if(value == DiffMarker::Right){
            return Color::RedWhite;
        }
        else if(value == DiffMarker::None){
            return Color::None;
        }

        circ::unreachable() << "could not read diff_marker properly";
    }
} // namespace circ
