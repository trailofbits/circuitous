#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Diff/Diff.hpp>

namespace circ::print
{
    static inline std::string make_style( std::string fill_color, std::string font_color )
    {
        return "fillcolor=" + fill_color + ";fontcolor=" + font_color + ";style=filled;";
    }

    enum class Color
    {
        None,
        RedWhite,
        YellowBlack,
        GreenBlack,
        BlueYellow,
        VioletWhite,
        GrayWhite,
        OrangeBlack
    };

    template<typename T>
    concept GraphColorer = requires( T v, Circuit *circuit ) {
        { v.color_circuit(circuit) } -> std::same_as< void >;
        { v.remove_coloring(circuit) } -> std::same_as< void >;
        { v.get_to_color() } -> std::same_as< std::function< Color( Operation * ) > >;
    };

    std::string color_to_dot( Color c );

    using color_default_styles_t = std::array< Color, 7 >;
    using color_styles_t = std::array< std::string_view, 7 >;

    static constexpr const color_default_styles_t color_defaults {
        Color::RedWhite,    Color::YellowBlack, Color::GreenBlack, Color::BlueYellow,
        Color::VioletWhite, Color::GrayWhite,   Color::OrangeBlack
    };

    Color sem_taint_coloring( Operation *op );
    Color diff_coloring( Operation *op );
    Color static inline no_coloring( Operation *op ) { return Color::None; }

    struct HighlightColorer
    {
        using names_t = std::vector< std::string >;
        using opt_name_t = std::optional< std::string_view >;

        HighlightColorer( const names_t &highlight_nodes );

        Color operator()( Operation *op );

        void color_circuit( Circuit *c ) { }
        std::function< Color( Operation * ) > get_to_color() { return *this; }
        void remove_coloring( Circuit *c ) { }

    private:
        names_t highlight_nodes;
        uint32_t color_counter = 0;
        std::unordered_map< std::string_view, uint32_t > node_to_color_map;

        opt_name_t name_for_op( Operation *op );
    };

    struct SemanticsColorer
    {
        void color_circuit( Circuit *circ )
        {
            run_visitor_on< leaf_values_ts >( circ, inspect::SemanticsTainterVisitor() );
        }

        std::function< Color( Operation * ) > get_to_color() { return sem_taint_coloring; }

        void remove_coloring( Circuit *circ )
        {
            run_visitor_on< leaf_values_ts >( circ, inspect::SemanticsTainterRemovalVisitor() );
        }
    };

    struct EmptyColorer
    {
        void color_circuit( Circuit *circ ) { }
        std::function< Color( Operation * ) > get_to_color() { return no_coloring; }
        void remove_coloring( Circuit *circ ) { }
    };

    template < inspect::SubPathCol CollectorT >
    struct DiffColorer
    {
        DiffColorer() { collector = CollectorT(); }

        void color_circuit( Circuit *circuit )
        {
            if ( circuit->attr< circ::VerifyInstruction >().size() != 2 )
                return;
            auto lhs = circuit->attr< circ::VerifyInstruction >()[ 0 ];
            auto rhs = circuit->attr< circ::VerifyInstruction >()[ 1 ];

            inspect::apply_on_subtree( collector( lhs ),
                                       []( Operation *op ) {
                                           inspect::mark_operation(
                                               op, inspect::DiffMarker::Left,
                                               inspect::DiffMarker::Right );
                                       } );

            inspect::apply_on_subtree( collector( rhs ),
                                       []( Operation *op ) {
                                           inspect::mark_operation( op,
                                                                    inspect::DiffMarker::Right,
                                                                    inspect::DiffMarker::Left );
                                       } );
            has_colored = true;
        }

        std::function< Color( Operation * ) > get_to_color() { return diff_coloring; }
        void remove_coloring( Circuit *circuit )
        {
            if ( !has_colored || circuit->attr< circ::VerifyInstruction >().size() != 2 )
                return;
            auto lhs = circuit->attr< circ::VerifyInstruction >()[ 0 ];
            auto rhs = circuit->attr< circ::VerifyInstruction >()[ 1 ];

            inspect::apply_on_subtree( collector( lhs ),
                                       []( Operation *op ) { inspect::clear_mark( op ); } );
            inspect::apply_on_subtree( collector( rhs ),
                                       []( Operation *op ) { inspect::clear_mark( op ); } );
        }

        CollectorT collector;
        bool has_colored = false;
    };

    template < typename ColorFunc >
    struct Printer : UniqueVisitor< Printer< ColorFunc > >
    {
        using value_map_t = std::unordered_map< Operation *, std::string >;

        template < typename CF >
        explicit Printer( std::ostream &os_, const value_map_t &vals, CF &&color_op ) :
            os( os_ ), node_values( vals ), color_op( std::forward< ColorFunc >( color_op ) )
        {
        }

        std::string operand( Operation *of, std::size_t i )
        {
            return node_id( of ) + ':' + node_id( of ) + std::to_string( i );
        }

        void edge( Operation *from, Operation *to, std::size_t i )
        {
            os << operand( from, i ) << " -> " << node_id( to ) << ";\n";
        }

        std::string node_id( Operation *op ) { return "v" + std::to_string( op->id() ) + "v"; }

        std::string as_id( const std::string &what ) { return "<" + what + ">"; }

        void node( Operation *op )
        {
            os << node_id( op ) << "[";
            os << color_to_dot( color_op( op ) );

            os << "label = \" { " << as_id( node_id( op ) ) << " " << op->name();
            if ( node_values.count( op ) )
                os << " " << node_values.find( op )->second << " ";

            if ( op->operands.size() == 0 )
            {
                os << " }" << '"' << "];\n";
                return;
            }

            os << "| {";
            for ( std::size_t i = 0; i < op->operands.size(); ++i )
            {
                os << as_id( node_id( op ) + std::to_string( i ) );
                if ( i != op->operands.size() - 1 )
                    os << " | ";
            }
            os << " }}" << '"' << "];\n";
        }

        void init()
        {
            os << "digraph {" << std::endl;
            os << "node [shape=record];";
        }

        void visit( Operation *op )
        {
            op->traverse( *this );
            node( op );
            for ( std::size_t i = 0; i < op->operands.size(); ++i )
                edge( op, op->operands[ i ], i );
        }

        void visit( Circuit *op )
        {
            init();

            op->traverse( *this );
            node( op );
            for ( std::size_t i = 0; i < op->operands.size(); ++i )
                edge( op, op->operands[ i ], i );

            os << "}";
        }

        std::ostream &os;
        const value_map_t &node_values;
        ColorFunc color_op;
    };

    void print_dot( std::ostream &os, Circuit *circuit, GraphColorer auto &&c, const std::unordered_map< Operation *, std::string > &values = {} )
    {
        c.color_circuit( circuit );
        Printer< std::function< print::Color( Operation * ) > > dot_os( os, values,
                                                                        c.get_to_color() );
        dot_os.visit( circuit );
        c.remove_coloring( circuit );
    }
}
