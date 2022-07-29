#pragma once

#include <circuitous/IR/IR.hpp>

namespace circ::print
{
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

    using color_default_styles_t = std::array< Color, 7 >;
    using color_styles_t = std::array< std::string_view, 7 >;

    static constexpr const color_default_styles_t color_defaults
    {
        Color::RedWhite,
        Color::YellowBlack,
        Color::GreenBlack,
        Color::BlueYellow,
        Color::VioletWhite,
        Color::GrayWhite,
        Color::OrangeBlack
    };

    static constexpr const color_styles_t colors
    {
        "fillcolor=red;fontcolor=white;style=filled;",
        "fillcolor=yellow;fontcolor=black;style=filled;",
        "fillcolor=green;fontcolor=black;style=filled;",
        "fillcolor=blue;fontcolor=yellow;style=filled;",
        "fillcolor=violet;fontcolor=white;style=filled;",
        "fillcolor=gray;fontcolor=white;style=filled;",
        "fillcolor=orange;fontcolor=black;style=filled;"
    };

    Color SemanticsTainterColoring(Operation *op);
    Color DiffColoring(Operation *op);
    Color ColorNone(Operation *op);

    struct HighlightColorer
    {
        using highlight_names_t = std::vector< std::string >;
        using opt_highlight_name_t = std::optional< std::string_view >;

        HighlightColorer(const highlight_names_t &highlight_nodes);

        Color operator()(Operation *op);

    private:
        highlight_names_t highlight_nodes;
        uint32_t color_counter = 0;
        std::unordered_map< std::string_view, uint32_t > node_to_color_map;

        opt_highlight_name_t highlight_name_for_op(Operation *op);
    };
}
