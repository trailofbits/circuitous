#include <circuitous/Decoder/DecodeAST.hpp>
#include <string>
#include <variant>

namespace circ::decoder {

    void ExpressionPrinter::print(const Expr &e) {
        expr( e );
    }

    ExpressionPrinter & ExpressionPrinter::binary_op(const BinaryOp <Expr> &binOp,
                                                     const std::string &op,
                                                     GuardStyle gs) {
        auto g = make_guard(gs);
        expr(binOp.lhs()).raw(" ", op, " ").expr(binOp.rhs());

        return *this;
    }

    template < typename T >
    ExpressionPrinter&
    ExpressionPrinter::expr_array(const std::vector< T > &ops, const ExprStyle style) {
        auto g = [&]()
        {
            switch (style) {
                case ExprStyle::FuncArgs: return make_guard( GuardStyle::Parens );
                case ExprStyle::FuncBody: return make_guard( GuardStyle::Curly );
            }
        }();

        for (std::size_t i = 0; i < ops.size(); i++) {
            print( ops[ i ] );

            if ( i != ops.size() - 1 ) {
                switch (style) {
                    case ExprStyle::FuncArgs: raw(", "); break;
                    case ExprStyle::FuncBody: raw(";").endl() ; break;
                }
            }
        }

        return *this;
    }

    ExpressionPrinter &ExpressionPrinter::expr(const Expr &e) {
        if(e.op->valueless_by_exception()){
            circ::unreachable() << "valueless by exception";
        }
        std::visit( overloaded{
                [&](const Plus &arg) {binary_op( arg, "+" );},
                [&](const Mul &arg) {binary_op( arg, "*" );},
                [&](const Id &arg) { raw(arg);},
                [&](const Int &arg) { raw(arg.value);},
                [&](const Uint64 &arg) { raw("0b", std::bitset< 64 >( arg.value ));},
                [&](const Expr &arg) { circ::unreachable() << "No plain expr allowed" ; },
                [&](const Empty &arg) {},
                [&](const Var &arg) { raw(arg.name); },
                [&](const VarDecl &arg) { raw(arg.value().type, " ", arg.value().name); },
                [&](const IndexVar &arg) { expr(arg.lhs()).expr(arg.rhs(), GuardStyle::Square); },
                [&](const Dereference &arg) { raw("(*").expr(arg.value()).raw(")"); }, // TODO(sebas): guard styles don't take results of raw as a direct argument
                [&](const Statement &arg) {
                    expr( arg.value()).raw(";").endl();
                },
                [&](const Return &arg) {
                    raw("return ").expr( arg.value() ).raw(";");
                },
                [&](const CastToUint64 &arg) {
                    raw("static_cast<uint64_t>").expr( arg.value(), GuardStyle::Parens);
                },
                [&](const BitwiseNegate &arg) {
                    raw("~").expr( *arg.value().op, GuardStyle::Parens);
                },
                [&](const BitwiseOr &arg) {binary_op( arg, "|");},
                [&](const BitwiseXor &arg) {binary_op( arg, "^");},
                [&](const BitwiseAnd &arg) {binary_op( arg, "&");},
                [&](const And &arg) {binary_op( arg, "&&");},
                [&](const Shfl &arg) {binary_op( arg, "<<");},

                [&](const Equal &arg) {binary_op( arg, "==");},
                [&](const Assign &arg) {binary_op( arg, "=", GuardStyle::None);},
                [&](const StatementBlock &arg) {
                    for (auto &e: arg) {
                        expr( e );
                    }
                },
                [&](const IfElse &arg) {
                    raw("if").expr( arg.cond(), GuardStyle::Parens )
                    .expr( arg.ifBody(), GuardStyle::Curly)
                    .raw(" else ")
                    .expr( arg.elseBody(), GuardStyle::Curly).endl();
                },
                [&](const FunctionCall &arg) {
                    raw( arg.function_name ).expr_array( arg.args, ExprStyle::FuncArgs);
                },
                [&](const FunctionDeclaration &arg) {
                    raw( arg.retType, " ", arg.function_name )
                    .expr_array( arg.args, ExprStyle::FuncArgs).endl()
                    .expr_array( arg.body, ExprStyle::FuncBody).endl().endl();
                },
        }, *e.op );
        return *this;
    }

    template <typename T, typename... Ts>
    ExpressionPrinter &ExpressionPrinter::raw(T&& val, Ts&&... vals) {
        os << std::forward<T>(val);
        ((os << std::forward<Ts>(vals)), ...);
        return *this;
    }

    ExpressionPrinter &ExpressionPrinter::endl() {
        os << std::endl;
        return *this;
    }

    Guard ExpressionPrinter::make_guard(GuardStyle g) {
        switch(g){
            case GuardStyle::None :return {"", "", os};
            case GuardStyle::Square :return {"[", "]", os};
            case GuardStyle::Parens :return {"  (", ") ", os};
            case GuardStyle::Curly :return {"{", "}", os, true};
            default: circ::unreachable() << "invalid guard style";
        }
    }

    ExpressionPrinter::self_t &ExpressionPrinter::expr(const Expr &e, const GuardStyle gs) {
        auto g = make_guard(gs); // needs to be bound to a variable to force proper scoping
        expr(e);
        return *this;
    }
};
