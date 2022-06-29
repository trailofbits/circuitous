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
        auto g = guard(gs);
        expr(binOp.lhs()).raw(" ", op, " ").expr(binOp.rhs());

        return *this;
    }

    template < typename T >
    ExpressionPrinter&
    ExpressionPrinter::expr_array(const std::vector< T > &ops, const ExprStyle style) {
        auto g = [&]()
        {
            switch (style) {
                case ExprStyle::FuncArgs: return guard( GuardStyle::Parens );
                case ExprStyle::FuncBody: return guard( GuardStyle::Curly );
            }
        }();

        for (std::size_t i = 0; i < ops.size(); i++) {
            print( ops[ i ] );

            if ( i != ops.size() - 1 ) {
                switch (style) {
                    case ExprStyle::FuncArgs: raw(", "); break;
                    case ExprStyle::FuncBody: endl() ; break;
                }
            }
        }

        return *this;
    }

    ExpressionPrinter &ExpressionPrinter::expr(const Expr &e) {
        std::cout << "called expr" << std::endl;
        if(e.op->valueless_by_exception()){
            circ::unreachable() << "valueless by excp";
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
                [&](const IndexVar &arg) { raw(arg.var.name, "[", arg.index, "]");},
                [&](const Statement &arg) {
                    expr( arg.value()).raw(";").endl();
                },
                [&](const Return &arg) {
                    raw("return ").expr( arg.value() ).raw(";");
                },
                [&](const CastToUint64 &arg) {
                    raw("((uint64_t)").expr( arg.value()).raw(")");
                },
                [&](const BitwiseNegate &arg) {
                    raw("~").expr( Parenthesis(*arg.value().op));
                },
                [&](const Parenthesis &arg) {
                    raw("(").expr(arg.value()).raw(")");
                },
                [&](const CurlyBrackets &arg) {
                    raw("{").endl().
                    expr(arg.value()).endl()
                    .raw("}");
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
                    raw("if").expr( Parenthesis(arg.cond()) )
                    .expr( arg.ifBody())
                    .raw(" else ")
                    .expr( CurlyBrackets(arg.elseBody())).endl();
                },
                [&](const FunctionCall &arg) {
                    raw( arg.function_name )
                    .expr_array( arg.args, ExprStyle::FuncArgs);
                },
                [&](const FunctionDeclaration &arg) {
                    raw( arg.retType, " ", arg.function_name )
                    .expr_array( arg.args, ExprStyle::FuncArgs).endl()
                    .expr_array( arg.body, ExprStyle::FuncBody).endl();
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

    Guard ExpressionPrinter::guard(GuardStyle g) {
        if(g == GuardStyle::None)
            return {"", "", os};
        else if( g == GuardStyle::Curly)
            return {"{", "}", os};
        else
            return {"(", ")", os};
    }
};