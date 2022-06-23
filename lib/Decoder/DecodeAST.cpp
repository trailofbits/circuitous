#include <circuitous/Decoder/DecodeAST.hpp>
#include <string>
namespace circ::decoder {

    void ExpressionPrinter::print(const Expr &e) {
        expr( e );
    }

    ExpressionPrinter& ExpressionPrinter::binary_op(BinaryOp< Expr > &binOp, const std::string &op) {
        expr(binOp.lhs()).raw(" ", op, " ").expr(binOp.rhs());
        return *this;
    }

    template < typename T >
    ExpressionPrinter&
    ExpressionPrinter::expr_array(const std::vector< T > &ops, const ExprStyle style) {
        switch (style) {
            case ExprStyle::FuncArgs: raw("("); break;
            case ExprStyle::FuncBody: raw("{").endl(); break;
        }

        for (std::size_t i = 0; i < ops.size(); i++) {
            print( ops[ i ] );

            if ( i != ops.size() - 1 ) {
                switch (style) {
                    case ExprStyle::FuncArgs: raw(", "); break;
                    case ExprStyle::FuncBody: endl() ; break;
                }
            }
        }

        switch (style) {
            case ExprStyle::FuncArgs: raw(")"); break;
            case ExprStyle::FuncBody: endl().raw("}").endl(); break;
        }
        return *this;
    }

    ExpressionPrinter &ExpressionPrinter::expr(const Expr &e) {
        std::visit( overloaded{
                [&](Plus &arg) {binary_op( arg, "+" );},
                [&](Mul &arg) {binary_op( arg, "*" );},
                [&](const Id &arg) { raw(arg);},
                [&](const Int &arg) { raw(arg.value);},
                [&](const Uint64 &arg) { raw("0b", std::bitset< 64 >( arg.value ));},
                [&](Expr &arg) {
                    os << "dont want this";
                    throw std::invalid_argument( "No plain expr allowed" );
                },
                [&](Empty &arg) {},
                [&](const Var &arg) { raw(arg.name); },
                [&](VarDecl &arg) { raw(arg.value().type, " ", arg.value().name); },
                [&](IndexVar &arg) { raw(arg.var.name, "[", arg.index, "]");},
                [&](Statement &arg) {
                    expr( arg.value()).raw(";").endl();
                },
                [&](Return &arg) {
                    raw("return ").expr( arg.value() ).raw(";");
                },
                [&](CastToUint64 &arg) {
                    raw("(uint64_t)").expr( arg.value());
                },
                [&](BitwiseNegate &arg) {
                    raw("~").expr( Parenthesis(*arg.value().op));
                },
                [&](Parenthesis &arg) {
                    raw("(").expr(arg.value()).raw(")");
                },
                [&](CurlyBrackets &arg) {
                    raw("{").endl().
                    expr(arg.value()).endl()
                    .raw("}");
                },
                [&](BitwiseOr &arg) {binary_op( arg, "|" );},
                [&](BitwiseXor &arg) {binary_op( arg, "^" );},
                [&](BitwiseAnd &arg) {binary_op( arg, "&" );},

                [&](Shfl &arg) {binary_op( arg, "<<" );},
                [&](Equal &arg) {binary_op( arg, "==" );},

                [&](Assign &arg) {binary_op( arg, "=" );},
                [&](And &arg) {binary_op( arg, "&&" );},
                [&](StatementBlock &arg) {
                    for (auto &e: arg) {
                        expr( e );
                    }
                },
                [&](IfElse &arg) {
                    raw("if").expr( Parenthesis(arg.cond()) ).
                    expr( arg.ifBody()).
                    raw(" else ").
                    expr( CurlyBrackets(arg.elseBody())).endl();
                },
                [&](const FunctionCall &arg) {
                    raw( arg.function_name ).
                    expr_array( arg.args, ExprStyle::FuncArgs);
                },
                [&](const FunctionDeclaration &arg) {
                    raw( arg.retType, " ", arg.function_name ).
                    expr_array( arg.args, ExprStyle::FuncArgs).endl().
                    expr_array( arg.body, ExprStyle::FuncBody).endl();
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

};