#include <circuitous/Decoder/DecodeAST.hpp>
#include <string>
namespace circ::decoder {

    void ExpressionPrinter::print(const Expr &e) {
        expr( e );
    }

    ExpressionPrinter & ExpressionPrinter::binary_op(BinaryOp <Expr> &binOp,
                                                     const std::string &op,
                                                     bool add_parenthesis = true) {
        if(add_parenthesis){
            raw("(");
        }
        expr(binOp.lhs()).raw(" ", op, " ").expr(binOp.rhs());
        if(add_parenthesis){
            raw(")");
        }
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
                [&](Expr &arg) { circ::unreachable() << "No plain expr allowed" ; },
                [&](Empty &arg) {},
                [&](const Var &arg) { raw(arg.name); },
                [&](VarDecl &arg) { raw(arg.value().type, " ", arg.value().name); },
                [&](const IndexVar &arg) { raw(arg.var.name, "[", arg.index, "]");},
                [&](Statement &arg) {
                    expr( arg.value()).raw(";").endl();
                },
                [&](Return &arg) {
                    raw("return ").expr( arg.value() ).raw(";");
                },
                [&](CastToUint64 &arg) {
                    raw("((uint64_t)").expr( arg.value()).raw(")");
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
                [&](BitwiseOr &arg) {binary_op( arg, "|");},
                [&](BitwiseXor &arg) {binary_op( arg, "^");},
                [&](BitwiseAnd &arg) {binary_op( arg, "&");},

                [&](Shfl &arg) {binary_op( arg, "<<");},
                [&](Equal &arg) {binary_op( arg, "==");},

                [&](Assign &arg) {binary_op( arg, "=", false);},
                [&](And &arg) {binary_op( arg, "&&");},
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


    FunctionDeclarationBuilder &FunctionDeclarationBuilder::retType(const Id& ret) {
        m_retType = ret;
        return *this;
    }

    FunctionDeclarationBuilder &FunctionDeclarationBuilder::name(const Id& name) {
        m_function_name = name;
        return *this;
    }

    FunctionDeclarationBuilder &FunctionDeclarationBuilder::args(const std::vector< VarDecl >& args) {
        m_args = args;
        return *this;
    }

    FunctionDeclarationBuilder &FunctionDeclarationBuilder::body_insert(const Expr& expr) {
        m_body.emplace_back(expr);
        return *this;
    }

    FunctionDeclarationBuilder &FunctionDeclarationBuilder::body(const StatementBlock& b) {
        m_body = b;
        return *this;
    }

    FunctionDeclaration FunctionDeclarationBuilder::make() {
        return FunctionDeclaration( m_retType, m_function_name, m_args, m_body );
    }
};