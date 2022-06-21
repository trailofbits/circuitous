#include <circuitous/Decoder/DecodeAST.hpp>

namespace circ::decoder {
    void print_binary_op(BinaryOp<Expr>& binOp, std::ostream &os, const std::string & op){
        print_expr(binOp.lhs(), os);
        os << " " << op << " " ;
        print_expr(binOp.rhs(), os);
    }

    void print_expr(const Expr &expr, std::ostream &os) {
        return std::visit(overloaded {
                [&](Plus& arg) {print_binary_op( arg, os, "+" ); },
                [&]( Mul& arg) {print_binary_op( arg, os, "*" ); },
                [&](const Id& arg) { os << arg; },
                [&](const Int& arg) { os << arg.value; },
                [&](const Uint64& arg) { os  << "0b" << std::bitset<64>(arg.value); },
                [&](Expr& arg) { os << "dont want this"; throw std::invalid_argument("No plain expr allowed");},
                [&](Empty& arg) { },

                [&](const Var& arg) { os << arg.name; },
                [&](VarDecl& arg) { os << arg.value().type << " " << arg.value().name;},
                [&](IndexVar& arg) { os << arg.var.name << "[" << arg.index << "]";},
                [&](Statement& arg) {print_expr( arg.value(), os ); os << ";" << std::endl; },
                [&](Return& arg) { os << "return ";
                    print_expr( arg.value(), os ); os << ";"; },
                [&](CastToUint64& arg) { os << "(uint64_t)";
                    print_expr( arg.value(), os ); },
                [&](BitwiseNegate& arg) { os << "~";
                    print_expr( Parenthesis( *arg.value().op ), os ); },
                [&](Parenthesis& arg) { os << "(";
                    print_expr( arg.value(), os ); os << ")"; },
                [&](CurlyBrackets& arg)  { os << "{" << std::endl;
                    print_expr( arg.value(), os ); os << std::endl << "}"; },

                [&](BitwiseOr& arg) {print_binary_op( arg, os, "|" );},
                [&](BitwiseXor& arg) {print_binary_op( arg, os, "^" ); },
                [&](BitwiseAnd& arg) {print_binary_op( arg, os, "&" ); },

                [&](Shfl& arg) {print_binary_op( arg, os, "<<" ); },
                [&](Equal& arg) {print_binary_op( arg, os, "==" ); },

                [&](Assign& arg) {print_binary_op( arg, os, "=" ); },
                [&](And& arg) {print_binary_op( arg, os, "&&" ); },
                [&](StatementBlock & arg) {
                    for (auto& e: arg) {
                        print_expr( e, os );
                    }
                },
                [&](IfElse & arg) {
                    os << "if";
                    print_expr( Parenthesis( arg.cond()), os );
                    print_expr( CurlyBrackets( arg.ifBody()), os );
                    os << " else ";
                    print_expr( CurlyBrackets( arg.elseBody()), os );
                    os << std::endl;
                },
                [&](const FunctionCall& arg) {
                    os << arg.function_name;
                    print_expr_array( arg.args, os, Wrapper::Parenthesis, ", " );
                },
                [&](const FunctionDeclaration& arg) {
                    os << arg.retType << " " << arg.function_name;
                    print_expr_array( arg.args, os, Wrapper::Parenthesis, ", " );
                    os << std::endl;
                    print_expr_array( arg.body, os, Wrapper::CurlyBrackets, "\n" );
                    os << std::endl;
                },
        }, *expr.op);
    }


    template < typename T >
    void print_expr_array(const std::vector< T > &ops, std::ostream &os, const Wrapper wrappedIn,
                          const std::string &delim) {
        switch (wrappedIn) {
            case Wrapper::Parenthesis: os << "("; break;
            case Wrapper::CurlyBrackets: os << "{" << std::endl; break;
        }

        for(std::size_t i =0 ; i< ops.size(); i++){
            print_expr( ops[ i ], os );
            if(i != ops.size()-1){
                os << delim;
            }
        }

        switch (wrappedIn) {
            case Wrapper::Parenthesis: os << ")"; break;
            case Wrapper::CurlyBrackets:  os  << std::endl << "}" << std::endl; break;
        }
    }
}