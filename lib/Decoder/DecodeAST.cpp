#include <circuitous/Decoder/DecodeAST.hpp>

namespace circ::decoder {
    void printBinaryOp(BinaryOp<Expr>& binOp,  std::ostream &os, const std::string & op){
        os << printExpr(binOp.lhs()) << " " << op << " " << printExpr(binOp.rhs());
    }

    void printExpr(const Expr &expr, std::ostream &os) {
        return std::visit(overloaded {
                [&](Plus& arg) { printBinaryOp(arg, os, "+"); },
                [&]( Mul& arg) { printBinaryOp(arg, os, "*"); },
                [&](const Id& arg) { os << arg; },
                [&](const Int& arg) { os << arg.v; },
                [&](const Uint64& arg) { os  << "0b" << std::bitset<64>(arg.value); }, // TODO prefix with 0b once old code gen is removed
                [&](Expr& arg) { os << "dont want this";},
                [&](Empty& arg) { },

                [&](const Var& arg) { os << arg.name; },
                [&](VarDecl& arg) { os << arg.value().type << " " << arg.value().name;},
                [&](IndexVar& arg) { os << arg.var.name << "[" << arg.index << "]";},
                [&](Statement& arg) {
                    printExpr(arg.value(), os); os << ";" << std::endl;
                },
                [&](Return& arg) { os << "return "; printExpr(arg.value(), os); os << ";"; },
                [&](CastToUint64& arg) { os << "(uint64_t)"; printExpr(arg.value(), os); },
                [&](BitwiseNegate& arg) { os << "~"; printExpr(Parenthesis(*arg.value().op), os); },
                [&](Parenthesis& arg) { os << "("; printExpr(arg.value(), os); os << ")"; },
                [&](CurlyBrackets& arg)  { os << "{"; printExpr(arg.value(), os); os << "}"; },

                [&](BitwiseOr& arg) { printBinaryOp(arg, os, "|");},
                [&](BitwiseXor& arg) { printBinaryOp(arg, os, "^"); },
                [&](BitwiseAnd& arg) { printBinaryOp(arg, os, "&"); },

                [&](Shfl& arg) { printBinaryOp(arg, os, "<<"); },
                [&](Equal& arg) { printBinaryOp(arg, os, "=="); },

                [&](Assign& arg) { printBinaryOp(arg, os, "="); },
                [&](And& arg) { printBinaryOp(arg, os, "&&"); },
                [&](StatementBlock & arg) {
                    for (auto& e: arg) {
                        printExpr(e,os);
                    }
                },
                [&](If & arg) {
                    os << "if(";
                    printExpr(arg.lhs(), os);
                    os << ") {\n";
                    printExpr(arg.rhs(), os);
                    os << "\n}\n";
                },


                [&](IfElse & arg) {
                    os << "if(";
                    printExpr(arg.cond(), os);
                    os << ") {\n";
                    printExpr(arg.ifBody(), os);
                    os << "\n}else{\n";
                    printExpr(arg.elseBody(), os);
                    os << "\n}\n";
                },

                [&](const FunctionCall& arg) {
                    os << arg.function_name;
                    printExprWrappedIn(arg.args, os, Wrapper::Parenthesis, ", ");
                },
                [&](const FunctionDeclaration& arg) {
                    os << arg.retType << " " << arg.function_name;
                    printExprWrappedIn(arg.args, os, Wrapper::Parenthesis, ", ");
                    os << std::endl;
                    printExprWrappedIn(arg.body, os, Wrapper::CurlyBrackets, ";\n");
                    os << std::endl;

                },
        }, *expr.op);
    }
    template < typename T >
    void printExprWrappedIn(T &op, std::ostream &os, const Wrapper wrappedIn) {
        printExprWrappedIn(std::vector<T>{op}, os, wrappedIn, "");
    }

    template < typename T >
    void printExprWrappedIn(const std::vector< T > &ops, std::ostream &os, const Wrapper wrappedIn,
                            const std::string &delim) {
        switch (wrappedIn) {
            case Wrapper::Parenthesis: os << "("; break;
            case Wrapper::CurlyBrackets: os << "{" << std::endl; break;
        }

        for(std::size_t i =0 ; i< ops.size(); i++){
            printExpr(ops[i], os);
            if(i != ops.size()-1){
                printExpr(Var{delim}, os);
            }
        }

        switch (wrappedIn) {
            case Wrapper::Parenthesis: os << ")"; break;
            case Wrapper::CurlyBrackets:  os  << std::endl << "}" << std::endl; break;
        }
    }

    std::string printExpr(const Expr &expr) {
        std::stringstream ss;
        printExpr(expr, ss);
        return ss.str();
    }
}