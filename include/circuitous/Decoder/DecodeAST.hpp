#include <string>
#include <iostream>
#include <variant>
#include <vector>
#include <memory>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace circ::decoder::ast {

    struct Expr;
    using Id = std::string;
    struct Var { Id name; Id type; explicit Var(Id s, Id t = "auto"): name(std::move(s)), type(std::move(t)){};};
    struct Int { int v; explicit Int(const int& v):v(v){}; };

    template <typename T>
    struct UnaryOperator{
        explicit UnaryOperator(const std::shared_ptr< T > &expr) : value( expr ) {}
        explicit UnaryOperator(const T&& expression) {
            value = std::make_shared<T>(expression);
        }

        explicit UnaryOperator(const T& expression) {
            value = std::make_shared<T>(expression);
        }
        std::shared_ptr<T> value;
    };

    struct BinaryOperator {
        BinaryOperator(std::shared_ptr<Expr>& lhs, std::shared_ptr<Expr>& rhs) : lhs(lhs), rhs(rhs) {}

        BinaryOperator(std::shared_ptr<Expr>& lhs, const Expr&& right) : lhs(lhs) {
            rhs = std::make_shared<Expr>(right);
        }

        BinaryOperator(const Expr&& left, std::shared_ptr<Expr>& rhs) : rhs(rhs) {
            lhs = std::make_shared<Expr>(left);
        }

        BinaryOperator(const Expr&& left, const Expr&& right) {
            lhs = std::make_shared<Expr>(left);
            rhs = std::make_shared<Expr>(right);
        }

        std::shared_ptr<Expr> lhs;
        std::shared_ptr<Expr> rhs;
    };

#define GenUnaryOperator(name, T) \
    struct name : UnaryOperator<T>{ using UnaryOperator::UnaryOperator; };


    GenUnaryOperator(VarDecl, Var)
    GenUnaryOperator(Statement, Expr)


#define GenUnaryExpression(name) \
        struct name : UnaryOperator<Expr>{ using UnaryOperator::UnaryOperator; };

    GenUnaryExpression(Return)
    GenUnaryExpression(TypeExpr) // maybe seperate from declare, like in func args
    GenUnaryExpression(Parenthesis)
    GenUnaryExpression(CastToUint64)

#define BinOpStruct(name) \
        struct name : BinaryOperator{ using BinaryOperator::BinaryOperator; };


    BinOpStruct(Plus)
    BinOpStruct(Mul)
    BinOpStruct(BitwiseOr)
    BinOpStruct(BitwiseXor)
    BinOpStruct(BitwiseNegate)
    BinOpStruct(Assign)
    BinOpStruct(Shfl)
    BinOpStruct(Equal)
    BinOpStruct(Index) //?


    struct FunctionDeclaration {
        Id retType;
        Id function_name;
        std::vector<VarDecl> args;
        std::vector<Statement> body;
    };


    struct FunctionCall{
        Id function_name;
        std::vector<Expr> args;
    };


//TODO Is there a way in where we can merge the declare through macros and writing it down here?
    using op_t = std::variant< Expr, Int, Id,
            Var, VarDecl, Statement,
            Return, TypeExpr, Parenthesis, CastToUint64,
            Plus, Mul, BitwiseOr, BitwiseXor, BitwiseNegate, Assign, Shfl, Equal, Index,
            FunctionDeclaration, FunctionCall >;



    struct Expr
    {
        // Implicit constructors help a lot for prettier calling
        Expr(const std::shared_ptr<op_t> &op) : op(op) {}
        Expr(std::shared_ptr<op_t> &op) : op(op) {}
        template<typename T>
        Expr(T&& operand){
            //TODO get saner error handling, this just segfaults if T is not in op_T
            op  = std::make_shared<op_t>(operand);
        }

        std::shared_ptr<op_t> op;
    };

    void printExpr(const Expr& op);

    template <typename T>
    void printExpr(const std::vector<T>& ops, const std::string& delim = " "){
        for(std::size_t i =0 ; i< ops.size(); i++){
            printExpr(ops[i]);
            if(i != ops.size()-1){
                printExpr(Var{delim});
            }
        }
    }


    void printExpr(const Expr& op){
        return std::visit(overloaded {
                [&](const Plus& arg) {
                    printExpr(*arg.lhs); std::cout << " + ";
                    printExpr(*arg.rhs);
                },
                [&](const Mul& arg) {
                    printExpr(*arg.lhs); std::cout << " * ";
                    printExpr(*arg.rhs);
                },

                [&](const BitwiseOr& arg) {  },
                [&](const BitwiseXor& arg) {  },
                [&](const BitwiseNegate& arg) {  },
                [&](const Assign& arg) {  },

                [&](const Int& arg) { std::cout << arg.v; },
                [&](const Var& arg) { std::cout << arg.name; },
                [&](const FunctionDeclaration& arg) {
                    std::cout << arg.retType << " " << arg.function_name << "(";
                    printExpr(arg.args, ", ");
                    std::cout << ") { \n";
                    printExpr(arg.body, "");
                    std::cout << "}";
                },
                [&](const Expr& arg) { std::cout << "dont want this"; throw std::invalid_argument("throw! ");},
                [&](const VarDecl& arg) { std::cout << arg.value->type << " " << arg.value->name;},
                [&](const Statement& arg) { printExpr(*arg.value); std::cout << ";" << std::endl;},
        }, *op.op);
    }

    int main()
    {
        FunctionDeclaration funcDecl;
        funcDecl.function_name = "sum";
        funcDecl.retType = "int";
        Var a("a", "int");
        Var b("b", "int");

        funcDecl.args.emplace_back(VarDecl(a));
        funcDecl.args.emplace_back(VarDecl(b));

        auto p = Plus(a,b);
        funcDecl.body.emplace_back(Statement(p));
        printExpr(funcDecl);

        return 0;
    }


}