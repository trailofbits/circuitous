#include <string>
#include <iostream>
#include <variant>
#include <vector>
#include <memory>
#include <circuitous/IR/Circuit.hpp>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace circ::decoder {

    struct Expr;
    using Id = std::string;
    struct Var { Id name; Id type; explicit Var(Id s, Id t = "auto"): name(std::move(s)), type(std::move(t)){};};
    struct Int { int v; explicit Int(const int& v):v(v){}; };
    struct Uint64 {
        uint64_t value;
        explicit Uint64(uint64_t val) :value(val){};
    };

    struct IndexVar{
        IndexVar(const Var &var, uint32_t index): var( var ), index( index ) {}

        Var var;
        uint32_t index;
    };
    template <typename T>
    struct Operator{
        template <typename... U>
        Operator(U&& ... expression ){
            (ops.template emplace_back(expression), ...);
        }

        std::vector<T> ops;
    };

    template<typename T>
    struct BinaryOp : Operator<T>{
        using Operator<T>::Operator;
        T& lhs(){ return this->ops[0]; };
        T& rhs(){ return this->ops[1]; };
    };

    template<typename T>
    struct UnaryOp : Operator<T>{
        using Operator<T>::Operator;
        T& value(){ return this->ops[0]; };
    };


#define GenUnaryOperator(name, T) \
    struct name : UnaryOp<T>{ using UnaryOp::UnaryOp; };


    GenUnaryOperator(VarDecl, Var)
    GenUnaryOperator(Statement, Expr)


#undef GenUnaryOperator

#define GenUnaryExpression(name) \
        struct name : UnaryOp<Expr>{ using UnaryOp::UnaryOp; };

    GenUnaryExpression(Return)
    GenUnaryExpression(CastToUint64)
    GenUnaryExpression(BitwiseNegate)
    GenUnaryExpression(Parenthesis)
    GenUnaryExpression(CurlyBrackets)

#undef GenUnaryExpression

#define GenBinaryExpression(name) \
        struct name : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };


    GenBinaryExpression(Plus)
    GenBinaryExpression(Mul)
    GenBinaryExpression(BitwiseOr)
    GenBinaryExpression(BitwiseXor)
    GenBinaryExpression(BitwiseAnd)
    GenBinaryExpression(Assign)
    GenBinaryExpression(Shfl)
    GenBinaryExpression(Equal)
    GenBinaryExpression(And)
    GenBinaryExpression(If)
//GenBinaryExpression(Index) //?

#undef GenBinaryExpression
    using StatementBlock = std::vector<Expr>;

    struct FunctionDeclaration {
        Id retType;
        Id function_name;
        std::vector<VarDecl> args;
        StatementBlock body;
    };


//    struct IfElse{
//        Expr cond;
//        StatementBlock& ifBody;
//    };


    struct IfElse: Operator<Expr>{
        using Operator<Expr>::Operator;
        Expr& cond(){ return this->ops[0]; };
        Expr& ifBody(){ return this->ops[1]; };
        Expr& elseBody(){ return this->ops[2]; };
    };


    struct FunctionCall{
        Id function_name;
        std::vector<Expr> args;
    };

    struct Empty{};

    //TODO Is there a way in where we can merge the declare through macros and writing it down here?
    using op_t = std::variant<
            Expr, Int, Uint64, Id, //primitive types Expr, int, std::string
            Var, VarDecl, Statement, Return, CastToUint64, Parenthesis, CurlyBrackets, IndexVar,// unary
            Plus, Mul, BitwiseOr, BitwiseXor, BitwiseNegate, BitwiseAnd, Assign, Shfl, Equal, And, // binary
            If, IfElse,
            FunctionDeclaration, FunctionCall, // function
            StatementBlock, Empty
    >;



    struct Expr
    {
        template<typename T>
        Expr(T&& operand){
            //TODO get saner error handling, this just segfaults if T is not in op_T
            // Also it seems that we can gain an infinite loop through wrongly converting to an Expr?
            op  = std::make_shared<op_t>(std::forward<op_t>(operand));
        }

        // For exprs we just want only copy the pointer instead of create extra
        Expr(Expr&& e) = default;
        Expr(Expr& e) = default;
        Expr(const Expr& e) = default;

        std::shared_ptr<op_t> op;
    };


    enum class Wrapper :uint32_t {
        Parenthesis,
        CurlyBrackets,
    };



    template <typename T>
    void printExprWrappedIn(const std::vector<T>& ops, std::ostream& os, const Wrapper wrappedIn = Wrapper::Parenthesis, const std::string& delim = " ");
    void printExpr(const Expr& expr, std::ostream& os);
    std::string printExpr(const Expr& expr);
}
