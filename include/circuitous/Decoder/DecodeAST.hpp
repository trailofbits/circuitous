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

    struct Var {
        explicit Var(Id s, Id t = "auto") : name( std::move( s )), type( std::move( t )) {};
        Id name;
        Id type;
    };

    struct Int {
        explicit Int(const int &v) : value( v ) {};
        int value;
    };

    struct Uint64 {
        uint64_t value;

        explicit Uint64(uint64_t val) : value( val ) {};
    };

    //Indexes into a variable, i.e var[<index>]
    struct IndexVar {
        IndexVar(const Var &var, uint32_t index) : var( var ), index( index ) {}

        Var var;
        uint32_t index;
    };

    template < typename T >
    struct Operator {
        template < typename... U >
        Operator(U &&... expression) {
            (ops.template emplace_back( std::forward<U>(expression) ), ...);
        }

        std::vector< T > ops;
    };

    template < typename T >
    struct BinaryOp : Operator< T > {
        using Operator< T >::Operator;

        T &lhs() {return this->ops[ 0 ];};
        T &rhs() {return this->ops[ 1 ];};
    };

    template < typename T >
    struct UnaryOp : Operator< T > {
        using Operator< T >::Operator;

        const T &value() {return this->ops[ 0 ];};
    };


#define CircuitousDecoder_GenerateUnaryOperator(name, T) \
    struct name : UnaryOp<T>{ using UnaryOp::UnaryOp; };


    CircuitousDecoder_GenerateUnaryOperator(VarDecl, Var)
    CircuitousDecoder_GenerateUnaryOperator(Statement, Expr)
    CircuitousDecoder_GenerateUnaryOperator(Return, Expr)
    CircuitousDecoder_GenerateUnaryOperator(CastToUint64, Expr)
    CircuitousDecoder_GenerateUnaryOperator(BitwiseNegate, Expr)
    CircuitousDecoder_GenerateUnaryOperator(Parenthesis, Expr)
    CircuitousDecoder_GenerateUnaryOperator(CurlyBrackets, Expr)

#undef CircuitousDecoder_GenerateUnaryOperator

#define CircuitousDecoder_GenerateBinaryOperator(name) \
        struct name : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };


    CircuitousDecoder_GenerateBinaryOperator(Plus)
    CircuitousDecoder_GenerateBinaryOperator(Mul)
    CircuitousDecoder_GenerateBinaryOperator(BitwiseOr)
    CircuitousDecoder_GenerateBinaryOperator(BitwiseXor)
    CircuitousDecoder_GenerateBinaryOperator(BitwiseAnd)
    CircuitousDecoder_GenerateBinaryOperator(Assign)
    CircuitousDecoder_GenerateBinaryOperator(Shfl)
    CircuitousDecoder_GenerateBinaryOperator(Equal)
    CircuitousDecoder_GenerateBinaryOperator(And)

#undef CircuitousDecoder_GenerateBinaryOperator

    using StatementBlock = std::vector< Expr >;

    struct FunctionDeclaration {
        FunctionDeclaration(const Id& retType, const Id& functionName,
                            std::vector< VarDecl > args, StatementBlock body)
        : retType( retType ), function_name( functionName ), args( args ), body( body ) {}

        Id retType;
        Id function_name;
        std::vector< VarDecl > args;
        StatementBlock body;
    };

    struct FunctionDeclarationBuilder{
        FunctionDeclarationBuilder& retType(const Id& retType);
        FunctionDeclarationBuilder& name(const Id& name);
        FunctionDeclarationBuilder& args(const std::vector< VarDecl >& args);
        FunctionDeclarationBuilder& body_insert(const Expr& expr);
        FunctionDeclarationBuilder& body(const StatementBlock& b);
        FunctionDeclaration make();

    private:
        Id m_retType;
        Id m_function_name;
        std::vector< VarDecl > m_args;
        StatementBlock m_body;
    };

    struct IfElse : Operator< Expr > {
        using Operator< Expr >::Operator;

        Expr &cond() {return this->ops[ 0 ];};
        Expr &ifBody() {return this->ops[ 1 ];};
        Expr &elseBody() {return this->ops[ 2 ];};
    };

    struct FunctionCall {
        Id function_name;
        std::vector< Expr > args;
    };

    struct Empty { };

    //TODO Is there a way in where we can merge the declare through macros and writing it down here?
    using op_t = std::variant<
            Expr, Int, Uint64, Id, //primitive types Expr, int, std::string
            Var, VarDecl, Statement, Return, CastToUint64, Parenthesis, CurlyBrackets, IndexVar,// unary
            Plus, Mul, BitwiseOr, BitwiseXor, BitwiseNegate, BitwiseAnd, Assign, Shfl, Equal, And, // binary
            IfElse,
            FunctionDeclaration, FunctionCall, // function
            StatementBlock, Empty
    >;



    struct Expr
    {
        template<typename T>
        Expr(T&& operand) : op(std::make_shared<op_t>(std::forward<op_t>(operand))){};

        // For exprs we just want only copy the pointer instead of create extra
        Expr(Expr& e) = default;
        Expr(const Expr& e) = default;
        Expr(Expr&& e) = default;
        Expr& operator=(const Expr& e) = default;
        Expr& operator=(Expr&& e) = default;


        std::shared_ptr<op_t> op;
    };


    enum class ExprStyle : uint32_t {
        FuncArgs,
        FuncBody,
    };


    class ExpressionPrinter{
    public:
        ExpressionPrinter(std::ostream& os) : os(os){};
        void print(const Expr& expr);
    private:
        std::ostream& os;


        template <typename T, typename... Ts>
        ExpressionPrinter &raw(T&& val, Ts&&... vals);
        ExpressionPrinter& expr(const Expr& expr);

        template < typename T >
        ExpressionPrinter& expr_array(const std::vector< T > &ops, const ExprStyle style);

        ExpressionPrinter& endl();
        ExpressionPrinter& binary_op(BinaryOp <Expr> &binOp, const std::string &op);
    };
}

