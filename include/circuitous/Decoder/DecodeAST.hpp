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
        explicit Int(const int64_t v) : value( v ) {};
        int64_t value;
    };

    struct Uint64 {
        uint64_t value;

        explicit Uint64(uint64_t val) : value( val ) {};
    };



    template < typename T >
    struct NAryOperation {
        template < typename... U >
        NAryOperation(U &&... expression) {
            (ops.template emplace_back( std::forward<U>(expression) ), ...);
        }

        std::vector< T > ops;
    };

    template < typename T >
    struct BinaryOp : NAryOperation< T > {
        using NAryOperation< T >::NAryOperation;

        const T &lhs() const {return this->ops[ 0 ];};
        const T &rhs() const {return this->ops[ 1 ];};
    };

    template < typename T >
    struct UnaryOp : NAryOperation< T > {
        using NAryOperation< T >::NAryOperation;

        const T &value() const {return this->ops[ 0 ];} ;
    };

    struct VarDecl: UnaryOp<Var>{ using UnaryOp::UnaryOp; };
    struct Statement: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct Return: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct CastToUint64: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct BitwiseNegate: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct Parenthesis: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct CurlyBrackets: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };



    struct Plus : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Mul : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseOr : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseXor : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseAnd : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Assign : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Shfl : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Equal : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct And : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct IndexVar : BinaryOp<Expr>{ using BinaryOp::BinaryOp; }; // //Indexes into a variable, i.e var[<index>]

    // TODO there are subtypes of Statements like Returns which also belong here
    // So currently we just take Expr
    using StatementBlock = std::vector< Expr >;

    struct FunctionDeclaration {
        FunctionDeclaration(const Id& retType, const Id& functionName,
                            const std::vector< VarDecl > &args, const StatementBlock &body)
        : retType( retType ), function_name( functionName ), args( args ), body( body ) {}

        Id retType;
        Id function_name;
        std::vector< VarDecl > args;
        StatementBlock body;
    };

    struct FunctionDeclarationBuilder{
        using self_t = FunctionDeclarationBuilder;
        self_t& retType(const Id& retType) {_retType = retType; return *this;};
        self_t& name(const Id& name) { _function_name = name; return *this; };
        self_t& arg_insert(const VarDecl& args) { _args.emplace_back(args); return *this; };
        self_t& body_insert(const Expr& expr) { _body.emplace_back(expr); return *this; };
        self_t& body(const StatementBlock& b) { _body = b; return *this; };
        FunctionDeclaration make() {
            return FunctionDeclaration( _retType, _function_name, _args, _body );
        };

    private:
        Id _retType;
        Id _function_name;
        std::vector< VarDecl > _args;
        StatementBlock _body;
    };

    struct IfElse : NAryOperation< Expr > {
        using NAryOperation< Expr >::NAryOperation;

        const Expr &cond() const {return this->ops[ 0 ];};
        const Expr &ifBody() const {return this->ops[ 1 ];};
        const Expr &elseBody() const {return this->ops[ 2 ];};
    };

    struct FunctionCall {
        FunctionCall(const Id &functionName, std::vector< Expr > args)
                                    : function_name( functionName ), args( args ) {} ;

        Id function_name;
        std::vector< Expr > args;
    };

    struct Empty { };

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
        Expr(T operand) : op(std::make_shared<op_t>(std::forward<op_t>(operand))){};

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

    enum class GuardStyle : uint32_t{
        None,
        Parens,
        Curly,
        Square
    };

    struct Guard{
        Guard(const std::string &g1, std::string g2, std::ostream &os, bool endl_on_insides = false)
                : g1( g1 ), g2( g2 ), os( os ), endl_on_insides(endl_on_insides) {
            os << g1;
            if(endl_on_insides)
                os << std::endl;
        }
        ~Guard(){
            if(endl_on_insides)
                os << std::endl;
            os << g2;
        }

        std::string g1;
        std::string g2;
        std::ostream& os;
        bool endl_on_insides;
    };

    class ExpressionPrinter{
    public:
        ExpressionPrinter(std::ostream& os) : os(os){};
        void print(const Expr& expr);
    private:
        std::ostream& os;
        using self_t = ExpressionPrinter;
        Guard guard(GuardStyle g);

        template <typename T, typename... Ts>
        self_t &raw(T &&val, Ts &&... vals) ;
        self_t &expr(const Expr &expr);
        self_t &expr(const Expr &e, const GuardStyle gs);

        template < typename T >
        self_t &expr_array(const std::vector< T > &ops, const ExprStyle style);
        self_t &endl();
        self_t &binary_op(const BinaryOp <Expr> &binOp, const std::string &op,
                          GuardStyle gs = GuardStyle::Parens);
    };
}

