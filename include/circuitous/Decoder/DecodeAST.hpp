#pragma once

#include <string>
#include <iostream>
#include <variant>
#include <vector>
#include <memory>
#include <circuitous/IR/Circuit.hpp>
#include <stack>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace circ::decoder {

    struct Expr;

    using Id = std::string;

    struct Type
    {
        Type();
        explicit Type( Id name );
        explicit Type( Id name, std::vector< Expr > templateParameters );
        Id name;

        std::vector< Expr > template_parameters;
        bool is_constexpr = false;
        bool is_const = false;
        bool is_static = false;

        bool equals( const Type &rhs);
    };

    struct Var {
        explicit Var( Id s );
        explicit Var( Id s, Type t, bool is_struct = false, bool is_pointer = false );

        Id name;
        Type type;
        bool is_struct;
        bool is_pointer;
    };

    struct Int {
        explicit Int(const int64_t v) : value( v ) {};
        int64_t value;
    };

    struct Uint64 {
        uint64_t value;

        explicit Uint64(uint64_t val) : value( val ) {};
    };

    /*
     * Main easy storage class.
     */
    template < typename T , int n = -1 >
    struct NAryOperation {
        template < typename... U >
        NAryOperation(U &&... expression) {
            (ops.template emplace_back( std::forward<U>(expression) ), ...);

            if constexpr ( n > 0 )
            {
                circ::check( ops.size() == n )
                    << "Expression required " << n << " arguments, got: " << ops.size();
            }
        }

        std::vector< T > ops;
    };

    template < typename T >
    struct BinaryOp : NAryOperation< T, 2 > {
        using NAryOperation< T, 2 >::NAryOperation;

        const T &lhs() const {return this->ops[ 0 ];};
        const T &rhs() const {return this->ops[ 1 ];};
    };

    template < typename T >
    struct UnaryOp : NAryOperation< T, 1 > {
        using NAryOperation< T, 1 >::NAryOperation;

        const T &value() const {return this->ops[ 0 ];} ;
    };

    struct VarDecl: UnaryOp<Var>{ using UnaryOp::UnaryOp; };
    struct Statement: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct Return: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct CastToUint64: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct BitwiseNegate: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };
    struct Dereference: UnaryOp<Expr>{ using UnaryOp::UnaryOp; };



    struct Plus : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Mul : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseOr : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseXor : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct BitwiseAnd : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Assign : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct MemberInit : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Shfl : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct Equal : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct And : BinaryOp<Expr>{ using BinaryOp::BinaryOp; };
    struct IndexVar : BinaryOp<Expr>{ using BinaryOp::BinaryOp; }; // //Indexes into a variable, i.e var[<index>]

    // TODO there are subtypes of Statements like Returns which also belong here
    // So currently we just take Expr
    using StatementBlock = std::vector< Expr >;

    struct FunctionCall {
        FunctionCall( const Var &var, const Id &func, std::vector< Expr > args );

        FunctionCall(const Id &functionName, std::vector< Expr > args);

        FunctionCall(const Id &functionName, std::vector< Expr > args, std::vector< Expr > template_parameters);

        Id function_name;
        std::vector< Expr > args;
        std::vector< Expr > template_parameters;
    };

    struct FunctionDeclaration {
        FunctionDeclaration(const Type& retType, const Id& functionName,
                            const std::vector< VarDecl > &args, const StatementBlock &body);
        Type retType;
        Id function_name;
        std::vector< VarDecl > args;
        StatementBlock body;
    };

    struct FunctionDeclarationBuilder{
        using self_t = FunctionDeclarationBuilder;
        self_t& retType(const Type& retType);
        self_t& name(const Id& name);
        self_t& arg_insert(const VarDecl& args);
        self_t& body_insert(const Expr& expr);
        self_t& body_insert_statement(const Expr& expr);
        self_t& body(const StatementBlock& b);

        Var get_new_arg(Type t);

        FunctionDeclaration make();

    protected:
        Type _retType;
        Id _function_name;
        std::vector< VarDecl > _args;
        StatementBlock _body;

        std::string arg_prefix = "arg_";
        int arg_suffix_counter = 0;
    };

    struct ConstructorDeclaration : FunctionDeclaration
    {
        ConstructorDeclaration( const Type &retType, const Id &functionName,
                             const std::vector< VarDecl > &args, const StatementBlock &body,
                             const std::vector< MemberInit > &members_init,
                             const std::vector< FunctionCall > &init_calls );

        std::vector< MemberInit > member_inits;
        std::vector< FunctionCall > init_calls;
    };

    struct ConstructorDeclarationBuilder : FunctionDeclarationBuilder
    {
        using self_t = ConstructorDeclarationBuilder;

        self_t& member_init_insert(const MemberInit& member_init);
        self_t& init_call_insert(const FunctionCall& init_call);

        ConstructorDeclaration make();

    private:
        std::vector< MemberInit > _member_inits;
        std::vector< FunctionCall > _init_calls;
    };

    struct If : BinaryOp< Expr >
    {
        using BinaryOp::BinaryOp;

        const Expr &cond() const;
        const Expr &ifBody() const;
    };

    struct IfElse : NAryOperation< Expr > {
        using NAryOperation< Expr >::NAryOperation;

        const Expr &cond() const;
        const Expr &ifBody() const;
        const Expr &elseBody() const;
    };


    using case_value_expr = std::pair< Expr , Expr >;
    struct Switch
    {
        Switch(const Expr& e);

        std::shared_ptr< Expr > cond;
        std::vector< case_value_expr > cases;
    };

    struct Enum;

    struct EnumValue
    {
        EnumValue( const std::shared_ptr< Enum > &e, std::size_t index ) :
            e( e ), index( index )
        {
        }

        std::shared_ptr< Enum > e;
        std::size_t index;
    };

    struct Enum
    {
        Enum( const Id &enumName ) : enum_name( enumName ) { }
        void Register( const Id &value_name ) { names.push_back( value_name ); };
        Id index( std::size_t index ) { return names[ index ]; }
        std::optional< EnumValue > get_by_name( Id name )
        {
            auto index = std::find_if( names.begin(), names.end(),
                                       [ & ]( Id known_name ) { return known_name == name; } );
            if ( index != names.end() )
                return EnumValue( std::make_shared< Enum >( *this ),
                                  static_cast< size_t >( index - names.begin() ) );
            return std::nullopt;
        }

        Id enum_name;
        std::vector< Id > names;
    };

    struct EnumDecl: UnaryOp<Enum>{ using UnaryOp::UnaryOp; };

    struct Struct
    {
        Struct( const int templateSize = 0 );
        Struct( const int templateSize, const std::vector< Expr > &derivedFrom );

        Id name;
        const int template_size;
        const bool templatized;
        std::vector< Id > template_typenames;

        std::vector< Expr > derived_from; // should be structs, but our printers crash if this is a struct
        std::vector< VarDecl > default_init_variables;
        std::vector< Assign > assignment_init_variables;
        std::vector< FunctionDeclaration > methods;
        std::vector< ConstructorDeclaration > constructors;
    };

//    struct StructDecl: UnaryOp<Struct>
//    {
//        using UnaryOp::UnaryOp;
////        explicit StructDecl() { }
//    };

    struct Empty : NAryOperation< Expr, 0 >
    {
        using NAryOperation::NAryOperation;
    };

    using op_t = std::variant<
            Expr, Int, Uint64, Id, Type, //primitive types Expr, int, std::string
            Var, VarDecl, Statement, Return, CastToUint64, IndexVar, EnumValue, Enum, EnumDecl, Dereference, // unary
            Plus, Mul, BitwiseOr, BitwiseXor, BitwiseNegate, BitwiseAnd, Assign, MemberInit, Shfl, Equal, And, // binary
            If, IfElse, Switch, case_value_expr,
            FunctionDeclaration, ConstructorDeclaration, FunctionCall, // function
            Struct,
            StatementBlock, Empty
    >;


    /*
     * Expr is meant to emulate a recursive sum type.
     * To the best of my knowledge c++ does not support this directly
     * As it is only a wrapper over a shared_ptr it allows for safe/heavy use of implicit conversion
     * between the instantiations of the sum type.
     */
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
        EnumBody,
        TemplateParams,
        StructDecl,
        StructMethods,
        StructVars,
        StructDerivations,
        StructMemberInitialization,
        Switch
    };

    enum class GuardStyle : uint32_t{
        None,
        Parens,
        Curly,
        Square,
        Angled,
        CurlyWithSemiColon,
        SingleColon,
    };

    struct Guard{
        Guard(const std::string &g1, const std::string& g2, std::ostream &os, bool endl_on_insides = false)
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
        Guard make_guard(GuardStyle g);

        template <typename T, typename... Ts>
        self_t &raw(T &&val, Ts &&... vals) ;
        self_t &expr(const Expr &expr);
        self_t &expr(const Expr &e, const GuardStyle gs);

        template < typename T >
        self_t &expr_array(const std::vector< T > &ops, ExprStyle style, bool wrap_in_statement = false);
        self_t &endl();
        self_t &binary_op(const BinaryOp <Expr> &binOp, const std::string &op,
                          GuardStyle gs = GuardStyle::Parens);

        self_t &wrap(GuardStyle gs);
        self_t &unwrap();
        std::stack<Guard> guards;

        Guard guard_for_expr(ExprStyle style);
    };

    const Var inner_func_arg1( "first8bytes", Type("uint64_t"));
    const Var inner_func_arg2( "second8bytes", Type("uint64_t"));
    inline static const std::array<Var,2> inner_func_args = {inner_func_arg1, inner_func_arg2};

    static constexpr const auto extract_helper_function_name = "extract_helper";
}

