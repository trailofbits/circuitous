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
        auto g = make_guard(gs);
        expr(binOp.lhs()).raw(" ", op, " ").expr(binOp.rhs());

        return *this;
    }

    template < typename T >
    ExpressionPrinter&
    ExpressionPrinter::expr_array(const std::vector< T > &ops, const ExprStyle style) {
        auto g = guard_for_expr(style);

        for (std::size_t i = 0; i < ops.size(); i++) {
            print( ops[ i ] );

            if ( i != ops.size() - 1 ) {
                switch (style) {
                    case ExprStyle::FuncArgs: raw(", "); break;
                    // no ; required in funcbody as for example if statements don't end with them
                    case ExprStyle::FuncBody: endl() ; break;
                    case ExprStyle::EnumBody: raw(", ").endl(); break;
                    case ExprStyle::TemplateParams: raw(", "); break;
                    case ExprStyle::StructDecl: endl(); break;
                    case ExprStyle::StructMethods: endl(); break;
                    case ExprStyle::StructVars: endl(); break;
                    case ExprStyle::StructDerivations: raw(", "); break;
                }
            }
        }

        return *this;
    }

    ExpressionPrinter &ExpressionPrinter::expr(const Expr &e) {
        if(e.op->valueless_by_exception()){
            circ::unreachable() << "valueless by exception";
        }
        std::visit( overloaded{
                [&](const Plus &arg) {binary_op( arg, "+" );},
                [&](const Mul &arg) {binary_op( arg, "*" );},
                [&](const Id &arg) { raw(arg);},
                [&](const Type &arg) {
                    if(arg.is_constexpr) raw("constexpr ");
                    if(arg.is_const) raw("const ");
                    if(arg.is_static) raw("static ");
                    expr( arg.name );
                    if( arg.template_parameters.size() != 0 )
                        expr_array( arg.template_parameters, ExprStyle::TemplateParams );
                },
                [&](const Int &arg) { raw(arg.value);},
                [&](const Uint64 &arg) { raw("0b", std::bitset< 64 >( arg.value ));},
                [&](const Expr &arg) { circ::unreachable() << "No plain expr allowed" ; },
                [&](const Empty &arg) {},
                [&](const Var &arg) { raw(arg.name); },
                [&](const VarDecl &arg) { raw(arg.value().type, " ", arg.value().name); },
                [&]( const EnumValue &arg) {
//                            raw(arg.e->enum_name, "::", arg.e->index(arg.index));
                        },
                [&]( const Enum &arg ) { raw( arg.enum_name ); },
                [&]( const EnumDecl &arg )
                {
//                    raw( "enum class ", arg.value().enum_name )
//                        .expr_array< Id >( arg.value().names, ExprStyle::EnumBody )
//                        .endl();
                },
                [&](const IndexVar &arg) { expr(arg.lhs()).expr(arg.rhs(), GuardStyle::Square); },
                [&](const Dereference &arg) { raw("(*").expr(arg.value()).raw(")"); }, // TODO(sebas): guard styles don't take results of raw as a direct argument
                [&](const Statement &arg) {
                    expr( arg.value()).raw(";").endl();
                },
                [&](const Return &arg) {
                    raw("return ").expr( arg.value() ).raw(";");
                },
                [&](const CastToUint64 &arg) {
                    raw("static_cast<uint64_t>").expr( arg.value(), GuardStyle::Parens);
                },
                [&](const BitwiseNegate &arg) {
                    raw("~").expr( *arg.value().op, GuardStyle::Parens);
                },
                [&](const BitwiseOr &arg) {binary_op( arg, "|");},
                [&](const BitwiseXor &arg) {binary_op( arg, "^");},
                [&](const BitwiseAnd &arg) {binary_op( arg, "&");},
                [&](const And &arg) {binary_op( arg, "&&");},
                [&](const Shfl &arg) {binary_op( arg, "<<");},

                [&](const Equal &arg) {binary_op( arg, "==");},
                [&](const Assign &arg) {binary_op( arg, "=", GuardStyle::None).raw(';');},
                [&](const StatementBlock &arg) {
                    for (auto &e: arg) {
                        expr( e );
                    }
                },
                [&](const If &arg) {
                    raw("if").expr( arg.cond(), GuardStyle::Parens )
                        .expr( arg.ifBody(), GuardStyle::Curly).endl();
                },
                [&](const IfElse &arg) {
                    raw("if").expr( arg.cond(), GuardStyle::Parens )
                    .expr( arg.ifBody(), GuardStyle::Curly)
                    .raw(" else ")
                    .expr( arg.elseBody(), GuardStyle::Curly).endl();
                },
                [&](const FunctionCall &arg) {
                    if(arg.template_parameters.empty())
                        raw( arg.function_name ).expr_array( arg.args, ExprStyle::FuncArgs);
                    else
                        raw( arg.function_name )
                            .expr_array( arg.template_parameters, ExprStyle::TemplateParams )
                            .expr_array( arg.args, ExprStyle::FuncArgs );
                },
                [&](const FunctionDeclaration &arg) {
                    expr( arg.retType).raw(" ", arg.function_name )
                    .expr_array( arg.args, ExprStyle::FuncArgs).endl()
                    .expr_array( arg.body, ExprStyle::FuncBody).endl().endl();
                },
                [&](const Struct &arg) {
                    raw( "struct " ).expr(arg.name);
//                                .expr_array(arg.derived_from, ExprStyle::StructDerivations)
                    auto g = make_guard(GuardStyle::CurlyWithSemiColon);
                    expr_array(arg.methods, ExprStyle::StructMethods)
                        .expr_array(arg.default_init_variables, ExprStyle::StructVars)
                        .expr_array(arg.assignment_init_variables, ExprStyle::StructVars);
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

    Guard ExpressionPrinter::make_guard(GuardStyle g) {
        switch(g){
            case GuardStyle::None :return {"", "", os};
            case GuardStyle::Square :return {"[", "]", os};
            case GuardStyle::Parens :return {"  (", ") ", os};
            case GuardStyle::Curly :return {"{", "}", os, true};
            case GuardStyle::Angled : return {"<", ">", os};
            case GuardStyle::SingleColon : return {":", "", os, true};
            case GuardStyle::CurlyWithSemiColon : return { "{", "};", os, true};
            default: circ::unreachable() << "invalid guard style";
        }
    }

    ExpressionPrinter::self_t &ExpressionPrinter::expr(const Expr &e, const GuardStyle gs) {
        auto g = make_guard(gs); // needs to be bound to a variable to force proper scoping
        expr(e);
        return *this;
    }

    ExpressionPrinter::self_t &ExpressionPrinter::unwrap()
    {
        guards.pop();
        return *this;
    }

    ExpressionPrinter::self_t &ExpressionPrinter::wrap(GuardStyle style)
    {
        guards.push( make_guard(style) );
        return *this;
    }

    Guard ExpressionPrinter::guard_for_expr( ExprStyle style )
    {
        switch ( style )
        {
            case ExprStyle::FuncArgs: return make_guard( GuardStyle::Parens );
            case ExprStyle::FuncBody: return make_guard( GuardStyle::Curly );
            case ExprStyle::EnumBody: return make_guard( GuardStyle::Curly );
            case ExprStyle::TemplateParams: return make_guard( GuardStyle::Angled );
            case ExprStyle::StructDecl: return make_guard( GuardStyle::CurlyWithSemiColon );
            case ExprStyle::StructMethods: return make_guard( GuardStyle::None );
            case ExprStyle::StructVars: return make_guard( GuardStyle::None );
            case ExprStyle::StructDerivations: return make_guard( GuardStyle::SingleColon );
        }
    }

    const Expr &If::cond() const
    {
        return this->ops[ 0 ];
    }

    const Expr &If::ifBody() const
    {
        return this->ops[ 1 ];
    }

    const Expr &IfElse::cond() const
    {
        return this->ops[ 0 ];
    }

    const Expr &IfElse::ifBody() const
    {
        return this->ops[ 1 ];
    }

    const Expr &IfElse::elseBody() const
    {
        return this->ops[ 2 ];
    }

    FunctionCall::FunctionCall( const Var &var, const Id &func, std::vector< Expr > args ) :
        args( args )
    {
        if ( !var.is_struct )
        {
            function_name = var.name;
            return;
        }
        auto delim = var.is_pointer ? "->" : ".";
        this->function_name = Id( var.name + delim + func );
    }

    FunctionCall::FunctionCall( const Id &functionName, std::vector< Expr > args ) :
        function_name( functionName ), args( args )
    {
    }

    FunctionCall::FunctionCall( const Id &functionName, std::vector< Expr > args,
                                std::vector< Expr > template_parameters ) :
        function_name( functionName ),
        args( args ), template_parameters( template_parameters )
    {
    }

    FunctionDeclaration::FunctionDeclaration( const Type &retType, const Id &functionName,
                                              const std::vector< VarDecl > &args,
                                              const StatementBlock &body ) :
        retType( retType ),
        function_name( functionName ), args( args ), body( body )
    {
    }

    FunctionDeclarationBuilder::self_t &
    FunctionDeclarationBuilder::body_insert( const Expr &expr )
    {
        _body.emplace_back( expr );
        return *this;
    }

    FunctionDeclarationBuilder::self_t &
    FunctionDeclarationBuilder::body_insert_statement( const Expr &expr )
    {
        _body.emplace_back( Statement( expr ) );
        return *this;
    }

    FunctionDeclarationBuilder::self_t &
    FunctionDeclarationBuilder::body( const StatementBlock &b )
    {
        _body = b;
        return *this;
    }

    FunctionDeclaration FunctionDeclarationBuilder::make()
    {
        return FunctionDeclaration( _retType, _function_name, _args, _body );
    }

    FunctionDeclarationBuilder::self_t &FunctionDeclarationBuilder::retType( const Type &retType )
    {
        _retType = retType;
        return *this;
    }

    FunctionDeclarationBuilder::self_t &FunctionDeclarationBuilder::name( const Id &name )
    {
        _function_name = name;
        return *this;
    }

    FunctionDeclarationBuilder::self_t &
    FunctionDeclarationBuilder::arg_insert( const VarDecl &args )
    {
        _args.push_back( args );
        return *this;
    }

    Struct::Struct( const int templateSize ) :
        template_size( templateSize ), templatized( templateSize == 0 ), derived_from( {} )
    {
        for ( int i = 0; i < templateSize; i++ )
        {
            template_typenames.push_back( Id( "T" + std::to_string( i ) ) );
        }
    }

    Struct::Struct( const int templateSize, const std::vector< Expr > &derivedFrom ) :
        template_size( templateSize ), templatized( templateSize == 0 ),
        derived_from( derivedFrom )
    {
        for ( int i = 0; i < templateSize; i++ )
        {
            template_typenames.push_back( Id( "T" + std::to_string( i ) ) );
        }
    }

    Type::Type( Id name ) :
        name(name)
    {
    }

    Type::Type( Id name, const std::vector< Expr > &templateParameters ) :
        name(name),
        template_parameters( templateParameters )
    {
    }

    Type::Type() { }
};
