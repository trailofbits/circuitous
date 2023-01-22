#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Decoder/SemanticsPrinter.hpp>
#include <circuitous/IR/Shapes.hpp>

namespace circ::decoder::semantics
{
    // init modules?
    void register_leaf_intrinsics();
//    void register_selects();

    void specialize_tree(); // This is VI level? --> try to get rid of selects as much as possible
    void emit_llvm(); // ??

//    void print_semantics( Circuit *circ )
//    {
//        std::cout << "printing semantics, VIs: " << circ->attr< VerifyInstruction >().size()
//                  << std::endl;
//
//        SelectStorage st;
//        select_vis sel( &st );
//        for ( auto vi : circ->attr< VerifyInstruction >() )
//            sel.visit( vi );
//        ExpressionPrinter ep( std::cout );
//
//        for ( auto &func : st.get_functions_for_select() )
//            ep.print( func );
//
//        std::vector< FunctionDeclaration > lifted_funcs;
//        for ( auto vi : circ->attr< VerifyInstruction >() )
//        {
//            lifted_funxcs.push_back(
//                get_function_for_VI( static_cast< VerifyInstruction * >( vi ), st, 0 ) );
//        }
//
//        for ( auto lf : lifted_funcs )
//            ep.print( lf );
//    }

    /*
     * What kind of visitors do we need?
     * Select and Advice will remain across all target languages
     * so we can
     *
     * we can convert to semIR -> print textual version of semIR?
     *
     * conclusie:
     *  we gaan 1 grote sem visitor hebben
     *  select resolve struct maken
     *  die runt convert_vistior op target / sem plek
     *      convert_vistor neemt select resovle struct:
     *          converts alle nodes naar SemIR equivelant
     *      Converted struct word per IR in een functie gedouwede
     *      emission word gedaan niet naar strings maar naar vectors
     *          voor target = sem
     *          hooks for registereren van variables + ander ezooi
     *      we exposen iterators naar vectors
     */

    // should be used to fill out struct?
    struct operation_to_vistor_setup_visitor : UniqueVisitor<operation_to_vistor_setup_visitor>
    {
        void visit( Operation *op )
        {
            //check if even should be added in the first place (by checking enum regs)
            auto key = op->name();
            // assumes all binary operators have two operands
            if ( op_to_method.contains( key ) )
                return;

            auto name = "visit_" + op->name();
            FunctionDeclarationBuilder fdb;
            fdb.name( name );

            // TODO these T's should come from struct definition
            // We assume all operations can be reduced to two binaries
            // Maybe these T's should be auto?
            if ( op->operands_size() >= 2 )
            {
                fdb.arg_insert( VarDecl( Var( "lhs", "T" ) ) );
                fdb.arg_insert( VarDecl( Var( "rhs", "T" ) ) );
            }
            if ( op->operands_size() == 1 )
                fdb.arg_insert( VarDecl( "value", "T" ) );

            op_to_method.insert( { key, fdb.make() } );
        }

        //TODO(sebas): fix this
//        std::optional< FunctionDeclaration > get_by_operation( Operation *op )
//        {
//            auto key = op->name();
//            if ( op_to_method.contains( key ) )
//                return std::optional< FunctionDeclaration >( op_to_method[ key ] );
//            return std::nullopt;
//        }

        std::map< std::string, FunctionDeclaration > op_to_method;
    };

//    struct test_vis : Visitor<test_vis> {
//        bool first = false;
//        VerifyInstruction* vi;
//        test_vis(  VerifyInstruction *context ) : vi( context ) { }
//
//
//        std::string visit(Select *op){
//            //TODO(sebas): FIX this gap
////            auto x = std::vector<Operation*>(op->operands.begin() + 1, op->operands.end());
//            print::PrettyPrinter pp;
//
////            return "select:: " + pp.Print( op->operand(), 0 ) + " -> " + pp.Hash( select_values(op));
//        }
//
//        std::string visit(Advice *op){
//            return Visitor<test_vis>::dispatch((get_adviced_value(op, vi)));
//            return "special advice";
//        }
//
//        std::string visit(Extract* op)
//        {
//            check(op->operands_size() == 1 ) << "extract has more than one operand";
//            return Visitor<test_vis>::dispatch(op->operand(0)) + "[" + std::to_string(op->low_bit_inc) + ".." + std::to_string(op->high_bit_exc) + "]";
//        }
//
//        std::string visit(Concat* op)
//        {
//            std::stringstream ss;
//            for(std::size_t i = 0; i < op->operands_size(); i++){
//                auto  x= op->operand(i);
//                ss << Visitor<test_vis>::dispatch(x);
//                if(i != op->operands_size() -1)
//                    ss << " ++ ";
//            }
//            return ss.str();
//        }
//
//        std::string visit(InputInstructionBits* op){ return "input"; }
//
//        std::string visit(Operation *op) {
//            if(!first){
//                first = true;
//                return Visitor<test_vis>::dispatch(op);
//            }
//            print::PrettyPrinter pp;
//            return pp.Print(op, 0);
//        }
//    };

//    std::string sem_op_printer(Select*op){
//        return "found select";
//    }
//
//    std::string sem_op_printer(Operation*op ,){
////        print::PrettyPrinter pp;
////        if(isa<Select>(op)){
//            return vs.visit(op);
////            return "found select";
////        };
////        return pp.Print(op, 0);
//    }
//
//
//    std::string get_target( Operation *op , VerifyInstruction* vi)
//    {
//        print::PrettyPrinter pp;
//        test_vis vs(vi);
//        if ( isa< RegConstraint >( op ) )
//            return vs.visit(op->operand( 1 ));
//        if ( isa< WriteConstraint >( op ) )
//            return pp.Print(op->operand( 2 ), 0);
//        return "unsupported: " + pp.Print( op, 0 );
//    }
//
//    std::string get_semantics( Operation *op, VerifyInstruction *vi )
//    {
//        test_vis vs( vi );
//        print::PrettyPrinter pp;
//        if ( isa< RegConstraint >( op ) )
//            return vs.visit( op->operand( 0 ) );
//        if ( isa< WriteConstraint >( op ) )
//            return pp.Print( op->operand( 4 ), 0 );
//        return "unsupported: " + pp.Print( op, 0 );
//    }

//    FunctionDeclaration get_function_for_VI( VerifyInstruction *op, SelectStorage &st,
//                                             uint32_t size_of_encoding )
//    {
//        FunctionDeclarationBuilder fdb;
//        collect::DownTree <tl::TL<RegConstraint, WriteConstraint>> down_collector;
//        down_collector.Run( op );
//
//        ExpressionPrinter ep(std::cout);
//        auto lifted_func_name = "lifted_func_" + std::to_string(op->id());
//        fdb.retType(Id("llvm::Function*")).name(lifted_func_name );
//        /*
//         * TODO: Decide what the length of input bits should be.
//         * We have two options
//         *  1. Have it equal to the size of the instructions --> would give trouble once VI get's varaible length
//         *  2. HAve it the size of the architecture
//         */
//
//        fdb.arg_insert(Var("input_bits", "std::bitset<" + std::to_string(MAX_ENCODING_LENGTH) + ">"));
//        Var llvm_func("func");
//        auto getVoidTy = decoder::FunctionCall("Type::getVoidTy", { llvm_context } );
//        auto getFuncTy = decoder::FunctionCall("FunctionType::get", { getVoidTy } );
//
//        fdb.body_insert_statement(
//            Assign( VarDecl( llvm_func ),
//                    decoder::FunctionCall( "Function::Create",
//                                           { getFuncTy, Id( "Function::ExternalLinkage" ),
//                                             Id( lifted_func_name ) } ) ) );
//
//        Var bb("bb");
//        fdb.body_insert_statement( Assign(
//            VarDecl( bb ), FunctionCall( "BasicBlock::Create", { llvm_context, Id( "\"\""), llvm_func } ) ) );
//        fdb.body_insert_statement(FunctionCall(irb.name + ".SetInsertPoint", { bb } ));
//
//        //TODO module must be context?
////        circIR_to_llvmIR_visitor to_semIR( &st, op, "irb", Var("llvm_context") );
////        for(auto& constraint : down_collector.collected)
////        {
////            if ( isa< RegConstraint >( constraint ) )
////            {
////                fdb.body_insert( Assign( to_semIR.visit( constraint->operands[ 1 ] ),
////                                             to_semIR.visit( constraint->operands[ 0 ] ) ) );
////            }
////            if ( isa< WriteConstraint >( constraint ) )
////                circ::unreachable() << "memory not yet supported";
////
////
////        }
//        fdb.body_insert(Return(llvm_func));
//        return fdb.make();
//    }


//    Operation* get_adviced_value( Advice *advice , Operation *VI )
//    {
//        auto found_constraints = false;
//        AdviceConstraint* ac = nullptr;
//        SubtreeCollector<AdviceConstraint> ACCollector;
//        ACCollector.Run(VI);
//        auto advices_in_vi = ACCollector.collected;
//        for (auto user : advice->users())
//        {
//            if(isa<AdviceConstraint>(user) && user->operands_size() == 2 && std::find(advices_in_vi.begin(), advices_in_vi.end(), user) != advices_in_vi.end())
//            {
//                if(found_constraints)
//                    circ::unreachable() << "advice has multiple constraints";
//                found_constraints = true;
//                ac = static_cast<AdviceConstraint*>(user);
//            }
//        }
//        if(!found_constraints)
//            circ::unreachable() << "advice without constraint";
//
//        return ac->operand(0) == advice ? ac->operand(1) : ac->operand(0); // return other value
//    }
//    Expr emit_llvm_context()
//    {
//        StatementBlock statements;
//        statements.push_back(Statement(Assign(llvm_context, Id("std::make_unique<Module>(\"circ_sem_mod\", " + llvm_context.name + ")"))));
//        statements.push_back(Statement(Assign(irb, FunctionCall(irb.type, { llvm_context })) ));
//
//        return statements;
//    }


};
