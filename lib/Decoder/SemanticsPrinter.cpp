#include <circuitous/Decoder/SemanticsPrinter.hpp>
#include <circuitous/IR/Shapes.hpp>

namespace circ::decoder::semantics
{
    // init modules?
    void register_leaf_intrinsics();
    void register_selects(); //???

    void specialize_tree(); // This is VI level? --> try to get rid of selects as much as possible
    void emit_llvm(); // ??

    void print_semantics( Circuit *circ )
    {




        std::cout << "printing semantics, VIs: " << circ->attr< VerifyInstruction >().size() << std::endl;

        SelectStorage st;
        select_vis sel(&st);
        for ( auto vi : circ->attr< VerifyInstruction >() )
            sel.visit(vi);
        ExpressionPrinter ep(std::cout);

        for(auto& func : st.get_functions_for_select())
            ep.print(func);

        std::vector<FunctionDeclaration> lifted_funcs;
        for ( auto vi : circ->attr< VerifyInstruction >() )
        {
            lifted_funcs.push_back(
                get_function_for_VI( static_cast< VerifyInstruction * >( vi ), st, 0 ) );
        }

        for ( auto lf : lifted_funcs)
            ep.print(lf);
    }

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

    Operation* select_index(Select* op) { return op->operands[0]; }
    std::vector<Operation*> select_values(Select* op) { return std::vector<Operation*>(op->operands.begin() + 1, op->operands.end()); }


    std::size_t hash_select(Select* op){
        print::PrettyPrinter pp;
        return std::hash<std::string>{}( pp.Hash(select_values(op)) );
    }


    struct test_vis : Visitor<test_vis> {
        bool first = false;
        VerifyInstruction* vi;
        test_vis(  VerifyInstruction *context ) : vi( context ) { }


        std::string visit(Select *op){
            auto x = std::vector<Operation*>(op->operands.begin() + 1, op->operands.end());
            print::PrettyPrinter pp;

            return "select:: " + pp.Print( select_index(op), 0 ) + " -> " + pp.Hash( select_values(op));
        }

        std::string visit(Advice *op){
            return Visitor<test_vis>::dispatch((get_adviced_value(op, vi)));
            return "special advice";
        }

        std::string visit(Extract* op)
        {
            check(op->operands.size() == 1 ) << "extract has more than one operand";
            return Visitor<test_vis>::dispatch(op->operands[0]) + "[" + std::to_string(op->low_bit_inc) + ".." + std::to_string(op->high_bit_exc) + "]";
        }

        std::string visit(Concat* op)
        {
            std::stringstream ss;
            for(std::size_t i = 0; i < op->operands.size(); i++){
                auto  x= op->operands[i];
                ss << Visitor<test_vis>::dispatch(x);
                if(i != op->operands.size() -1)
                    ss << " ++ ";
            }
            return ss.str();
        }

        std::string visit(InputInstructionBits* op){ return "input"; }

        std::string visit(Operation *op) {
            if(!first){
                first = true;
                return Visitor<test_vis>::dispatch(op);
            }
            print::PrettyPrinter pp;
            return pp.Print(op, 0);
        }
    };

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


    std::string get_target( Operation *op , VerifyInstruction* vi)
    {
        print::PrettyPrinter pp;
        test_vis vs(vi);
        if ( isa< RegConstraint >( op ) )
            return vs.visit(op->operands[ 1 ]);
        if ( isa< WriteConstraint >( op ) )
            return pp.Print(op->operands[ 2 ], 0);
        return "unsupported: " + pp.Print( op, 0 );
    }

    std::string get_semantics( Operation *op, VerifyInstruction *vi )
    {
        test_vis vs( vi );
        print::PrettyPrinter pp;
        if ( isa< RegConstraint >( op ) )
            return vs.visit( op->operands[ 0 ] );
        if ( isa< WriteConstraint >( op ) )
            return pp.Print( op->operands[ 4 ], 0 );
        return "unsupported: " + pp.Print( op, 0 );
    }

    FunctionDeclaration get_function_for_VI( VerifyInstruction *op, SelectStorage &st,
                                             uint32_t size_of_encoding )
    {
        FunctionDeclarationBuilder fdb;
        collect::DownTree <tl::TL<RegConstraint, WriteConstraint>> down_collector;
        down_collector.Run( op );

        ExpressionPrinter ep(std::cout);
        fdb.retType(Id("void")).name("lifted_func_" + std::to_string(op->id()));
        fdb.arg_insert()
        circIR_to_llvmIR_visitor to_semIR( &st, op, std::string() );
        for(auto& constraint : down_collector.collected)
        {
            if ( isa< RegConstraint >( constraint ) )
            {
                fdb.body_insert( Assign( to_semIR.visit( constraint->operands[ 1 ] ),
                                             to_semIR.visit( constraint->operands[ 0 ] ) ) );
            }
            if ( isa< WriteConstraint >( constraint ) )
                circ::unreachable() << "memory not yet supported";


        }
        return fdb.make();
    }


    Operation* get_adviced_value( Advice *advice , Operation *VI )
    {
        auto found_constraints = false;
        AdviceConstraint* ac = nullptr;
        SubtreeCollector<AdviceConstraint> ACCollector;
        ACCollector.Run(VI);
        auto advices_in_vi = ACCollector.collected;
        for (auto user : advice->users)
        {
            if(isa<AdviceConstraint>(user) && user->operands.size() == 2 && std::find(advices_in_vi.begin(), advices_in_vi.end(), user) != advices_in_vi.end())
            {
                if(found_constraints)
                    circ::unreachable() << "advice has multiple constraints";
                found_constraints = true;
                ac = static_cast<AdviceConstraint*>(user);
            }
        }
        if(!found_constraints)
            circ::unreachable() << "advice without constraint";

        return ac->operands[0] == advice ? ac->operands[1] : ac->operands[0]; // return other value
    }

    std::size_t SelectStorage::hash_select_targets( Select *sel )
    {
        print::PrettyPrinter pp;
        return std::hash< std::string > {}( pp.Hash( select_values( sel ) ) );
    }

    void SelectStorage::register_select( Select *sel )
    {
        FunctionDeclarationBuilder fdb;
        print::PrettyPrinter pp;
        auto values_hash = hash_select(sel);

        fdb.name("select_" + std::to_string(values_hash));
        //TODO get type of
        fdb.retType(Id("Register"));
        Var select_index = Var("index", "int:" + std::to_string(sel->bits));
        fdb.arg_insert(VarDecl(select_index));
        for(std::size_t i = 0; i < select_values( sel ).size(); i++ )
        {
            // TODO replace with to string visitor
            auto res = pp.Print( sel->operands[ i ], 0 );
            //TODO turn ifelse into if
            auto if_expr =  If( Equal( select_index, Uint64(i) ), Return( Id( res ) ));
            fdb.body_insert( if_expr );
//                ss << "selector == " << std::to_string(i) <<  << "\n";
        }

        selects.insert( std::make_pair( values_hash, fdb.make() ) );
    }

    std::vector< decoder::FunctionDeclaration > SelectStorage::get_functions_for_select()
    {
        std::vector< decoder::FunctionDeclaration > funcs;
        print::PrettyPrinter pp;
        std::stringstream sl;


        // This needs to be only done once per key
        // Still need to verify that all keys have same size, maybe during registration
        // Need to safe the functions somewhere, maybe during registration?
        for(auto & k : selects)
        {
//            FunctionDeclarationBuilder fdb;
//            fdb.name("select_" + std::to_string(k.first));
//            //TODO get type of
//            fdb.retType(Id("Register"));
//            Var select_index = Var("index", "int:" + std::to_string(k.second->bits));
//            fdb.arg_insert(VarDecl(select_index));
//            for(std::size_t i = 0; i < select_values(k.second ).size(); i++ )
//            {
//                // TODO replace with to string visitor
//                auto res = pp.Print( k.second->operands[ i ], 0 );
//                //TODO turn ifelse into if
//                auto if_expr =  IfElse( Equal( select_index, Uint64(i) ), Return( Id( res ) ), Id("<Else>") );
//                fdb.body_insert( Statement( ) );
////                ss << "selector == " << std::to_string(i) <<  << "\n";
//            }
            funcs.push_back(k.second);

//            std::stringstream ss;
//            ss << "Register select_" << std::to_string(k.first) << "(selector:" << std::to_string(select_values(k.second).size()) << ") {\n";
//            for(std::size_t i = 0; i < select_values(k.second).size(); i++){
//                ss << "selector == " << std::to_string(i) << pp.Print(k.second->operands[i],0) << "\n";
//            }
//            ss << "}\n";
//            sl << ss.str();
        }
//        return sl.str();


        return funcs;
    }
    decoder::FunctionCall SelectStorage::get_specialization( circ::Select *sel, Expr index )
    {
        auto func = selects.at( hash_select(sel));
        return decoder::FunctionCall( func.function_name, { index } );
    }
};
