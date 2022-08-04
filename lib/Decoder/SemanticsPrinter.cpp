#include <circuitous/Decoder/SemanticsPrinter.hpp>
#include <circuitous/IR/Shapes.hpp>

namespace circ::semantics
{
    void print_semantics( Circuit *circ )
    {
        std::cout << "printing semantics, VIs: " << circ->attr< VerifyInstruction >().size() << std::endl;
        for ( auto vi : circ->attr< VerifyInstruction >() )
        {
            print_semantics(static_cast<VerifyInstruction*>(vi));
        }
    }

    struct test_vis : Visitor<test_vis> {

        bool first = false;
        VerifyInstruction* vi;
        test_vis(  VerifyInstruction *context ) : vi( context ) { }
        std::string visit(Select *op){
            return "found select with: " + Visitor<test_vis>::dispatch(op->operands[0]);
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

    void print_semantics( VerifyInstruction *op )
    {
        collect::DownTree <tl::TL<RegConstraint, WriteConstraint>> down_collector;
        down_collector.Run( op );

        for(auto& constraint : down_collector.collected)
            std::cout << "[" << get_target(constraint, op) << "]" << " = " << get_semantics(constraint, op) << std::endl;
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
};
