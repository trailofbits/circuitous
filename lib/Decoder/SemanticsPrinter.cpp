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

    std::string sem_op_printer(Select*op){

        return "found select";
    }

    std::string sem_op_printer(Operation*op){
        print::PrettyPrinter pp;
        if(isa<Select>(op)){
            ToSemanticsIRVisitor vs;
            vs.visit(op);
            return "found select";
        }
//            return sem_op_printer(op);
        return pp.Print(op, 0);
    }

    std::string get_target( Operation *op )
    {
        print::PrettyPrinter pp;
        if ( isa< RegConstraint >( op ) )
            return sem_op_printer(op->operands[ 1 ]);
        if ( isa< WriteConstraint >( op ) )
            return pp.Print(op->operands[ 2 ], 0);
        return "unsupported: " + pp.Print( op, 0 );
    }

    void print_semantics( VerifyInstruction *op )
    {
        collect::DownTree <tl::TL<RegConstraint, WriteConstraint>> down_collector;
        down_collector.Run( op );

        for(auto& constraint : down_collector.collected)
            std::cout << "[" << get_target(constraint) << "]" << " = " << get_semantics(constraint) << std::endl;
    }

    std::string get_semantics( Operation *op ) {
        print::PrettyPrinter pp;
        if ( isa< RegConstraint >( op ) )
            return pp.Print(op->operands[ 0 ], 0);
        if ( isa< WriteConstraint >( op ) )
            return pp.Print(op->operands[ 4 ], 0);
        return "unsupported: " + pp.Print( op, 0 );
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
