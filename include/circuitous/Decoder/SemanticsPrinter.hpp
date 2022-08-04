#include <circuitous/IR/Circuit.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>
#include <circuitous/IR/Visitors.hpp>

namespace circ::semantics
{
    void print_semantics( Circuit *op );
    void print_semantics( VerifyInstruction *op );

    // Templated for constraints?
    std::string get_target( Operation *op );

    std::string get_semantics( Operation *op, VerifyInstruction *vi );

    Operation* get_adviced_value( Advice *advice , Operation *VI );

    struct ToSemanticsIRVisitor : NonDefaultingVisitor<ToSemanticsIRVisitor>
    {
        void visit(Select* op){
            std::cout << "omg -=----------" << std::endl;
        }

        void visit(Operation* op){
            std::cout << "sad:((((((((" << std::endl;
        }

    };
}
