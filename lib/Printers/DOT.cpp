/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Printers/Dot.hpp>
#include <circuitous/Diff/Diff.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

#include <ostream>
#include <unordered_map>


CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <circuitous/IR/Shapes.hpp>
#include <circuitous/Diff/SemanticsTainter.hpp>

CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::print
{
    std::string color_to_dot(Color c){
        switch(c){
            case Color::None: return "";
            case Color::RedWhite: return "fillcolor=red;fontcolor=white;style=filled;";
            case Color::YellowBlack: return "fillcolor=yellow;fontcolor=black;style=filled;";
            case Color::GreenBlack: return "fillcolor=green;fontcolor=black;style=filled;";
            case Color::BlueYellow: return "fillcolor=blue;fontcolor=yellow;style=filled;";
            case Color::VioletWhite: return "fillcolor=violet;fontcolor=white;style=filled;";
            case Color::GrayWhite: return "fillcolor=gray;fontcolor=white;style=filled;";
            case Color::OrangeBlack: return "fillcolor=orange;fontcolor=black;style=filled;";
        }
    }

    HighlightColorer::opt_name
    HighlightColorer::name_for_op(Operation *op) {
        auto is_prefix_to_op_name = [&](const std::string &lhs) {
            if(lhs.size() > op->name().size()) {
                return false;
            }
            return std::equal( lhs.begin(), lhs.end(), op->name().begin(),
                               [](char a, char b) {
                                   return std::tolower( a ) == std::tolower( b );
                               } );
        };
        auto highlight = std::find_if( highlight_nodes.begin(), highlight_nodes.end(),
                                       is_prefix_to_op_name );
        if ( highlight != highlight_nodes.end()) {
            return opt_name{ *highlight};
        }
        return std::nullopt;
    }

    HighlightColorer::HighlightColorer(const std::vector< std::string > &highlight_nodes)
            : highlight_nodes( highlight_nodes) {
        for(auto& hl: highlight_nodes){
            node_to_color_map[hl] = color_counter % colors.size();
            color_counter++;
        }
    }

    Color HighlightColorer::operator()(Operation *op) {
        if(auto opt_highlight_name = name_for_op( op ))
            return color_defaults[node_to_color_map[opt_highlight_name.value()]];

        return Color::None;
    }

    namespace
    {

        static const char *const kBeginDOTNode =
            "[label=<<TABLE cellpadding=\"0\" cellspacing=\"0\" border=\"1\"><TR>";
        static const char *const kEndDOTNode = "</TR></TABLE>>];\n";

        class DOTPrinter : public UniqueVisitor<DOTPrinter> {
            using value_map_t = std::unordered_map<Operation *, std::string>;
            public:
            explicit DOTPrinter(std::ostream &os_, const value_map_t &vals)
                : os(os_), node_values(vals) {}

            void PrintOperands(Operation *op) {
                if (!op->operands.empty()) {
                    os << "</TR><TR>";
                    for (auto sub_op : op->operands) {
                        os << "<TD port=\"s";
                        os << sub_op->id();
                        os << "\"> &nbsp; </TD>";
                    }
                }
                os << kEndDOTNode;
                for (auto sub_op : op->operands) {
                    os << 'o' << op->id() << ":s" << sub_op->id()
                       << " -> o" << sub_op->id() << ":id;\n";
                }
            }

            void PrintNodeName(Operation *op) {
                os << "o" << op->id() << " " << kBeginDOTNode << "<TD port=\"id\"";
                if (!op->operands.empty()) {
                    os << " colspan=\"" << op->operands.size() << "\"";
                }
                os << ">" << op->name();
                if (node_values.count(op)) {
                    os << " = " << node_values.find(op)->second;
                }
                os << "</TD>";
            }

            void visit(Operation *op) {
                op->traverse(*this);
                PrintNodeName(op);
                PrintOperands(op);
            }

            void visit(Circuit *op) {
                os << "digraph {\n"
                    << "node [shape=plain];\n";
                op->traverse(*this);
                PrintNodeName(op);
                PrintOperands(op);
                os << "}\n";
            }

            private:
            std::ostream &os;
            const value_map_t &node_values;
        };

    }  // namespace


    Color sem_taint_coloring(Operation *op) {
        using namespace inspect;
        if ( !op->has_meta( SemanticsTainter::meta_key ))
            return Color::None;

        switch (read_semantics(op)) {
            case sem_taint::None: return Color::None;
            case sem_taint::State: return Color::GreenBlack;
            case sem_taint::Decode: return  Color::BlueYellow;
            case sem_taint::Semantics: return Color::OrangeBlack;
            case sem_taint::Config: return Color::RedWhite;
            case sem_taint::Delete: return Color::GrayWhite;
        }
    }

    Color diff_coloring(Operation *op) {
        using namespace inspect;

        if ( !op->has_meta( SemanticsTainter::meta_key ))
            return Color::None;

        auto value = diffmarker_read(op);
        if(value == DiffMarker::Overlapping){
            return Color::GreenBlack;
        } else if(value == DiffMarker::Left){
            return Color::BlueYellow;
        }
        else if(value == DiffMarker::Right){
            return Color::RedWhite;
        }
        else if(value == DiffMarker::None){
            return Color::None;
        }

        circ::unreachable() << "could not read diff_marker properly";
    }
} // namespace circ

namespace circ::dot
{
    template<typename ColorFunc>
    struct Printer : UniqueVisitor<Printer<ColorFunc>> {
        using value_map_t = std::unordered_map<Operation *, std::string>;

        explicit Printer(std::ostream &os_, const value_map_t &vals, ColorFunc color_op)
            : os(os_), node_values(vals), color_op(color_op) { }

        std::string Operand(Operation *of, std::size_t i) {
            return NodeID(of) + ':' + NodeID(of) + std::to_string(i);
        }

        void Edge(Operation *from, Operation *to, std::size_t i) {
            os << Operand(from, i)
                << " -> "
                << NodeID(to)
                << ";\n";
        }

        std::string NodeID(Operation *op) {
            return "v" + std::to_string(op->id()) + "v";
        }

        std::string AsID(const std::string &what) {
            return "<" + what + ">";
        }

        void Node(Operation *op) {
            os << NodeID(op) << "[";

            os << color_to_dot(color_op(op));

            os << "label = \" { " << AsID(NodeID(op)) << " " << op->name();
            if (node_values.count(op)) {
                os << " " << node_values.find(op)->second << " ";
            }

            if (op->operands.size() == 0) {
                os << " }" << '"' << "];\n";
                return;
            }

            os << "| {";
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                os << AsID(NodeID(op) + std::to_string(i));
                if (i != op->operands.size() - 1) {
                    os << " | ";
                }
            }
            os << " }}" << '"' << "];\n";
        }

        void Init() {
            os << "digraph {" << std::endl;
            os << "node [shape=record];";
        }

        void visit(Operation *op) {
            op->traverse(*this);
            Node(op);
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                Edge(op, op->operands[i], i);
            }
        }


        struct DownTreeRunner{
            virtual ~DownTreeRunner() {};
            virtual void Execute(Operation* op) = 0;
            void Run(Operation *op) {
                Execute(op);
                for (auto o : op->operands) {
                    Run(o);
                }
            };
        };

        struct UpTreeRunner{
            virtual ~UpTreeRunner() {};
            virtual void Execute(Operation* op) = 0;
            void Run(Operation* op) {
                Execute( op );
                for (auto o: op->users) {
                    Run( o );
                }
            }
        };

        /*
         * Accepts a typelist, and runs up from all nodes starting inside TL
         */
        template<typename TL>
        struct UpTreeRunnerTyped{
            Circuit* circuit;


            UpTreeRunnerTyped(Circuit *circuit) : circuit( circuit ) {}

            virtual ~UpTreeRunnerTyped() {};
            virtual void Execute(Operation* op) = 0;
            void Run_(Operation* op) {
                Execute( op );
                for (auto o: op->users) {
                    Run_( o );
                }
            }

            void Run(Operation *op) {
                collect::DownTree<TL> down_collector;
                down_collector.Run(op);
                std::cout << "collected: " << down_collector.collected.size() << std::endl;
                for(auto& o : down_collector.collected) {
                    Run_( o );
                }
            }
        };

//
//
//        struct CompleteOrMarker :DownTreeRunner {
//            void Execute(Operation* op) {
//                using cmp_target = std::pair<std::size_t, Or*>;
//                std::vector<cmp_target> cmp_targets;
//                size_t target_size = 0;
//                for(auto& o : op->operands){
//                    if(isa<Or>(o) && o->operands.size() == 1){
//
//                        std::cout << "or && 1 " << std::endl;
//                        auto op_or = dynamic_cast<Or*>(o);
//                        if(isa<Icmp_eq>(op_or->operands[0])){
//                            auto icmp = op_or->operands[0];
//                            if(isa<Advice>(icmp->operands[0]) && isa<Constant>(icmp->operands[1])){
//                                auto const_val= dynamic_cast<Constant*>(icmp->operands[1]);
//                                if(target_size == 0){
//                                    target_size = const_val->bits.size();
//                                }
//                                // shouldn't crash the program
//                                // check(target_size == const_val->size) << "Multiple different sizes";
//
//                                std::cout << "trying to add something" << std::endl;
//                                if(target_size != const_val->bits.size())
//                                    return;
//                                cmp_targets.push_back({stoi(const_val->bits, 0,2 ), op_or});
//                            }
//                        }
//                    }
//                }
//
//                auto is_power_of_2 = [](std::size_t v){ return v >0 && (v & (v - 1)) == 0;};
//                if(is_power_of_2(cmp_targets.size())){
//                    std::cout << "found power of two shit" << std::endl;
//                    for(std::size_t i = 0; i < cmp_targets.size(); i++){
//                        if(std::find_if(cmp_targets.begin(), cmp_targets.end(), [&](cmp_target ct){
//                            return ct.first == i;
//                        }) == cmp_targets.end()){
//                            //there is a whole, don't mark them as deletable
//                        };
//                    }
//                    for(auto& ct : cmp_targets){
//                        ct.second->set_meta<true>("inspect_delete", "true");
//                    }
//                }
//            }
//        };
//
//        struct DownTreeIdentityMarker :DownTreeRunner {
//            void Execute(Operation* op){
//                if(isa<RegConstraint>(op)){
//                    if (isa<InputRegister>(op->operands[0]) && isa<OutputRegister>(op->operands[1])){
//                        auto inReg = dynamic_cast<InputRegister*>(op->operands[0]);
//                        auto outReg = dynamic_cast<OutputRegister *>(op->operands[1]);
//                        if(inReg->reg_name == outReg->reg_name){
//                            op->set_meta<true>("inspect_delete", "true");
//                        }
//                    }
//                    else{
//                        for(auto o : op->operands){
//                            std::cout << "unlisted child " << o->name() << std::endl;
//                        }
//                    }
//                }
//            }
//        };
//
//        struct DownTreeIdentityRemover {
//            bool Run(Operation *op) {
//                if(op->has_meta("inspect_delete")){
//                    for(auto& p : op->users){
//                        std::cout << "Deleting: " << op->name() << std::endl;
//                        op->remove_use(p);
//                        return true;
//                    }
//                }
//                for (auto o : op->operands) {
//                    if(Run(o))
//                        return Run(op); // restart after something has been deleted
//                }
//                return false;
//            }
//        };
//
//        template<typename TL>
//        struct RemoveCurrentSubtreeFromContext : UpTreeRunnerTyped<TL>{
//            using UpTreeRunnerTyped<TL>::UpTreeRunnerTyped;
//
//            void Execute(Operation* op){
//                std::cout << "for " << op->name() << std::endl;
//                for(auto& user : op->users){
//                    std::cout << "users " << user->name() << std::endl;
//                    std::cout << "users " << user->name() << std::endl;
//                    if(isa<VerifyInstruction>(user)){
//                        op->remove_use(user);
//                        return Execute(op); // start new loop since we might invalidate iterator
//                    }
//                }
//            }
//        };
//
//        struct TrivialAndRemover : UpTreeRunnerTyped<leaf_values_ts>{
//            using UpTreeRunnerTyped::UpTreeRunnerTyped;
//            void Execute(Operation* op){
//                // as we recurse into our parents we check children
//                // as removing parents from the parents break our ability to recurse upwards
//                for (auto o: op->operands) {
//                    if(isa<And>(o) && o->operands.size() == 1){
//                        o->replace_all_uses_with(o->operands[0]);
//                    }
//                }
//            }
//        };
//
//        struct BitNegationFixer : UpTreeRunnerTyped<leaf_values_ts>{
//            using UpTreeRunnerTyped::UpTreeRunnerTyped;
//            void Execute(Operation* op){
//
//                if(isa<InputRegister>(op)){
//                    auto reg = dynamic_cast<InputRegister*>(op);
//                    if(reg->size == 1){
//                        std::cout << "sized one: " << reg->reg_name << std::endl;
//                        for(auto& user: reg->users){
//                            if(isa<ZExt>(user)){
//                                std::cout << "zero extended" << std::endl;
//
//                                for(auto& u: user->users) {
//                                    if(isa<Icmp_eq>(u)){
//                                        auto cmp = dynamic_cast<Icmp_eq*>(u);
//                                        auto target = cmp->operands[1];
//                                        if(isa<Constant>(target)){
//                                            auto c = dynamic_cast<Constant*>(target);
//                                            if(c->bits == std::string(8, '0')){
//                                                std::cout << "cmp with 0000000000" << std::endl;
//                                                auto replace = circuit->create<Not>(1u);
//                                                replace->add_use(reg);
//                                                std::cout << "replace value" << std::endl;;
//                                                cmp->replace_all_uses_with(replace);
//                                            }
//                                        }
//                                    }
//                                }
//                            }
//                        }
//                    }
//                }
//                // as we recurse into our parents we check children
//                // as removing parents from the parents break our ability to recurse upwards
////                for (auto o: op->operands) {
////                    if(isa<And>(o) && o->operands.size() == 1){
////                        o->replace_all_uses_with(o->operands[0]);
////                    }
////                }
//            }
//        };

        void visit(Circuit *op) {
            Init();
//            DownTreeIdentityMarker im;
//            DownTreeIdentityRemover ir;
//            im.Run(op);
//            ir.Run(op);
//
//            CompleteOrMarker om;
//            om.Run(op);
//            ir.Run(op); // depends on having idenities removed
//            TrivialAndRemover tar(op);
//            tar.Run(op);
//            ir.Run(op); // depends on having idenities removed
//
////            RemoveCurrentSubtreeFromContext<trace_ts> trace_remover(op);
////            trace_remover.Run(op);
//
//            BitNegationFixer bnf(op);
//            bnf.Run(op);


            op->traverse(*this);
            Node(op);
            for (std::size_t i = 0; i < op->operands.size(); ++i) {
                Edge(op, op->operands[i], i);
            }
            os << "}";
        }

        std::ostream &os;
        const value_map_t &node_values;
        ColorFunc color_op;
    };
} // namespace circ::dot

namespace circ
{
    void print_dot(std::ostream &os, Circuit *circuit,
                  const std::unordered_map<Operation *, std::string> &node_values,
                  std::function<print::Color(Operation*)> oc)
    {
      circ::dot::Printer<std::function<print::Color(Operation*)>> dot_os(os, node_values, oc);
      dot_os.visit(circuit);
    }
} // namespace circ
