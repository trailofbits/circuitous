/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/CallSite.h>
#include <llvm/IR/CFG.h>
#pragma clang diagnostic pop

#include <sstream>

namespace circuitous {

  // Keeps track of instruction dependencies.
  template <typename T>
  class DependencyVisitor {
  public:
    void VisitArgument(llvm::Use &, llvm::Argument *) {}
    bool VisitInstruction(llvm::Use &, llvm::Instruction *) {
      return true;
    }
    void VisitConstant(llvm::Use &, llvm::Constant *) {}
    void Visit(llvm::Use &use_);
  };

  // Analyze how `use_` is produced.
  template <typename T>
  void DependencyVisitor<T>::Visit(llvm::Use &use_) {
    auto self = reinterpret_cast<T *>(this);

    std::vector<llvm::Use *> work_list;
    work_list.emplace_back(&use_);

    while (!work_list.empty()) {
      const auto use = work_list.back();
      work_list.pop_back();

      const auto val = use->get();

      // Bottom out at an argument; it should be an input register.
      if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val); arg_val) {
        self->VisitArgument(*use, arg_val);

      // Instruction; follow the dependency chain.
      } else if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val);
                inst_val) {
        if (self->VisitInstruction(*use, inst_val)) {
          if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val);
              call_val) {
            for (auto &op_use : call_val->arg_operands()) {
              work_list.push_back(&op_use);
            }
          } else {
            for (auto &op_use : inst_val->operands()) {
              work_list.push_back(&op_use);
            }
          }
        }

      // Bottom out at a constant, ignore for now.
      } else if (auto const_val = llvm::dyn_cast<llvm::Constant>(val);
                const_val) {
        if (!llvm::isa<llvm::Function>(const_val)) {
          self->VisitConstant(*use, const_val);
        }

      } else {
        LOG(ERROR) << "Unexpected value encountered during dependency analysis: "
                  << remill::LLVMThingToString(val);
      }
    }
  }

  // Collect register dependencies.
  class RegisterDependencyCollector
      : public DependencyVisitor<RegisterDependencyCollector> {
  public:
    explicit RegisterDependencyCollector(const remill::Arch *arch_)
        : arch(arch_) {}

    // Analyze how `val` is produced, and what input registers are read to
    // compute `val` or other output registers in the same logical instruction.
    void VisitArgument(llvm::Use &use, llvm::Argument *arg_val) {
      const auto arg_name = arg_val->getName();
      CHECK(!arg_name.endswith("_next"))
          << "Unexpected output register " << arg_val->getName().str()
          << " appears in use chain for computation of next value of "
          << remill::LLVMThingToString(use.getUser());

      CHECK(arch->RegisterByName(arg_name.str()))
          << "Argument " << remill::LLVMThingToString(arg_val)
          << " is not associated with a register";

      read_registers.insert(arg_val);
      auto func = arg_val->getParent();
      for (auto arg_it = func->arg_begin() + arg_val->getArgNo() + 1u;
          arg_it != func->arg_end(); ++arg_it) {
        if (arg_it->getName().endswith("_next") &&
            arg_it->getName().startswith(arg_name)) {
          written_registers.insert(&*arg_it);
          break;
        }
      }
    }

    const remill::Arch *const arch;
    std::set<llvm::Argument *> read_registers;
    std::set<llvm::Argument *> written_registers;
  };

}  // namespace circuitous
