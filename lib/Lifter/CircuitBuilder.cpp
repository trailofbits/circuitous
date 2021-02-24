/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "CircuitBuilder.h"
#include "circuitous/IR/Lifter.hpp"
#include "InstructionFuzzer.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#pragma clang diagnostic pop

#include <sstream>

namespace circuitous {
namespace {

// Get the semantic name for an instruction encoding. We attach on the size of
// the instruction in bytes as on x86, the iforms from XED don't guarantee us
// the same size of bits.
static std::string IselName(const remill::Instruction &inst) {
  CHECK_GE(15, inst.bytes.size());
  return inst.function + ("123456789abcdef"[inst.bytes.size()]);
}

// Apply a callback `cb` to every instruction in the buffer `buff`.
template <typename CB>
static void ForEachInstructionInBuffer(const remill::Arch::ArchPtr &arch,
                                       llvm::StringRef buff, CB cb) {
  const auto max_inst_size = arch->MaxInstructionSize();

  remill::Instruction inst;
  for (size_t i = 0u, max_i = buff.size(); i < max_i; inst.Reset()) {
    auto next_i = std::min<size_t>(max_i, i + max_inst_size);
    std::string_view bytes(&(buff.data()[i]), next_i - i);

    if (!arch->DecodeInstruction(0, bytes, inst) || !inst.IsValid()) {
      LOG(ERROR) << "Unable to decode instruction at byte offset " << i;
      ++i;
    } else {
      cb(inst);
      i += inst.bytes.size();
    }
  }
}

// Check that the decoding of a particular instruction results in position-
// independent operands. It's possible that some operands have PC-relative
// operands and have pre-calculated those values.
static bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                        const remill::Instruction &inst) {
  remill::Instruction copy;
  if (!arch->DecodeInstruction(inst.pc + 32, inst.bytes, copy) ||
      inst.operands.size() != copy.operands.size()) {
    return false;
  }

  for (auto i = 0u; i < inst.operands.size(); ++i) {
    if (inst.operands[i].Serialize() != copy.operands[i].Serialize()) {
      return false;
    }
  }

  return true;
}

// TODO(pag): Add other architecture flag names here.
static const std::string kFlagRegisters =
    ",SF,OF,PF,AF,ZF,CF"  // x86, amd64.
    ",N,Z,C,V"  // AArch64.
    ",icc_c,icc_v,icc_z,icc_n"  // SPARCv8.
    ",xcc_c,xcc_v,xcc_z,xcc_n"  // SPARCv9.
    ",";

// Return an integral type that is big enough to hold any value can inhabit the
// register associated with `reg`.
static llvm::IntegerType *IntegralRegisterType(llvm::Module &module,
                                               const remill::Register *reg) {
  if (reg->type->isIntegerTy()) {

    // Optimization for flag registers, which should only occupy a single
    // bit. We look to see if it's in the set of
    if (auto found_at = kFlagRegisters.find(reg->name);
        found_at != std::string::npos && 0 < found_at &&
        (found_at + 1u) < kFlagRegisters.size() &&
        kFlagRegisters[found_at - 1u] == ',' &&
        kFlagRegisters[found_at + reg->name.size()] == ',') {

      return llvm::Type::getInt1Ty(module.getContext());
    } else {
      return llvm::dyn_cast<llvm::IntegerType>(reg->type);
    }
  } else {
    return llvm::Type::getIntNTy(
        module.getContext(),
        static_cast<unsigned>(
            module.getDataLayout().getTypeAllocSize(reg->type) * 8u));
  }
}

// Looks for calls to a function like `__remill_error`, and
// replace its state pointer with a null pointer so that the state
// pointer never escapes.
static void MuteStateEscape(llvm::Module &module, const char *func_name) {
  auto func = module.getFunction(func_name);
  if (!func) {
    return;
  }

  for (auto user : func->users()) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user)) {
      auto arg_op = call_inst->getArgOperand(remill::kStatePointerArgNum);
      call_inst->setArgOperand(remill::kStatePointerArgNum,
                               llvm::UndefValue::get(arg_op->getType()));
    }
  }
}


void OptimizeSilently(const remill::Arch *arch, llvm::Module *module,
                      const std::vector<llvm::Function *> &fns) {
  auto saved_threshold = module->getContext().getDiagnosticsHotnessThreshold();
  module->getContext().setDiagnosticsHotnessThreshold(1);
  remill::OptimizeModule(arch, module, fns);
  module->getContext().setDiagnosticsHotnessThreshold(saved_threshold);
}

}  // namespace

llvm::Function *CircuitBuilder::Build(llvm::StringRef buff) {
  if (auto used = module->getGlobalVariable("llvm.used"); used) {
    used->eraseFromParent();
  }

  // TODO(pag): Create an immediate operand analysis that can identify bits
  //            associated with immediates (e.g. by bit flipping, re-decoding,
  //            then structurally comparing the decoded instructions for
  //            near-equivalence). Feed results of this analysis into a class
  //            deriving from the instruction lifter, and then use it to
  //            lift all immediates as something like:
  //
  //                __circuitous_immediate(i64 val, iN mask)
  //
  //            Where the `iN mask` is a bitmask on the instruction encoding
  //            representing the bits that produced `val`.

  // TODO(pag): Simplify certain patterns into intrinsic calls, e.g. getting
  //            the sign bit, getting the low N bits of an instruction (i.e.
  //            convert the AND into a function call.

  auto isels = DecodeInstructions(buff);
  IdentifyImms(isels);
  LiftInstructions(isels);

  // Delete the `__remill_intrinsics` so that we can get rid of more
  // functions.
  if (auto intrinsics = module->getFunction("__remill_intrinsics");
      intrinsics) {
    intrinsics->eraseFromParent();
  }

  if (auto mark_as_used = module->getFunction("__remill_mark_as_used");
      mark_as_used) {
    mark_as_used->eraseFromParent();
  }

  // TODO(lukas): Move to ctor
  one_of_func = intrinsics::OneOf::CreateFn(module.get());
  verify_inst_func = intrinsics::VerifyInst::CreateFn(module.get());

  // Mark these functions as not touching memory; this will help LLVM to
  // better optimize code that calls these functions.
  one_of_func->addFnAttr(llvm::Attribute::ReadNone);
  verify_inst_func->addFnAttr(llvm::Attribute::ReadNone);

  // These improve optimizability.
  MuteStateEscape(*module, "__remill_function_return");
  MuteStateEscape(*module, "__remill_error");
  MuteStateEscape(*module, "__remill_missing_block");

  return BuildCircuit1(BuildCircuit0(std::move(isels)));
}

template<typename T = uint64_t>
T CircuitBuilder::inst_fragments_count(llvm::CallInst *inst) const {
  auto maybe_count = GetMetadata(inst, bytes_fragments_count_kind);
  CHECK(maybe_count) << "Byte fragments size not set for"
                     << remill::LLVMThingToString(inst);
  return static_cast<T>(*maybe_count);
}

void CircuitBuilder::IdentifyImms(CircuitBuilder::InstSelections &insts) {
  for (auto &inst : insts) {
    for (auto i = 0U; i < inst.instructions.size(); ++i) {
      if (reduce_imms) {
        LOG(INFO) << "Searching for immediate operands regions in:";
        LOG(INFO) << inst.instructions[i].Serialize();
        inst.imms.push_back(InstructionFuzzer{arch}.FuzzOps(inst.instructions[i]));
      } else {
        inst.imms.emplace_back();
      }
    }
  }
}

// Decode all instructions in `buff` using `arch`. Group the instructions in
// terms of a general semantic category/class.
std::vector<InstructionSelection>
CircuitBuilder::DecodeInstructions(llvm::StringRef buff) {
  std::vector<InstructionSelection> grouped_insts;
  std::set<std::string> inst_bytes;
  std::unordered_map<std::string, size_t> isel_index;

  ForEachInstructionInBuffer(arch, buff, [&](remill::Instruction &inst) {
    auto all_zeroes = true;
    for (auto b : inst.bytes) {
      if (b) {
        all_zeroes = false;
        break;
      }
    }

    CHECK(!all_zeroes)
        << "Instructions whose machine code representation is all zeroes are "
        << "not permitted as they would invalidate the XOR-based checking of "
        << "encode verification checks";

    // It's likely that some of Remill's decoders implicitly put position-
    // dependent operands into the operands list, so try to catch that, warn
    // about them, and skip them.
    if (!IsDecodePositionIndependent(arch, inst)) {
      LOG(ERROR) << "Skipping position-dependent instruction: "
                 << inst.Serialize();
      return;
    }

    if (auto num_bits = static_cast<unsigned>(inst.bytes.size() * 8u);
        num_bits > encoded_inst_size) {
      encoded_inst_size = num_bits;
      CHECK_LE(encoded_inst_size, kMaxNumInstBits);
    }

    // Group the unique decoded instructions in terms of their ISELs, i.e. the
    // general semantic category of those instructions.
    if (auto [it, inserted] = inst_bytes.insert(inst.bytes);
        (void) it, inserted) {

      InstructionSelection *iclass = nullptr;

      // Make the isel specific to the `inst.function` and its size.
      const auto isel = IselName(inst);
      if (auto index_it = isel_index.find(isel); index_it != isel_index.end()) {
        iclass = &(grouped_insts[index_it->second]);
        CHECK_EQ(inst.bytes.size(), iclass->instructions.back().bytes.size());

      } else {
        isel_index.emplace(isel, grouped_insts.size());
        grouped_insts.emplace_back();
        iclass = &(grouped_insts.back());

        // iclass->known_bits.set();
      }

      iclass->encodings.emplace_back();

      auto &encoding = iclass->encodings.back();
      size_t i = 0u;
      for (char byte_ : std::string(inst.bytes.rbegin(), inst.bytes.rend())) {
        const auto byte = static_cast<uint8_t>(byte_);
        for (auto b = 0u; b < 8u; ++b, ++i) {
          if ((byte >> b) & 1u) {
            encoding.set(i);
          }
        }
      }

      iclass->instructions.emplace_back(std::move(inst));
    }
  });

  return grouped_insts;
}

// Flatten all control flow into pure data-flow inside of a function.
void CircuitBuilder::FlattenControlFlow(
    llvm::Function *func, const remill::IntrinsicTable &intrinsics) {

  const auto entry_block = &(func->getEntryBlock());
  llvm::ReversePostOrderTraversal<llvm::BasicBlock *> it(entry_block);

  std::unordered_map<llvm::BasicBlock *, llvm::Value *> reaching_cond;
  std::unordered_map<llvm::BasicBlock *,
                     std::unordered_map<llvm::BasicBlock *, llvm::Value *>>
      pred_conds;

  const auto new_block =
      llvm::BasicBlock::Create(context, "", func, entry_block);

  std::vector<llvm::Instruction *> insts;
  std::vector<llvm::Instruction *> to_remove;
  std::vector<llvm::BasicBlock *> orig_blocks;
  std::vector<std::pair<llvm::ReturnInst *, llvm::Value *>> ret_vals;


  for (llvm::BasicBlock *block : it) {
    orig_blocks.push_back(block);
  }

  for (auto block : orig_blocks) {

    // The entry block is guaranteed to be reachable.
    if (block->hasNPredecessors(0)) {
      reaching_cond.emplace(block, true_value);
      pred_conds[block].emplace(nullptr, true_value);
      pred_conds[block].emplace(block, true_value);

    // Figure out the reaching conditions for this block, and express them as
    // data flow (i.e. instructions).
    } else {
      llvm::IRBuilder<> ir(new_block);
      auto cond = false_value;

      for (auto pred_block : llvm::predecessors(block)) {
        const auto pred_cond = reaching_cond[pred_block];
        LOG_IF(FATAL, !pred_cond)
            << "Cycle in control-flow graphs are not handled";

        const auto pred_br =
            llvm::dyn_cast<llvm::BranchInst>(pred_block->getTerminator());

        const auto pred_switch =
            llvm::dyn_cast<llvm::SwitchInst>(pred_block->getTerminator());

        // Figure out the reaching condition for `block` coming through
        // `pred`.
        llvm::Value *edge_cond = pred_cond;
        if (pred_br && pred_br->isConditional()) {
          const auto true_succ = pred_br->getSuccessor(0);
          const auto false_succ = pred_br->getSuccessor(1);

          if (true_succ != false_succ) {
            edge_cond = pred_br->getCondition();
            if (true_succ == block) {
              edge_cond = ir.CreateAnd(pred_cond, edge_cond);
            } else {
              edge_cond = ir.CreateAnd(pred_cond, ir.CreateNot(edge_cond));
            }
          } else {
            edge_cond = pred_cond;
          }

        } else if (pred_switch) {
          LOG(FATAL) << "TODO: Edge condition on switch.";
        }

        pred_conds[block].emplace(pred_block, edge_cond);
        cond = ir.CreateXor(cond, edge_cond);
      }

      reaching_cond.emplace(block, cond);
    }

    insts.clear();
    for (llvm::Instruction &inst : *block) {
      insts.push_back(&inst);
    }

    for (auto inst : insts) {
      if (auto phi = llvm::dyn_cast<llvm::PHINode>(inst); phi) {
        llvm::IRBuilder<> ir(new_block);

        const auto num_preds = phi->getNumIncomingValues();
        CHECK_LT(0, num_preds);

        llvm::Value *sel_val = nullptr;

        if (1 == num_preds) {
          sel_val = phi->getIncomingValue(0);

        // Turn it into a SelectInst.
        } else if (2 == num_preds) {
          auto pred_block = phi->getIncomingBlock(0);
          auto val_cond = pred_conds[block][pred_block];
          LOG_IF(FATAL, !val_cond) << "Missing reaching condition for value";
          CHECK_NE(val_cond, true_value);
          auto true_val = phi->getIncomingValue(0);
          auto false_val = phi->getIncomingValue(1);
          sel_val = ir.CreateSelect(val_cond, true_val, false_val);

        // Turn it into a tower of SelectInsts.
        } else {
          sel_val = llvm::Constant::getNullValue(phi->getType());
          for (auto i = 0u; i < num_preds; ++i) {
            auto pred_block = phi->getIncomingBlock(i);
            auto pred_val = phi->getIncomingValue(i);
            auto val_cond = pred_conds[block][pred_block];
            LOG_IF(FATAL, !val_cond) << "Missing reaching condition for value";
            CHECK_NE(val_cond, true_value);
            sel_val = ir.CreateSelect(val_cond, pred_val, sel_val);
          }
        }

        phi->replaceAllUsesWith(sel_val);
        to_remove.push_back(inst);

      } else if (auto ret = llvm::dyn_cast<llvm::ReturnInst>(inst); ret) {
        ret_vals.emplace_back(ret, reaching_cond[block]);

      } else if (!inst->isTerminator()) {
        inst->removeFromParent();
        new_block->getInstList().insert(new_block->end(), inst);

      } else {
        to_remove.push_back(inst);
      }
    }
  }

  // Add a final return value to the data flow function.
  CHECK(!ret_vals.empty());
  if (1 == ret_vals.size()) {
    ret_vals[0].first->removeFromParent();
    new_block->getInstList().insert(new_block->end(), ret_vals[0].first);
    ret_vals.clear();

  // Create a tower of selects, where the default value is a call to
  // `__remill_error`, which will signal downstream translation to
  // the IR to produce set the error bit.
  } else {
    llvm::IRBuilder<> ir(new_block);

    llvm::Value *args[remill::kNumBlockArgs];
    for (auto i = 0u; i < remill::kNumBlockArgs; ++i) {
      args[i] = llvm::UndefValue::get(remill::NthArgument(func, i)->getType());
    }

    llvm::Value *sel_val = ir.CreateCall(intrinsics.error, args);
    for (auto [ret_inst, reaching_cond] : ret_vals) {
      CHECK_NE(reaching_cond, true_value);
      llvm::Value *ret_val = ret_inst->getReturnValue();
      sel_val = ir.CreateSelect(reaching_cond, ret_val, sel_val);
      ret_inst->eraseFromParent();
    }

    ir.CreateRet(sel_val);
  }

  for (auto inst : to_remove) {
    inst->eraseFromParent();
  }

  for (auto block : orig_blocks) {
    block->eraseFromParent();
  }
}

// Decode all instructions in `buff` using `arch` and fill up `inst_funcs`.
void CircuitBuilder::LiftInstructions(
    std::vector<InstructionSelection> &isels) {
  remill::IntrinsicTable intrinsics(module);
  InstructionLifter lifter(arch, intrinsics);
  std::vector<llvm::Function *> inst_funcs;

  unsigned g = 0u;
  for (auto &group : isels) {
      CHECK(group.instructions.size() == group.encodings.size());

    for (auto i = 0U; i < group.instructions.size(); ++i) {
      auto &inst = group.instructions[i];
      std::stringstream ss;
      ss << "inst_" << g << '_' << i;

      auto func = remill::DeclareLiftedFunction(module.get(), ss.str());
      remill::CloneBlockFunctionInto(func);
      auto block = &func->getEntryBlock();

      switch (lifter.LiftIntoBlock(inst, block, false, group.imms[i])) {
        case remill::LiftStatus::kLiftedInstruction:
          (void) llvm::ReturnInst::Create(
              context, remill::LoadMemoryPointer(block), block);

          // Make sure these functions stick around.
          func->removeFnAttr(llvm::Attribute::InlineHint);
          func->removeFnAttr(llvm::Attribute::AlwaysInline);
          func->setLinkage(llvm::GlobalValue::ExternalLinkage);
          func->addFnAttr(llvm::Attribute::NoInline);

          inst_funcs.push_back(func);
          continue;

        case remill::LiftStatus::kLiftedUnsupportedInstruction:
          LOG(ERROR) << "Missing semantics for instruction: "
                     << inst.Serialize();
          func->eraseFromParent();
          continue;

        case remill::LiftStatus::kLiftedInvalidInstruction:
          LOG(ERROR) << "Invalid instruction: " << inst.Serialize();
          func->eraseFromParent();
          continue;
        default:
          break;
      }

    }

    ++g;
  }

  OptimizeSilently(arch.get(), module.get(), inst_funcs);

  std::vector<llvm::Function *> reopt_funcs;
  for (auto func : inst_funcs) {
    if (func->size() == 1) {
      continue;  // Pure data-flow; doesn't need to be re-optimized.
    }

    reopt_funcs.push_back(func);
    FlattenControlFlow(func, intrinsics);
  }

  if (!reopt_funcs.empty()) {
    OptimizeSilently(arch.get(), module.get(), reopt_funcs);
  }

  // We're done; make the instruction functions more amenable for inlining
  // and elimination.
  for (auto func : inst_funcs) {
    func->setLinkage(llvm::GlobalValue::PrivateLinkage);
    func->removeFnAttr(llvm::Attribute::NoInline);
    func->addFnAttr(llvm::Attribute::InlineHint);
    func->addFnAttr(llvm::Attribute::AlwaysInline);
  }
}

// Apply a callback `cb(unsgined, const remill::Instruction &, llvm::CallInst *)`
// to each call of `__circuitous_verify_inst` in `circuit_func`.
template <typename T>
void CircuitBuilder::ForEachVerification(llvm::Function *circuit_func, T cb) {
  std::vector<llvm::CallInst *> verify_calls;
  for (auto user : verify_inst_func->users()) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user);
        call_inst && call_inst->getParent()->getParent() == circuit_func) {
      verify_calls.push_back(call_inst);
    }
  }
  for (auto call_inst : verify_calls) {
    cb(call_inst);
  }
}

// Build the first level circuit. We will analyze this function later to
// get an accurate picture of instruction dependencies.
auto CircuitBuilder::BuildCircuit0(std::vector<InstructionSelection> isels)
-> Circuit0 {

  // We'll be using this one a lot
  llvm::IRBuilder<> ir(context);

  const auto &dl = module->getDataLayout();

  auto circuit0 = Circuit0(*this);
  auto circuit0_func = circuit0.Create();

  auto entry_block = llvm::BasicBlock::Create(context, "", circuit0_func);
  auto exit_block = llvm::BasicBlock::Create(context, "", circuit0_func);
  auto prev_block = entry_block;

  // TODO(lukas): Maybe do this when budiling the fn.
  auto pc = [&]() -> llvm::Value *{
    for (auto &[reg, in, _] : circuit0.reg_to_args) {
      if (reg->name == arch->ProgramCounterRegisterName()) {
        return in;
      }
    }
    return nullptr;
  }();

  CHECK(pc != nullptr) << "Couldn't find program counter register "
                       << arch->ProgramCounterRegisterName();

  llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
  inst_func_args[remill::kPCArgNum] = pc;
  inst_func_args[remill::kMemoryPointerArgNum] =
      llvm::UndefValue::get(mem_ptr_type);

  // General parameters array. First used for collecting results of comparing
  // register states. Then used for collecting the bits that we use to encode
  // instructions given a verified instruction.
  std::vector<llvm::Value *> params;

  // Vector of return values, one for each result of doing a
  // `__circuitous_verify_decode`.
  std::vector<llvm::Value *> verified_insts;

  auto g = 0u;
  for (const auto &isel : isels) {
    auto i = 0u;
    const auto inst_name = IselName(isel.instructions.back());

    params.clear();

    // Add one basic block per lifted instruction. Each block allocates a
    // separate state structure.
    for (i = 0u; i < isel.instructions.size(); ++i) {
      std::stringstream ss;
      ss << "inst_" << g << '_' << i;
      const auto inst_func = module->getFunction(ss.str());
      CHECK_NOTNULL(inst_func);

      auto inst_block = llvm::BasicBlock::Create(context, "", circuit0_func);
      llvm::BranchInst::Create(inst_block, prev_block);
      prev_block = inst_block;

      ir.SetInsertPoint(inst_block);
      const auto state_ptr = ir.CreateAlloca(state_ptr_type->getElementType());

      for (auto [reg, arg, _] : circuit0.reg_to_args) {
        // TODO(surovic): The code from here down to and including
        // the bitcast is copy-pasted further down. Rewrite this.
        const auto reg_type = IntegralRegisterType(*module, reg);
        const auto reg_store_type = ir.getIntNTy(
            static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto reg_addr = reg->AddressOf(state_ptr, inst_block);
        auto reg_addr_type = llvm::PointerType::get(reg_store_type, 0);
        if (reg_addr->getType() != reg_addr_type) {
          reg_addr = ir.CreateBitCast(reg_addr, reg_addr_type);
        }
        // TODO(surovic): End-Of-Pasta
        llvm::Value *reg_val = arg;
        if (reg_type != reg_store_type) {
          reg_val = ir.CreateZExt(reg_val, reg_store_type);
        }

        ir.CreateStore(reg_val, reg_addr);
      }

      inst_func_args[remill::kStatePointerArgNum] = state_ptr;
      ir.CreateCall(inst_func, inst_func_args);

      // First few arguments are the known parts of this instruction's encoding
      // and are common across all instructions sharing the same ISEL.
      // params.resize(num_instruction_parts);
      params.clear();

      // Add instruction encoding check
      {
        // Reorders bytes so that they can be matched to extract from instruction
        // bytes without extra concats.
        auto generate_raw_bytes = [](auto full, uint64_t from, uint64_t to) {
          std::string out;
          while(true) {
            uint64_t y = std::min(from + (8 - from % 8), to);
            std::string partial = full.substr(from, y - from);
            std::reverse(partial.begin(), partial.end());
            out = partial + out;
            if (y == to) {
              return out;
            }
            from = y;
          }
        };

        LOG(INFO) << "Creating relevant sections of: " << isel.encodings[i].to_string();
        auto rinst_size = isel.instructions[i].bytes.size() * 8;
        const auto &encoding = isel.encodings[i];

        auto create_bit_check = [&](auto from, auto to) {
          LOG(INFO) << "Creating bitcheck: " << from << " " << to;

          auto fn = intrinsics::ExtractRaw::CreateFn(module.get(), from, to - from );

          std::string full_inst;
          // Encoding check needed since `x` is unsigned.
          for (auto x = rinst_size - 1; x >= 0 && x < encoding.size(); --x) {
            full_inst += (encoding[x]) ? '1' : '0';
          }
          // TODO(lukas): Sorry, most likely there is a more sane way to do this
          //              but since it may change in rather near future I am keeping
          //              it this way.
          std::reverse(full_inst.begin(), full_inst.end());
          std::string expected = generate_raw_bytes(full_inst, from, to);

          auto size = static_cast<uint32_t>(expected.size());
          CHECK(size == to - from) << size << " != " << to - from;

          auto expected_v = ir.getInt(llvm::APInt(size, expected, 2));
          const auto rhs = remill::NthArgument(circuit0_func, 0);
          auto x = ir.CreateCall(fn, {rhs});
          auto y = ir.CreateCall(
            intrinsics::BitCompare::CreateFn(module.get(), size), {expected_v, x});
          return y;
        };

        std::map<uint64_t, uint64_t> flattened_imm_regions;
        for (auto &[_, data] : isel.imms[i]) {
          for(auto &[from, size] : data) {
            flattened_imm_regions.emplace(from, from + size);
          }
        }

        uint64_t current = 0;
        for (auto &[from, to] : flattened_imm_regions) {
          if (current != from) {
            params.push_back(create_bit_check(current, from));
          }
          current = to;
        }
        if (current != isel.instructions[i].bytes.size() * 8) {
          params.push_back(
            create_bit_check(current, isel.instructions[i].bytes.size() * 8));
        }
      }
      // TODO(lukas): There can be something else present
      auto fragments_size = params.size();

      // Final set of parameters are comparisons on whether or not the resulting
      // register after the semantic has executed matches the next state of that
      // register.
      for (auto [reg, _, expected_reg_val] : circuit0.reg_to_args) {
        // TODO(surovic): See above TODO tag about duplication.
        const auto reg_type = IntegralRegisterType(*module, reg);
        const auto reg_store_type = ir.getIntNTy(
            static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto reg_addr = reg->AddressOf(state_ptr, inst_block);
        auto reg_addr_type = llvm::PointerType::get(reg_store_type, 0);
        if (reg_addr->getType() != reg_addr_type) {
          reg_addr = ir.CreateBitCast(reg_addr, reg_addr_type);
        }
        // TODO(surovic): End-Of-Pasta
        llvm::Value *reg_val = ir.CreateLoad(reg_addr);
        if (reg_type != reg_store_type) {
          reg_val = ir.CreateTrunc(reg_val, reg_type);
        }

        auto eq_func = intrinsics::Eq::CreateFn(module.get(), reg_type);
        llvm::Value *eq_args[] = {reg_val, expected_reg_val};
        params.push_back(ir.CreateCall(eq_func, eq_args));
      }

      // Call the instruction verification function. This returns `1` iff we
      // verified the isel decoding (first argument) and if all other arguments
      // (register comparisons) are true.
      ir.SetInsertPoint(exit_block);
      auto call = ir.CreateCall(verify_inst_func, params);
      AddMetadata(call, bytes_fragments_count_kind, fragments_size);
      verified_insts.push_back(call);
    }
    ++g;
  }

  llvm::BranchInst::Create(exit_block, prev_block);
  ir.SetInsertPoint(exit_block);
  ir.CreateRet(ir.CreateCall(one_of_func, verified_insts));

  OptimizeSilently(arch.get(), module.get(), {circuit0_func});

  return circuit0;
}

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

// Build the second level circuit. Here we analyze how the instruction checkers
// use registers and try to eliminate unneeded registers from the function's
// argument list.
llvm::Function *CircuitBuilder::BuildCircuit1(Circuit0 circuit0) {
  auto circuit0_func = circuit0.GetFn();
  RegisterDependencyCollector deps(arch.get());

  // Look at all calls to instruction verifiers. These function calls take as
  // input the integer index (into `insts`) of the instruction being verified,
  // followed by a variable (really: `regs.size()`) number of `i1` values that
  // should be the results of ICmp instructions, each testing whether or not
  // the current value of a register matches the next expected value of the
  // register.
  ForEachVerification(circuit0_func, [&](llvm::CallInst *verify_call_inst) {
    auto arg_num = 0u;
    auto reg_idx = 0u;
    auto inst_fragments_prefix = inst_fragments_count(verify_call_inst);
    for (auto &arg_use : verify_call_inst->arg_operands()) {
      llvm::Value *arg = arg_use.get();
      CHECK(inst_fragments_prefix > 0);
      if (arg_num < inst_fragments_prefix) {
        ++arg_num;
        continue;
      }

      // Figure out the input and output registers to the circuit function.
      // const auto reg_id = arg_num - num_instruction_parts;
      // const auto in_reg_arg_index = num_instruction_parts + (2u * reg_id);
      const auto in_reg_arg_index = circuit0.inst_bytes.size() + (2u * reg_idx);
      const auto in_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index);
      const auto out_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index + 1u);

      CHECK(out_reg_arg->getName().endswith("_next"))
          << out_reg_arg->getName().str();
      CHECK(out_reg_arg->getName().startswith(in_reg_arg->getName()));

      const auto in_reg_name = in_reg_arg->getName().str();
      const auto out_reg_name = out_reg_arg->getName().str();
      ++arg_num; ++reg_idx;

      const auto call_inst = llvm::dyn_cast<llvm::CallInst>(arg);
      CHECK(call_inst != nullptr)
          << "Unexpected argument value for " << in_reg_name << ": "
          << remill::LLVMThingToString(arg);

      const auto icmp_eq = call_inst->getCalledFunction();
      CHECK_NOTNULL(icmp_eq);
      CHECK(icmp_eq->getName().startswith("__circuitous_icmp_eq_"))
        << LLVMName(icmp_eq);

      const auto lhs = call_inst->getArgOperand(0);
      const auto rhs = call_inst->getArgOperand(1);

      const auto rhs_arg = llvm::dyn_cast<llvm::Argument>(rhs);
      CHECK_EQ(rhs_arg, out_reg_arg)
          << "Expected second argument to " << icmp_eq->getName().str()
          << " to match the output register " << out_reg_name << " but got "
          << remill::LLVMThingToString(rhs)
          << " instead: " << remill::LLVMThingToString(call_inst);

      // Input is compared to output, i.e. the instruction doesn't write
      // to this register.
      if ((llvm::isa<llvm::Argument>(lhs) && lhs == in_reg_arg)) {
        continue;

      // Something that wasn't this register's input (might be a different
      // register's input, or a constant, or a dynamic comparison) is
      // compared with the output reg, i.e. this instuction writes to the
      // register.
      } else if (llvm::isa<llvm::Argument>(lhs) ||
                 llvm::isa<llvm::Instruction>(lhs) ||
                 llvm::isa<llvm::ConstantInt>(lhs)) {
        deps.read_registers.insert(in_reg_arg);
        deps.written_registers.insert(out_reg_arg);
        auto &new_reg_val_use = call_inst->getArgOperandUse(0);
        deps.Visit(new_reg_val_use);

      // Some unrecognized pattern.
      } else {
        LOG(FATAL) << "Neither side of integer comparison associated with "
                   << out_reg_name << " was itself an argument of circuit_0 in "
                   << remill::LLVMThingToString(call_inst);
      }
    }
  });

  // Create the type for circuit1. Here, we'll have all output registers come
  // after all input registers.
  std::vector<llvm::Type *> circuit1_arg_types;
  std::vector<const remill::Register *> new_regs;
  auto i = 0u;

  // Start with the bits associated with the parts of an encoded instruction.
  // for (i = 0u; i < num_instruction_parts; ++i) {
  //   circuit1_arg_types.push_back(
  //       remill::NthArgument(circuit0_func, i)->getType());
  // }

  circuit1_arg_types.push_back(
      remill::NthArgument(circuit0_func, 0)->getType());

  for (auto in_reg : deps.read_registers) {
    new_regs.push_back(arch->RegisterByName(in_reg->getName().str()));
    circuit1_arg_types.push_back(in_reg->getType());
  }

  for (auto out_reg : deps.written_registers) {
    circuit1_arg_types.push_back(out_reg->getType());
  }

  // The read set should be a subset of the written set.
  CHECK_LE(deps.read_registers.size(), deps.written_registers.size());

  auto circuit1_func = llvm::Function::Create(
      llvm::FunctionType::get(bool_type, circuit1_arg_types, false),
      llvm::GlobalValue::ExternalLinkage, "circuit1_func", module.get());
  circuit1_func->addFnAttr(llvm::Attribute::ReadNone);

  // Rename the parameters to correspond with our input/output registers.
  // i = num_instruction_parts;
  i = 1u;
  for (auto in_reg : deps.read_registers) {
    remill::NthArgument(circuit1_func, i++)->setName(in_reg->getName());
  }

  for (auto out_reg : deps.written_registers) {
    remill::NthArgument(circuit1_func, i++)->setName(out_reg->getName());
  }

  // Mark circuit0 for inlining and elimination.
  circuit0_func->setLinkage(llvm::GlobalValue::PrivateLinkage);
  circuit0_func->addFnAttr(llvm::Attribute::InlineHint);
  circuit0_func->addFnAttr(llvm::Attribute::AlwaysInline);

  std::vector<llvm::Value *> args;

  args.push_back(remill::NthArgument(circuit1_func, 0));

  // Build up an argument list to call circuit0_func from circuit1_func. We
  // pass through the arguments associated with registers that the above
  // analysis determined to be used, and pass in null values (zeroes) for the
  // rest. We'll be able to observe calls like the following:
  //
  //    %34 = tail call i1 (i8, i8, ...) @__circuitous_icmp_eq_8(i8 0, i8 0)
  //
  // Removing uses of these redundant comparisons will let us shrink down the
  // verification call arg lists to only verify used registers.
  for (auto reg : regs) {
    const auto orig_reg = remill::FindVarInFunction(circuit0_func, reg->name);
    if (auto in_reg = remill::FindVarInFunction(circuit1_func, reg->name, true);
        in_reg) {
      args.push_back(in_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }

    if (auto out_reg =
            remill::FindVarInFunction(circuit1_func, reg->name + "_next", true);
        out_reg) {
      args.push_back(out_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }
  }

  auto entry = llvm::BasicBlock::Create(context, "", circuit1_func);
  auto res = llvm::CallInst::Create(circuit0_func, args, "", entry);
  (void) llvm::ReturnInst::Create(context, res, entry);

  // Optimizing the module again will inline circuit0_func into circuit1_func
  // and propagate the null (i.e. zero) values for all unused registers down
  // through the inlined body of circuit0_func.
  OptimizeSilently(arch.get(), module.get(), {circuit1_func});

  // "Constant fold" the uses of `__circuitous_icmp_eq_8`.
  std::vector<llvm::CallInst *> to_fold;

  for (auto matcher : intrinsics::Eq::All(module.get())) {
    if (!matcher) {
      continue;
    }
    for (auto user : matcher->users()) {
      if (const auto call_inst = llvm::dyn_cast<llvm::CallInst>(user);
          call_inst &&
          call_inst->getArgOperand(0) == call_inst->getArgOperand(1)) {
        to_fold.push_back(call_inst);
      }
    }
  }

  for (auto call_inst : to_fold) {
    call_inst->replaceAllUsesWith(true_value);
    call_inst->eraseFromParent();
  }

  std::vector<std::pair<llvm::CallInst *, llvm::CallInst *>> to_replace;

  // Now go through an change the arguments to `__circuitous_verify_inst` to
  // to reflect the new register transfer comparisons.
  ForEachVerification(circuit1_func, [&](llvm::CallInst *call_inst) {
    args.clear();
    auto inst_fragments_prefix = inst_fragments_count<uint32_t>(call_inst);
    for (auto j = 0u; j < inst_fragments_prefix; ++j) {
      args.push_back(call_inst->getArgOperand(j));
    }
    for (i = 0u; i < regs.size(); ++i) {
      const auto reg = regs[i];

      // const auto arg = call_inst->getArgOperand(i + num_instruction_parts);
      const auto arg = call_inst->getArgOperand(i + inst_fragments_prefix);
      if (std::find(new_regs.begin(), new_regs.end(), reg) != new_regs.end()) {
        CHECK(!llvm::isa<llvm::Constant>(arg)) << remill::LLVMThingToString(arg);
        args.push_back(arg);
      }
    }

    auto new_call =
        llvm::CallInst::Create(verify_inst_func, args, "", call_inst);
    to_replace.emplace_back(call_inst, new_call);
  });

  for (auto [old_call, new_call] : to_replace) {
    old_call->replaceAllUsesWith(new_call);
    old_call->eraseFromParent();
  }

  // Make sure `regs` represents the new and reduced set of registers that
  // we're going to be verifying.
  regs.swap(new_regs);

  return circuit1_func;
}

llvm::FunctionType *CircuitBuilder::Circuit0::FnT() {
  llvm::IRBuilder<> ir(parent.context);
  std::vector<llvm::Type *> param_types;

  // First parameter is the bit enconding of the instruction being verified
  param_types.push_back(ir.getIntNTy(kMaxNumInstBits));

    // The remaining parameters will be input/output registers for verification.
  for (auto reg : parent.regs) {
    const auto reg_type = IntegralRegisterType(*parent.module, reg);
    param_types.push_back(reg_type);
    param_types.push_back(reg_type);
  }

  return llvm::FunctionType::get(ir.getInt1Ty(), param_types, false);
}

llvm::Function *CircuitBuilder::Circuit0::GetFn() {
  return parent.module->getFunction(name);
}

llvm::Function *CircuitBuilder::Circuit0::Create() {
  llvm::IRBuilder<> ir(parent.context);

  if (auto fn = GetFn()) {
    LOG(FATAL) << "The module already has " << name << " function.";
    return fn;
  }

  auto fn = llvm::Function::Create(FnT(), llvm::GlobalValue::ExternalLinkage,
                                   name, parent.module.get());
  fn->addFnAttr(llvm::Attribute::ReadNone);

  CHECK(fn->arg_size() > 0);
  inst_bytes.push_back(&*fn->arg_begin());
  // The rest of arguments should be the registers in/out pairs
  CHECK((fn->arg_size() - inst_bytes.size()) % 2 == 0);
  for (auto i = inst_bytes.size(); i < fn->arg_size(); i += 2) {
    // This "order" can be flexible, but better let it go in order
    // of regs.
    auto reg = parent.regs[(i - inst_bytes.size()) / 2];

    auto input_reg = remill::NthArgument(fn, i);
    auto output_reg = remill::NthArgument(fn, i + 1);

    input_reg->setName(reg->name);
    output_reg->setName(reg->name + "_next");

    reg_to_args.emplace_back(reg, input_reg, output_reg);
  }

  return fn;
}

}  // namespace circuitous
