/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

static inline auto to_string(Operation *op) {
  std::stringstream ss;
  ss << op->Name() << " " << std::hex << op->id();
  return ss.str();
}

template<typename S>
void Interpreter<S>::SetNodeVal(Operation *op, const value_type &val) {
  node_values[op] = val;
}

template<typename S>
void Interpreter<S>::SetNodeVal(Operation *op, const raw_value_type &val) {
  node_values[op] = {val};
}

template<typename S>
auto Interpreter<S>::GetNodeVal(Operation *op) -> value_type {
  DLOG(INFO) << "Fetching value of: " << op->Name() << " " << op->id();
  auto iter{node_values.find(op)};
  CHECK(iter != node_values.end());
  return iter->second;
}

template<typename S>
void Interpreter<S>::SetInstructionBitsValue(const std::string &bits) {
  auto inst = circuit->Attr<InputInstructionBits>()[0];
  self().SetNodeVal(inst, llvm::APInt(inst->size, bits, /*radix=*/16U));
}

template<typename S>
void Interpreter<S>::SetInputRegisterValue(const std::string &name,
                                           uint64_t bits)
{
  DLOG(INFO) << "SETTING INPUT REG: " << name;
  if (auto reg = GetReg(name)) {
    return self().SetNodeVal(reg, llvm::APInt(reg->size, bits));
  }
  DLOG(WARNING) << "Input register " << name << " not present in circuit.";
}

template<typename S>
std::optional<uint64_t> Interpreter<S>::GetOutputRegisterValue(const std::string &name) {
  for (auto reg : circuit->Attr<OutputRegister>()) {
    if (reg->reg_name == name) {
      if (auto val = self().GetNodeVal(reg)) {
        return val->getLimitedValue();
      }
      return {};
    }
  }

  DLOG(FATAL) << "Output register " << name << " not present in circuit.";

  return 0ULL;
}

template<typename S>
void Interpreter<S>::VisitOperation(Operation *op) {
  DLOG(FATAL) << "Unhandled operation: " << op->Name() << " " << op->id();
}

template<typename S>
void Interpreter<S>::VisitConstant(Constant *op) {
  DLOG(INFO) << "VisitConstant: " << op->Name() << " " << op->id();
  std::string bits{op->bits.rbegin(), op->bits.rend()};
  self().SetNodeVal(op, llvm::APInt(op->size, bits, /*radix=*/2U));
}

template<typename S>
void Interpreter<S>::VisitInputRegister(InputRegister *op) {
  // TODO(lukas): Should this set undef?
  DLOG(INFO) << "VisitInputRegister: " << op->Name() << " " << op->id();
  CHECK(node_values.count(op))
      << "Input register " << op->reg_name << " bits not set.";
}

template<typename S>
void Interpreter<S>::VisitInputImmediate(InputImmediate *op) {
  DLOG(INFO) << "VisitInputImmediate: " << op->Name() << " " << op->id();
  CHECK(op->operands.Size() == 1)
    << "Incorrect number of operands of InputImmediate:"
    << op->operands.Size() << "!= 1";
  self().SetNodeVal(op, self().GetNodeVal(op->operands[0]));
}

template<typename S>
void Interpreter<S>::VisitOutputRegister(OutputRegister *op) {
  DLOG(INFO) << "VisitOutputRegister: " << op->Name() << " " << op->id();
  // TODO(surovic): figure out a better way to represent an
  // undefined initial value;
  if (!node_values.count(op)) {
    self().SetNodeVal(op, llvm::APInt(op->size, 0ULL));
  }
}

template<typename S>
void Interpreter<S>::VisitInputInstructionBits(InputInstructionBits *op) {
  DLOG(INFO) << "VisitInputInstructionBits: " << op->Name() << " " << op->id();
  CHECK(node_values.count(op)) << "Input instruction bits not set.";
}

template<typename S>
void Interpreter<S>::VisitHint(Hint *op) {
  DLOG(INFO) << "VisitHint: " << op->Name() << " " << op->id();
  // TODO(surovic): See VisitOutputRegister()
  if (!node_values.count(op)) {
    self().SetNodeVal(op, llvm::APInt(op->size, 0ULL));
  }
}

template<typename S>
void Interpreter<S>::VisitUndefined(Undefined *op) {
  DLOG(INFO) << "VisitUndefined: " << op->Name() << " " << op->id();
  // TODO(surovic): See VisitOutputRegister()
  if (!node_values.count(op)) {
    self().SetNodeVal(op, Undef());
  }
}

template<typename S>
void Interpreter<S>::VisitExtract(Extract *op) {
  DLOG(INFO) << "VisitExtract: " << op->Name() << " " << op->id();
  auto val = self().GetNodeVal(op->operands[0]);
  if (!val) {
    return self().SetNodeVal(op, val);
  }

  auto pos{op->low_bit_inc};
  auto num{op->high_bit_exc - pos};
  self().SetNodeVal(op, val->extractBits(num, pos));
}

// TODO(lukas): Some non-align cases most likely need some extra handling
//              which is currently not happening? Investigate.
template<typename S>
void Interpreter<S>::VisitConcat(Concat *op) {
  DLOG(INFO) << "Visit concat: " << op->Name() << " " << op->id();
  llvm::APInt build{ op->size, 0, false };
  auto current = 0u;
  for (auto i = 0u; i < op->operands.Size(); ++i) {
      auto val = self().GetNodeVal(op->operands[i]);
      if (!val) {
        return self().SetNodeVal(op, val);
      }
      build.insertBits(*val, current);
      current += op->operands[i]->size;
  }
  self().SetNodeVal(op, build);
}

template<typename S>
void Interpreter<S>::VisitNot(Not *op) {
  DLOG(INFO) << "Visit Not" << op->Name() << " " << op->id();
  auto val = self().GetNodeVal(op->operands[0]);
  if (!val) {
    return self().SetNodeVal(op, val);
  }
  // NOTE(lukas): To avoid confusion the copy is here explicitly, since `negate` does
  //              change the APInt instead of returning a new one.
  llvm::APInt copy = *val;
  copy.negate();
  self().SetNodeVal(op, copy);
}

template<typename S>
void Interpreter<S>::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name() << " " << op->id();
  if (!ValidChildren(op)) {
    return self().SetNodeVal(op, Undef());
  }

  auto lhs{[this, op] { return *self().GetNodeVal(op->operands[0]); }};
  auto rhs{[this, op] { return *self().GetNodeVal(op->operands[1]); }};

  auto str = [](auto val) { return val.toString(16, false); };

  switch (op->llvm_op_code) {
    case llvm::Instruction::OtherOps::Select: {
      auto selector = *self().GetNodeVal(op->operands[0]);
      auto true_val = *self().GetNodeVal(op->operands[1]);
      auto false_val = *self().GetNodeVal(op->operands[2]);
      self().SetNodeVal(op, selector.getBoolValue() ? true_val : false_val );
    } break;

    case llvm::BinaryOperator::Add: {
      self().SetNodeVal(op, lhs() + rhs());
    } break;

    case llvm::BinaryOperator::Sub: {
      self().SetNodeVal(op, lhs() - rhs());
    } break;

    case llvm::BinaryOperator::Mul: {
      self().SetNodeVal(op, lhs() * rhs());
    } break;

    case llvm::BinaryOperator::UDiv: {
      self().SetNodeVal(op, lhs().udiv(rhs()));
    } break;

    case llvm::BinaryOperator::SDiv: {
      DLOG(INFO) << str(lhs()) << "sdiv" << str(rhs());
      if (str(rhs()) == "0") {
        self().SetNodeVal(op, rhs());
      } else {
        self().SetNodeVal(op, lhs().sdiv(rhs()));
      }
    } break;

    case llvm::BinaryOperator::And: {
      self().SetNodeVal(op, lhs() & rhs());
    } break;

    case llvm::BinaryOperator::Or: {
      self().SetNodeVal(op, lhs() | rhs());
    } break;

    case llvm::BinaryOperator::Xor: {
      self().SetNodeVal(op, lhs() ^ rhs());
    } break;

    case llvm::BinaryOperator::Shl: {
      self().SetNodeVal(op, lhs() << rhs());
    } break;

    case llvm::BinaryOperator::LShr: {
      self().SetNodeVal(op, lhs().lshr(rhs()));
    } break;

    case llvm::BinaryOperator::AShr: {
      self().SetNodeVal(op, lhs().ashr(rhs()));
    } break;

    case llvm::BinaryOperator::Trunc: {
      self().SetNodeVal(op, lhs().trunc(op->size));
    } break;

    case llvm::BinaryOperator::ZExt: {
      self().SetNodeVal(op, lhs().zext(op->size));
    } break;

    case llvm::BinaryOperator::SExt: {
      self().SetNodeVal(op, lhs().sext(op->size));
    } break;

    case llvm::BinaryOperator::ICmp: {
      auto result{false};
      switch (op->llvm_predicate) {
        case llvm::CmpInst::ICMP_ULT: {
          result = lhs().ult(rhs());
        } break;

        case llvm::CmpInst::ICMP_SLT: {
          result = lhs().slt(rhs());
        } break;

        case llvm::CmpInst::ICMP_UGT: {
          result = lhs().ugt(rhs());
        } break;

        case llvm::CmpInst::ICMP_EQ: {
          result = lhs() == rhs();
        } break;

        case llvm::CmpInst::ICMP_NE: {
          result = lhs() != rhs();
        } break;

        default: DLOG(FATAL) << "Unknown LLVM operation: " << op->Name() << " " << op->id(); break;
      }
      self().SetNodeVal(op, result ? TrueVal() : FalseVal());
    } break;

    default: DLOG(FATAL) << "Unknown LLVM operation: " << op->Name() << " " << op->id(); break;
  }
}

template<typename S>
void Interpreter<S>::VisitSelect(Select *op) {
  DLOG(INFO) << "Visiting Select: " << op->Name() << " " << op->id();
  auto selector = self().GetNodeVal(op->operands[0]);
  if (!selector) { return self().SetNodeVal(op, selector); }
  self().SetNodeVal(op, self().GetNodeVal(op->operands[selector->getLimitedValue() + 1]));
}

template<typename S>
void Interpreter<S>::VisitParity(Parity *op) {
  DLOG(INFO) << "VisitParity: " << op->Name() << " " << op->id();
  auto val{self().GetNodeVal(op->operands[0])};
  if (!val) { return self().SetNodeVal(op, val); }
  self().SetNodeVal(op, llvm::APInt(1, val->countPopulation() % 2));
}

template<typename S>
void Interpreter<S>::VisitPopulationCount(PopulationCount *op) {
  DLOG(INFO) << "VisitPopulationCount: " << op->Name() << " " << op->id();
  auto val{self().GetNodeVal(op->operands[0])};
  if (!val) { return self().SetNodeVal(op, val); }
  self().SetNodeVal(op, llvm::APInt(op->size, val->countPopulation()));
}

template<typename S>
void Interpreter<S>::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name() << " " << op->id();
  auto inst{self().GetNodeVal(op->operands[0])};
  auto bits{self().GetNodeVal(op->operands[1])};
  if (!inst || !bits) {
    DLOG(FATAL) << "Unreachable";
    return self().SetNodeVal(op, Undef());
  }
  self().SetNodeVal(op, inst == bits ? TrueVal() : FalseVal());
}

template<typename S>
void Interpreter<S>::VisitRegisterCondition(RegisterCondition *op) {
  DLOG(INFO) << "VisitRegisterCondition: " << op->Name() << " " << op->id();
  auto val{op->operands[0]};
  auto reg{op->operands[1]};
  self().SetNodeVal(reg, self().GetNodeVal(val));
  self().SetNodeVal(op, TrueVal());
  // NOTE(msurovic): This is where we get compute "results".
  // Although I'm not sure if this is the correct way to do it.
  // Consider that we have output register condition checks
  // for various instructions present in the circuit. A valid
  // for one may not be a valid value for the another.
  //if (node_values.count(reg)) {
  //  self().SetNodeVal(op, self().GetNodeVal(reg) == self().GetNodeVal(val) ? TrueVal() : FalseVal());
  // } else {
  //   self().SetNodeVal(reg, self().GetNodeVal(val));
  //   self().SetNodeVal(op, TrueVal());
  // }
}

template<typename S>
void Interpreter<S>::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name() << " " << op->id();
  auto ireg{op->operands[0]};
  auto oreg{op->operands[1]};
  self().SetNodeVal(oreg, self().GetNodeVal(ireg));
  self().SetNodeVal(op, TrueVal());
  // NOTE(msurovic): See VisitRegisterCondition()
  // if (node_values.count(oreg)) {
  //   self().SetNodeVal(op,
  //              self().GetNodeVal(oreg) == self().GetNodeVal(ireg) ? TrueVal() : FalseVal());
  // } else {
  //   self().SetNodeVal(oreg, self().GetNodeVal(ireg));
  //   self().SetNodeVal(op, TrueVal());
  // }
}

template<typename S>
void Interpreter<S>::VisitCopyCondition(CopyCondition *op) {
  DLOG(INFO) << "VisitCopyCondition: " << op->Name() << " " << op->id();
  auto ireg{op->operands[0]};
  auto oreg{op->operands[1]};
  self().SetNodeVal(oreg, self().GetNodeVal(ireg));
  self().SetNodeVal(op, TrueVal());
  // NOTE(msurovic): See VisitRegisterCondition()
  // if (node_values.count(oreg)) {
  //   self().SetNodeVal(op,
  //              self().GetNodeVal(oreg) == self().GetNodeVal(ireg) ? TrueVal() : FalseVal());
  // } else {
  //   self().SetNodeVal(oreg, self().GetNodeVal(ireg));
  //   self().SetNodeVal(op, TrueVal());
  // }
}

template<typename S>
void Interpreter<S>::VisitHintCondition(HintCondition *op) {
  DLOG(INFO) << "VisitHintCondition: " << op->Name() << " " << op->id();
  auto real{op->operands[0]};
  auto hint{op->operands[1]};
  self().SetNodeVal(hint, self().GetNodeVal(real));
  self().SetNodeVal(op, TrueVal());
}

template<typename S>
void Interpreter<S>::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name() << " " << op->id();
  if (!ValidChildren(op)) {
    return self().SetNodeVal(op, FalseVal());
  }
  auto result{*self().GetNodeVal(op->operands[0])};
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result & *self().GetNodeVal(op->operands[i]);
  }
  self().SetNodeVal(op, result);
}

template<typename S>
void Interpreter<S>::VisitOnlyOneCondition(OnlyOneCondition *op) {
  auto result = 0u;
  for (std::size_t i = 0; i < op->operands.Size(); ++i) {
    result += (self().GetNodeVal(op->operands[i]) == TrueVal()) ? 1 : 0;
  }
  self().SetNodeVal(op, result == 1U ? TrueVal() : FalseVal());
}

template<typename S>
void Interpreter<S>::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name() << " " << op->id();
  self().SetNodeVal(op, self().GetNodeVal(op->operands[0]));
}