/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

/* Ctx */

template<typename S>
void Ctx_<S>::init() {
  init<OutputRegister, OutputErrorFlag>();
  S::init();
}

template<typename S>
void Ctx_<S>::derive_cond(Operation *op) {
  auto ireg{op->operands[0]};
  auto oreg{op->operands[1]};

  self().SetNodeVal(oreg, self().GetNodeVal(ireg));
  self().SetNodeVal(op, this->TrueVal());
}

template<typename S>
void Ctx_<S>::verify_cond(Operation *op) {
  CHECK(op->operands.size() == 2);
  self().SetNodeVal(op, this->BoolVal(this->get(op, 0) == this->get(op, 1)));
}

/* Input/Output nodes */

template<typename S>
void IOSem<S>::Visit(InputRegister *op) {
  CHECK(this->node_values.count(op)) << "Input register " << op->reg_name << " bits not set.";
}

template<typename S>
void IOSem<S>::Visit(InputImmediate *op) {
  CHECK(op->operands.Size() == 1)
    << "Incorrect number of operands of InputImmediate:"
    << op->operands.Size() << "!= 1";
  self().SetNodeVal(op, self().GetNodeVal(op->operands[0]));
}

template<typename S>
void IOSem<S>::Visit(OutputRegister *op) {
  LOG(FATAL) << "Output values should not be visited in derivation mode.";
}

template<typename S>
void IOSem<S>::Visit(InputErrorFlag *op) {
  CHECK(this->node_values.count(op)) << "InputErrorFlag bits not set.";
}

template<typename S>
void IOSem<S>::Visit(OutputErrorFlag *op) {
  LOG(FATAL) << "Output values should not be visited in derivation mode.";
}

template<typename S>
void IOSem<S>::Visit(InputInstructionBits *op) {
  CHECK(this->node_values.count(op)) << "Input instruction bits not set.";
}

template<typename S>
void IOSem<S>::Visit(Advice *op) {
  LOG(FATAL) << "Output values should not be visited in derivation mode.";
}

/* Conditions */

template<typename S>
void CSem<S>::Visit(DecodeCondition *op) {
  auto decode = [&](DecodeCondition *op) {
    return this->BoolVal( self().get( op, 0 ) == self().get( op, 1 ) );
  };
  return safe(op, decode);
}

template<typename S>
void CSem<S>::Visit(ReadConstraint *op) { return this-> handle_cond(op); }

template<typename S>
void CSem<S>::Visit(WriteConstraint *op) { return this-> handle_cond(op); }

template<typename S>
void CSem<S>::Visit(RegConstraint *op) { return this-> handle_cond(op); }

template<typename S>
void CSem<S>::Visit(PreservedConstraint *op) { return this->handle_cond(op); }

template<typename S>
void CSem<S>::Visit(CopyConstraint *op) { return this->handle_cond(op); }

template<typename S>
void CSem<S>::Visit(AdviceConstraint *op) { return this->handle_cond(op); }

template<typename S>
void CSem<S>::Visit(VerifyInstruction *op) {
  auto verify = [&](VerifyInstruction *op) {
    auto result = this->TrueVal();
    for (auto child : op->operands) {
      result &= *this->get(child);
    }
    return result;
  };
  safe(op, verify);
}

template<typename S>
void CSem<S>::Visit(OnlyOneCondition *op) {
  auto xor_ = [&](OnlyOneCondition *op) {
    auto result = 0u;
    for (std::size_t i = 0; i < op->operands.Size(); ++i) {
      result += uint32_t(self().GetNodeVal(op->operands[i]) == this->TrueVal());
    }
    return this->BoolVal(result == 1U);
  };
  safe(op, xor_);
}