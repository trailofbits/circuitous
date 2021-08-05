/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

/* Ctx */

template<typename S>
void Ctx_<S>::init() {
  init<OutputRegister, OutputErrorFlag, OutputTimestamp>();
  S::init();
}

template<typename S>
void Ctx_<S>::derive_cond(Operation *op) {
  CHECK(op->operands.size() == 2);
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
void CSem<S>::Visit(ReadConstraint *op_) {

  auto exec = [&](ReadConstraint *op) -> value_type {
    for (auto i = 1u; i < op->operands.Size(); ++i) {
      if (!this->GetNodeVal(op->operands[i])) {
        return {};
      }
    }

    // Is hint supplied?
    if (this->supplied.count(op->hint_arg())) {
      CHECK(this->has_value(op->hint_arg()));
      auto parsed = this->deconstruct(*this->get(op->hint_arg()));

      irops::memory::Parsed< llvm::APInt > args {
        this->TrueVal(),
        this->FalseVal(),

        parsed.id,
        *this->get(op->size_arg()),
        *this->get(op->addr_arg()),
        parsed.value,
        *this->get(op->ts_arg())
      };

      return this->BoolVal(args == parsed);
    }

    auto addr = this->get(op->addr_arg())->getLimitedValue();
    auto size = this->get(op->size_arg())->getLimitedValue();

    if (!this->defined(addr, size)) {
      std::stringstream ss;
      ss << "Memory at " << std::hex << addr << " is not defined with size: " << size;
      LOG(WARNING) << ss.str();
      self().SetNodeVal(op->hint_arg(), {});
      return this->FalseVal();
    }

    llvm::APInt val { irops::Memory::allocated_size, 0, false };

    val.insertBits(this->TrueVal(), 0);
    val.insertBits(this->FalseVal(), 1);
    val.insertBits(llvm::APInt(4, op->mem_idx(), false), 8);
    val.insertBits(*this->get(op->size_arg()), 12);

    val.insertBits(*this->get(op->addr_arg()), 16);
    val.insertBits(*this->load(addr, size), 16 + 64);
    val.insertBits(*this->get(op->ts_arg()), 16 + 128);

    self().SetNodeVal(op->hint_arg(), val);
    return this->TrueVal();
  }(op_);

  if (!exec && !this->supplied.count(op_->hint_arg())) {
    self().SetNodeVal(op_->hint_arg(), {});
  }
  self().SetNodeVal(op_, exec);
}

template<typename S>
void CSem<S>::Visit(WriteConstraint *op_) {
  auto exec = [&](WriteConstraint *op) -> value_type {
    for (auto i = 1u; i < op->operands.Size(); ++i) {
      if (!this->GetNodeVal(op->operands[i])) {
        return {};
      }
    }

    // Is hint supplied?
    if (this->supplied.count(op->hint_arg())) {
      CHECK(this->has_value(op->hint_arg()));
      auto parsed = this->deconstruct(*this->get(op->hint_arg()));

      irops::memory::Parsed< llvm::APInt > args {
        this->TrueVal(),
        this->FalseVal(),

        parsed.id,
        *this->get(op->size_arg()),
        *this->get(op->addr_arg()),
        *this->get(op->val_arg()),
        *this->get(op->ts_arg())
      };

      return this->BoolVal(args == parsed);
    }

    llvm::APInt val { irops::Memory::allocated_size, 0, false };

    val.insertBits(this->TrueVal(), 0);
    val.insertBits(this->TrueVal(), 1);
    val.insertBits(llvm::APInt(4, op->mem_idx(), false), 8);
    val.insertBits(*this->get(op->size_arg()), 12);

    val.insertBits(*this->get(op->addr_arg()), 16);
    val.insertBits(*this->get(op->val_arg()), 16 + 64);
    val.insertBits(*this->get(op->ts_arg()), 16 + 128);

    self().SetNodeVal(op->hint_arg(), val);
    return this->TrueVal();
  }(op_);

  if (exec) {
    auto addr = this->get(op_->addr_arg())->getLimitedValue();
    auto value = *this->get(op_->val_arg());
    this->store(addr, value);
  }

  if (!exec && !this->supplied.count(op_->hint_arg())) {
    self().SetNodeVal(op_->hint_arg(), {});
  }

  self().SetNodeVal(op_, exec);
}

template<typename S>
void CSem<S>::Visit(UnusedConstraint *op) {
  llvm::APInt unused { irops::Memory::allocated_size, 0, false };
  if (this->supplied.count(op->operands[0])) {
    self().SetNodeVal(op, this->BoolVal(this->get(op, 0) == llvm::APInt(unused)));
    return;
  }

  self().SetNodeVal(op, this->TrueVal());
  self().SetNodeVal(op->operands[0], unused);
}


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