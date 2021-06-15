/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

template<typename S>
void Base_<S>::SetNodeVal(Operation *op, const value_type &val) {
  this->node_values[op] = val;
}

template<typename S>
auto Base_<S>::GetNodeVal(Operation *op) const -> value_type {
  auto iter{this->node_values.find(op)};
  CHECK(iter != this->node_values.end()) << op->Name();
  return iter->second;
}

template<typename S>
void Base_<S>::Visit(Operation *op) {
  LOG(FATAL) << "Unhandled operation: " << op->Name() << " " << op->id();
}

template<typename S>
void OpSem<S>::Visit(Constant *op) {
  std::string bits{op->bits.rbegin(), op->bits.rend()};
  self().SetNodeVal(op, llvm::APInt(op->size, bits, /*radix=*/2U));
}

template<typename S>
void OpSem<S>::Visit(Undefined *op) {
  // TODO(surovic): See Visit()
  if (!this->node_values.count(op)) {
    self().SetNodeVal(op, this->Undef());
  }
}

template<typename S>
void OpSem<S>::Visit(Extract *op) {
  auto extract = [&](Extract *op) {
    auto pos{ op->low_bit_inc };
    auto num{ op->high_bit_exc - pos };
    return self().get(op, 0)->extractBits(num, pos);
  };
  safe(op, extract);
}

// TODO(lukas): Some non-align cases most likely need some extra handling
//              which is currently not happening? Investigate.
template<typename S>
void OpSem<S>::Visit(Concat *op) {

  auto concat = [&](Concat *op) {
    llvm::APInt build{ op->size, 0, false };
    auto current = 0u;
    for (auto i = 0u; i < op->operands.Size(); ++i) {
        build.insertBits( *( self().get(op, i) ), current );
        current += (*op)[i]->size;
    }
    return build;
  };
  safe(op, concat);
}

template<typename S>
void OpSem<S>::Visit(Not *op) {

  auto negate = [&](Not *op) {
    // NOTE(lukas): To avoid confusion the copy is here explicitly, since `negate` does
    //              change the APInt instead of returning a new one.
    llvm::APInt copy { *self().get(op, 0) };
    copy.negate();
    return copy;
  };
  safe(op, negate);
}

template<typename S>
void OpSem<S>::Visit(Select *op) {
  auto select = [&](Select *op) {
    auto selector = self().get( ( *op )[ 0 ] );
    return self().get( ( *op )[ selector->getLimitedValue() + 1 ] );
  };
  return safe(op, select);
}

template<typename S>
void OpSem<S>::Visit(Parity *op) {
  auto parity = [&](Parity *op) {
    return llvm::APInt(1, self().get( op, 0 )->countPopulation() % 2 );
  };
  return safe(op, parity);
}

template<typename S>
void OpSem<S>::Visit(PopulationCount *op) {
  auto popcount = [&](PopulationCount *op) {
    return llvm::APInt(op->size, self().get(op, 0)->countPopulation());
  };
  return safe(op, popcount);
}

template<typename S>
void OpSem<S>::Visit(Or *op) {
  auto or_ = [&](Or *or_) {
    for (const auto &child : or_->operands) {
      if (self().get(child) == this->TrueVal()) {
        return this->TrueVal();
      }
    }
    return this->FalseVal();
  };
  return safe(op, or_);
}

template<typename S>
void OpSem<S>::Visit(And *op) {
  auto and_ = [&](And *or_) {
    for (const auto &child : or_->operands) {
      if (self().get(child) == this->FalseVal()) {
        return this->FalseVal();
      }
    }
    return this->TrueVal();
  };
  return safe(op, and_);
}

template<typename S>
void Base_<S>::Visit(Circuit *op) {
  self().SetNodeVal(op, self().GetNodeVal(op->operands[0]));
}

template<typename S>
void Base_<S>::set_input_state(const trace::Entry &in) {
  auto inst_bits = circuit->input_inst_bits();
  self().SetNodeVal(inst_bits, in.get_inst_bits(inst_bits->size));

  self().SetNodeVal(circuit->input_ebit(), in.get_ebit());
  for (auto &[name, val] : in.regs) {
    if (auto reg = circuit->input_reg(name)) {
      self().SetNodeVal(reg, llvm::APInt(reg->size, val));
    }
  }
}

template<typename S>
void Base_<S>::set_output_state(const trace::Entry &out) {
  self().SetNodeVal(circuit->output_ebit(), out.get_ebit());
  for (auto &[name, val] : out.regs) {
    if (auto reg = circuit->output_reg(name)) {
      self().SetNodeVal(reg, llvm::APInt(reg->size, val));
    }
  }
}

template<typename S>
bool Base_<S>::get_result() const {
  return get(circuit) == TrueVal();
}

template<typename S>
trace::Entry Base_<S>::get_output_state() const {
  trace::Entry out;

  for (auto op : circuit->Attr<OutputRegister>()) {
    out.regs[op->reg_name] = get(op)->getLimitedValue();
  }
  out.ebit = get(circuit->output_ebit()) == TrueVal();
  return out;
}