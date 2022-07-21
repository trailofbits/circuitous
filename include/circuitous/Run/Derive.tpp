/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

/* Ctx */


template<typename S>
void Derive< S >::derive_cond(Operation *op) {
    check(op->operands().size() == 2);
    auto ireg{op->operands()[0]};
    auto oreg{op->operands()[1]};

    check(oreg == cond_to_value[op]);

    this->set_node_val(oreg, this->get_node_val(ireg));
    this->set_node_val(op, this->true_val());
}


template< typename S >
void Derive< S >::derive_cond(ReadConstraint *rc_op)
{
    auto exec = [&](ReadConstraint *op) -> value_type
    {
        for (auto i = 1u; i < op->operands().size(); ++i)
            if (!this->get_node_val(op->operands()[i]))
                return {};

        auto addr = this->get_node_val(op->addr_arg())->getLimitedValue();
        auto size = this->get_node_val(op->size_arg())->getLimitedValue();

        if (!this->state->defined(addr, size)) {
            std::stringstream ss;
            ss << "Memory at " << std::hex << addr << " is not defined with size: " << size;
            log_error() << ss.str();
            this->set_node_val(op->hint_arg(), {});
            return this->false_val();
        }

        llvm::APInt val { irops::memory::size(this->circuit->ptr_size), 0, false };

        val.insertBits(this->true_val(), 0);
        val.insertBits(this->false_val(), 1);
        val.insertBits(llvm::APInt(4, op->mem_idx(), false), 8);
        val.insertBits(*this->get_node_val(op->size_arg()), 12);

        val.insertBits(*this->get_node_val(op->addr_arg()), 16);
        val.insertBits(*this->state->load(addr, size), 16 + this->circuit->ptr_size);
        val.insertBits(*this->get_node_val(op->ts_arg()), 16 + this->circuit->ptr_size * 2);

        this->set_node_val(op->hint_arg(), val);
        return this->true_val();
    }(rc_op);

    this->set_node_val(rc_op, exec);
}

template< typename S >
void Derive< S >::derive_cond(WriteConstraint *wc_op)
{
    auto exec = [&](WriteConstraint *op) -> value_type
    {
        for (auto i = 1u; i < op->operands().size(); ++i)
            if (!this->get_node_val(op->operands()[i]))
                return {};

        llvm::APInt val { irops::memory::size(this->circuit->ptr_size), 0, false };

        val.insertBits(this->true_val(), 0);
        val.insertBits(this->true_val(), 1);
        val.insertBits(llvm::APInt(4, op->mem_idx(), false), 8);
        val.insertBits(*this->get_node_val(op->size_arg()), 12);

        val.insertBits(*this->get_node_val(op->addr_arg()), 16);
        val.insertBits(*this->get_node_val(op->val_arg()), 16 + 64);
        val.insertBits(*this->get_node_val(op->ts_arg()), 16 + 128);

        this->set_node_val(op->hint_arg(), val);
        return this->true_val();
    }(wc_op);

    if (exec) {
        auto addr = this->get_node_val(wc_op->addr_arg())->getLimitedValue();
        auto value = *this->get_node_val(wc_op->val_arg());
        this->state->store(addr, value);
    }
    this->set_node_val(wc_op, exec);
}

template< typename S >
void Derive< S >::derive_cond(UnusedConstraint *uu_op)
{
    llvm::APInt unused { irops::memory::size(this->circuit->ptr_size), 0, false };

    this->set_node_val(uu_op, this->true_val());
    this->set_node_val(uu_op->operands()[0], unused);
}

/* Input/Output nodes */

template<typename S>
void IOSem<S>::visit(InputRegister *op) {
    check(this->state->has_value(op)) << "Input register "
                                        << op->reg_name << " bits not set.";
}

template<typename S>
void IOSem<S>::visit(InputImmediate *op) {
    check(op->operands().size() == 1)
        << "Incorrect number of operands() of InputImmediate:"
        << op->operands().size() << "!= 1";
    this->set_node_val(op, this->get_node_val(op->operands()[0]));
}

template<typename S>
void IOSem<S>::visit(OutputRegister *op) {
    unreachable() << "Output values should not be visited in derivation mode.";
}

template<typename S>
void IOSem<S>::visit(InputErrorFlag *op) {
    check(this->state->has_value(op)) << "InputErrorFlag bits not set.";
}

template<typename S>
void IOSem<S>::visit(OutputErrorFlag *op) {
    unreachable() << "Output values should not be visited in derivation mode.";
}

template<typename S>
void IOSem<S>::visit(InputInstructionBits *op) {
    check(this->state->has_value(op)) << "Input instruction bits not set.";
}

template<typename S>
void IOSem<S>::visit(Advice *op) {
    unreachable() << "Output values should not be visited in derivation mode.";
}

/* Conditions */

template<typename S>
void CSem<S>::visit(DecodeCondition *op) {
    auto decode = [&](DecodeCondition *op) {
        return this->value(this->get_node_val( op, 0 ) == this->get_node_val( op, 1 ) );
    };
    return safe(op, decode);
}

template<typename S>
void CSem<S>::visit(ReadConstraint *op_)
{
    auto exec = [&](ReadConstraint *op) -> value_type
    {
        for (auto i = 1u; i < op->operands().size(); ++i)
            if (!this->get_node_val(op->operands()[i]))
                return {};

        check(this->has_value(op->hint_arg()));
        auto parsed = Memory::deconstruct(*this->get_node_val(op->hint_arg()),
                                          this->circuit->ptr_size);

        irops::memory::Parsed< llvm::APInt > args {
            this->circuit->ptr_size,
                {this->true_val(),
                    this->false_val(),
                    llvm::APInt(6, 0, false),
                    parsed.id(),
                    *this->get_node_val(op->size_arg()),
                    *this->get_node_val(op->addr_arg()),
                    parsed.value(),
                    *this->get_node_val(op->ts_arg())}
        };

        return this->value(args == parsed);

    }(op_);

    this->set_node_val(op_, exec);
}

template<typename S>
void CSem<S>::visit(WriteConstraint *op_)
{
    auto exec = [&](WriteConstraint *op) -> value_type
    {
        for (auto i = 1u; i < op->operands().size(); ++i)
            if (!this->get_node_val(op->operands()[i]))
                return {};

        check(this->has_value(op->hint_arg()));
        auto parsed = Memory::deconstruct(*this->get_node_val(op->hint_arg()),
                                          this->circuit->ptr_size);

        irops::memory::Parsed< llvm::APInt > args {
            this->circuit->ptr_size,
                {this->true_val(),
                    this->false_val(),
                    llvm::APInt(6, 0, false),
                    parsed.id(),
                    *this->get_node_val(op->size_arg()),
                    *this->get_node_val(op->addr_arg()),
                    *this->get_node_val(op->val_arg()),
                    *this->get_node_val(op->ts_arg())}
        };

        return this->value(args == parsed);
    }(op_);

    if (exec) {
        auto addr = this->get_node_val(op_->addr_arg())->getLimitedValue();
        auto value = *this->get_node_val(op_->val_arg());
        this->state->store(addr, value);
    }

    this->set_node_val(op_, exec);
}

template<typename S>
void CSem<S>::visit(UnusedConstraint *op)
{
    llvm::APInt unused { irops::memory::size(this->circuit->ptr_size), 0, false };
    this->set_node_val(op, this->value(this->get_node_val(op, 0) == llvm::APInt(unused)));
}

template< typename S >
void CSem< S >::visit(RegConstraint *op)
{
    // TODO(lukas): Until better system of undef handling is implemented
    //              if some value is undefined == no change happened.
    if (allows_undef(op) && (!this->get_node_val(op, 0) || !this->get_node_val(op, 1))) {
        // If undef bubbles up here, and test specified that reg is undef,
        // compare values.
        if (!this->get_node_val(op, 0) && !this->get_node_val(op, 1)) {
            this->set_node_val(op, this->value(true));
            return;
        }

        auto name = op->operands()[1]->name();
        auto as_ref = llvm::StringRef(name);
        auto has_prefix = as_ref.consume_front("Out.register.");
        check(has_prefix);
        auto in = this->circuit->template fetch_reg< InputRegister, false >(as_ref.str());
        this->set_node_val(op, this->value(this->get_node_val(in) == this->get_node_val(op, 1)));
        return;
    }
    this->set_node_val(op, this->value(this->get_node_val(op, 0) == this->get_node_val(op, 1)));
}

template< typename S >
void CSem< S >::visit(AdviceConstraint *op) { return not_implemented(); }

template<typename S>
void CSem<S>::visit(VerifyInstruction *op)
{
    auto verify = [&](VerifyInstruction *op) {
        auto result = this->true_val();
        for (auto child : op->operands()) {
            result &= *this->get_node_val(child);
        }
        return result;
    };
    safe(op, verify);
}

template<typename S>
void CSem<S>::visit(OnlyOneCondition *op)
{
    auto xor_ = [&](OnlyOneCondition *op) {
        auto result = 0u;
        for (std::size_t i = 0; i < op->operands().size(); ++i) {
            result += uint32_t(this->get_node_val(op->operands()[i]) == this->true_val());
        }
        return this->value(result == 1U);
    };
    safe(op, xor_);
}
