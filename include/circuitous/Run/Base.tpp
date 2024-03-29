/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

template<typename S>
void OpSem<S>::visit(Constant *op)
{
    std::string bits{op->bits.rbegin(), op->bits.rend()};
    this->set_node_val(op, llvm::APInt(op->size, bits, /*radix=*/2U));
}

template<typename S>
void OpSem<S>::visit(Undefined *op)
{
    if (!this->state->has_value(op)) {
        this->set_node_val(op, this->undef());
    }
}

template<typename S>
void OpSem<S>::visit(Extract *op)
{
    auto extract = [&](Extract *op) {
        auto pos{ op->low_bit_inc };
        auto num{ op->high_bit_exc - pos };
        return this->get_node_val(op, 0)->extractBits(num, pos);
    };
    safe(op, extract);
}

// TODO(lukas): Some non-align cases most likely need some extra handling
//              which is currently not happening? Investigate.
template<typename S>
void OpSem<S>::visit(Concat *op)
{
    auto concat = [&](Concat *op) {
        llvm::APInt build{ op->size, 0, false };
        auto current = 0u;
        for (auto i = 0u; i < op->operands_size(); ++i) {
            build.insertBits( *( this->get_node_val(op, i) ), current );
            current += op->operand(i)->size;
        }
        return build;
    };
    safe(op, concat);
}

template<typename S>
void OpSem<S>::visit(Not *op)
{
    auto negate = [&](Not *op) {
        // NOTE(lukas): To avoid confusion the copy is here explicitly, since `negate` does
        //              change the APInt instead of returning a new one.
        llvm::APInt copy { *this->get_node_val(op, 0) };
        copy.negate();
        return copy;
    };
    safe(op, negate);
}

template<typename S>
void OpSem<S>::visit(Select *op)
{
    if (!this->get_node_val(op, 0)) {
        return this->set_node_val(op, {});
    }
    auto selector = this->get_node_val( op->operand( 0 ) );
    auto chosen = this->get_node_val( op->operand( selector->getLimitedValue() + 1 ) );
    return this->set_node_val(op, chosen);
}

template<typename S>
void OpSem<S>::visit(Parity *op)
{
    auto parity = [&](Parity *op) {
        return llvm::APInt(1, this->get_node_val( op, 0 )->countPopulation() % 2 );
    };
    return safe(op, parity);
}

template<typename S>
void OpSem<S>::visit(PopulationCount *op)
{
    auto popcount = [&](PopulationCount *op) {
        return llvm::APInt(op->size, this->get_node_val(op, 0)->countPopulation());
    };
    return safe(op, popcount);
}

template<typename S>
void OpSem<S>::visit(Or *op)
{
    auto or_ = [&](Or *op_)
    {
        auto out = *this->get_node_val(op_->operand(0));
        for (std::size_t i = 1; i < op_->operands_size(); ++i)
            out |= *this->get_node_val(op_->operand(i));
        return out;
    };
    return safe(op, or_);
}

template<typename S>
void OpSem<S>::visit(And *op)
{
    auto and_ = [&](And *op_)
    {
        auto out = *this->get_node_val(op_->operand(0));
        for (std::size_t i = 1; i < op_->operands_size(); ++i)
            out &= *this->get_node_val(op_->operand(i));
        return out;
    };
    return safe(op, and_);
}

// TODO(lukas): Merge with `And`.
template<typename S>
void OpSem<S>::visit(DecoderResult *op)
{
    auto and_ = [&](DecoderResult *op_) {
        for (const auto &child : op_->operands()) {
            if (this->get_node_val(child) == this->false_val()) {
                return this->false_val();
            }
        }
        return this->true_val();
    };
    return safe(op, and_);
}

template< typename S >
void OpSem< S >::visit( Switch *op )
{
    auto val = [ & ]() -> value_type
    {
        for ( auto option : dyn_cast< Option >( op->operands() ) )
        {
            auto sel = this->compute_or( option->conditions(), this->false_val() );
            if ( sel == this->true_val() )
                return this->get_node_val( option->value() );
        }

        return this->zero( op->size );
    }();

    this->set_node_val( op, val );
}

template< typename S >
void OpSem< S >::visit( Option *op )
{
    auto val = [ & ]() -> value_type
    {
        auto is_selected = this->compute_or( op->conditions(), this->false_val() );
        if ( !is_selected )
            return {};

        if ( *is_selected == this->false_val() )
            return llvm::APInt{ op->size, 0, false };

        return this->get_node_val( op->value() );
    }();

    this->set_node_val( op, val );
}
