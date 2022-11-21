/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Instruction.hpp>

#include <circuitous/Util/InstructionBytes.hpp>

namespace circ
{

    InstructionBatch::InstructionBatch(Ctx &ctx) : parent_t(ctx) {}

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::vector< remill::Instruction > &rinsts)
        : parent_t(ctx)
    {

    }

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::vector< InstBytes > &instbytes)
        : parent_t(ctx)
    {

    }

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::string &raw_bytes)
        : parent_t(ctx)
    {

    }

    auto InstructionBatch::add(InstructionBatch &&other) -> self_t &
    {
        insts.insert(insts.end(),
                     std::make_move_iterator(other->begin()),
                     std::make_move_iterator(other->end()));
        return *this;
    }

    auto InstructionBatch::add(InstructionInfo &&info) -> self_t &
    {
        insts.push_back(std::move(info));
        return *this;
    }

    auto InstructionBatch::add(raw_insts_t &&rinsts) -> self_t &
    {
        for (auto &&rinst : rinsts)
            insts.emplace_back(std::move(rinst));
        return *this;
    }

    auto InstructionBatch::fuzz() -> self_t &
    {
        log_info() << categories();
        for (auto &info : insts)
            if (!info.has_shadow())
                info.make_fuzz(this->ctx);
        return *this;
    }

    std::string InstructionBatch::categories() const
    {
        // reg, addr, imm
        using input_t = std::tuple< std::size_t, std::size_t, std::size_t >;
        // read, write
        using inputs_t = std::tuple< input_t, input_t >;
        // type -> count
        using rinsts_t = std::vector< const remill::Instruction * >;
        using entry = std::tuple< inputs_t, rinsts_t >;

        std::vector< entry > counts;

        auto fetch = [ & ]( const auto &what, const remill::Instruction *rinst )
        {
            for ( auto &[ present, count ] : counts )
                if ( present == what )
                {
                    count.push_back( rinst );
                    return;
                }
            counts.emplace_back( what, rinsts_t{ rinst } );
        };

        for ( const auto &inst : insts )
        {
            auto &rinst = inst.rinst();

            inputs_t args = { { 0, 0, 0 }, { 0, 0, 0 } };
            for ( const auto &op : rinst.operands )
            {
                auto add = [ & ]( auto &where )
                {
                    if ( op.type == remill::Operand::kTypeRegister )
                        std::get< 0 >( where ) += 1;
                    else if ( op.type == remill::Operand::kTypeAddress )
                        std::get< 1 >( where ) += 1;
                    else if ( op.type == remill::Operand::kTypeImmediate )
                        std::get< 2 >( where ) += 1;
                    else
                        log_kill() << "Operand is neither reg, addr or imm.";
                };

                if ( op.action == remill::Operand::kActionRead )
                    add( std::get< 0 >( args ) );
                else if ( op.action == remill::Operand::kActionWrite)
                    add( std::get< 1 >( args ) );
                else
                    log_kill() << "Instruction is neither read or write.";

            }

            fetch( args, &rinst );
        }

        std::stringstream ss;
        for ( const auto &[ present, count ] : counts )
        {
            ss << "[type]: ";
            const auto &[ r, w ] = present;
            auto fmt = [ & ]( const auto &from )
            {
                const auto &[ reg, addr, imm ] = from;
                ss << "( " << reg << " " << addr << " " << imm << " )";
            };
            fmt( r );
            ss << " ";
            fmt( w );
            ss << " -> " << count.size() << "\n";
            for ( const auto &rinst_ptr : count )
                ss << "\t" << rinst_ptr->function << "\n";
        }
        return ss.str();
    }

    //void OpSelections::allocate( const InstructionBatch &batch )
    //{

    //}

} // namespace circ
