/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Shadows.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Logging.hpp>

namespace circ::shadowinst
{
    region_t Invert(region_t what, uint64_t lenght)
    {
        region_t out;
        uint64_t current = 0;
        for (auto &[from, size] : what) {
            if (current != from)
                out[current] = from - current;

            current = from + size;
        }

        if (current != lenght)
            out[current] = lenght - current;
        return out;
    }

    region_t FromToFormat(const region_t &region)
    {
        region_t out;
        for (auto &[from, size] : region)
            out[from] = from + size;
        return out;
    }

    /* Reg */

    std::string Reg::to_string(std::size_t indent, bool print_header) const
    {
        std::stringstream ss;
        std::string _indent(indent * 4, ' ');

        if (print_header)
            ss << "Reg" << std::endl;
        ss << this->has_regions::to_string(indent);
        ss << _indent << "where mapping" << std::endl;
        ss << translation_map.to_string(indent + 1);

        return ss.str();
    }

    /* Immediate */

    std::string Immediate::to_string(std::size_t indent, bool print_header) const
    {
        std::stringstream ss;
        std::string _indent(indent * 4, ' ');

        if (print_header)
            ss << "Immediate" << std::endl;
        ss << this->has_regions::to_string(indent);

        return ss.str();
    }

    /* Address */

    auto Address::flatten_significant_regs() const -> Regions
    {
        auto q = []< query::answers_queries_t V >(const V &r)
        {
            return std::make_optional(r.total_size());
        };
        auto c = query::get_combinator([](auto l, auto r) {
            return l;
        });
        auto s = this->query(query::overloaded{q, c});
        check(s);

        Regions out(*s);
        auto exec = [&](const auto &other)
        {
            for (auto kv : other.regions.areas)
                out.add(kv);
        };
        auto invoke = [&](const auto &on_what) { if (on_what) exec(*on_what); };
        invoke(base_reg());
        invoke(index_reg());
        return out;
    }

    std::string Address::to_string(std::size_t indent) const
    {
        auto make_indent = [&](auto count) { return std::string(count * 4, ' '); };

        std::stringstream ss;
        auto format = [&](const auto &what, const std::string &prefix) {
            ss << make_indent(indent) << "* " << prefix << std::endl;
            if (what)
                ss << what->to_string(indent + 1, false);
            else
                ss << make_indent(indent + 1u) << "( not set )\n";
        };

        ss << "Address\n";
        format(base_reg(), "Base");
        format(index_reg(), "Index");
        format(segment_reg(), "Segment");
        format(scale(), "Scale");
        format(displacement(), "Displacement");

        return ss.str();
    }

    // Cannot have templated code in function local `struct` defnitions.
    struct assign_
    {
        Instruction &self;
        void operator()(Reg &what)
        {
            check(!what.selector);
            what.selector = self.selectors.size();
            self.selectors.push_back(&what);
        }
        void operator()(Address &addr)
        {
            addr.for_each_present(*this);
        }
        // Fallthrough
        void operator()(auto &&) {}
    };

    void Instruction::distribute_selectors()
    {
        assign_ assign{ *this };
        for (auto &op : operands)
            op.visit(assign);

    }

    bool Instruction::operator==(const Instruction &o) const
    {
        if (o.operands != operands)
            return false;

        auto cmp_op_ctx = [](const operand_ctx_t &ctx, const operand_ctx_t &octx) {
            return std::get< 0 >(ctx) == std::get< 0 >(octx);
        };

        auto cmp_deps = [&](const auto &cluster, const auto &ocluster) {
            return std::equal(cluster.begin(), cluster.end(), ocluster.begin(), cmp_op_ctx);
        };

        return std::equal(deps.begin(), deps.end(), o.deps.begin(), cmp_deps);
    }

    bool Instruction::validate() const
    {
        uint32_t dirty_count = 0;
        for (const auto &op : operands)
          if (op.reg() && !op.reg()->dirty.empty())
            ++dirty_count;

        return dirty_count <= 1;
    }

    region_t Instruction::IdentifiedRegions() const
    {
        region_t out;
        auto collect = [&](const has_regions &x)
        {
            for (const auto &[from, size] : x.regions.areas)
            {
                dcheck(!out.count(from) || out[from] == size, [](){
                    return "Inconsistent regions."; }
                );
                out[from] = size;
            }
        };

        for_each_present(collect);
        return out;
    }

    std::string Instruction::to_string() const
    {
        std::stringstream ss;
        ss << "Shadowinst:" << std::endl;

        for (const auto &cluster : deps)
        {
            ss << "Deps cluster: [ ";
            for (const auto &[idx, _] : cluster)
                ss << idx << " ";
            ss << "]" << std::endl;
        }

        for (const auto &dirts : dirt)
        {
            ss << "dirt( ";
            for (auto d : dirts)
                ss << d << " ";
            ss << ")" << std::endl;
        }

        for (std::size_t i = 0; i < operands.size(); ++i)
        {
            ss << "OP: " << i << ": ";
            ss << operands[i].to_string(1);
        }
        ss << "(print done)" << std::endl;
        return ss.str();
    }

    std::string remove_shadowed(const Instruction &s_inst,
                                const std::string &bytes)
    {
        std::string out;
        for (std::size_t i = 0; i < bytes.size(); ++i)
            if (s_inst.present(i))
                out += bytes[bytes.size() - 1 - i];
        return out;
    }


} // namespace circ::shadowinst
