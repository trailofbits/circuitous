/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <remill/Arch/Instruction.h>

namespace circuitous::shadowinst {

  static inline std::string to_binary(const std::string &bytes) {
    std::stringstream ss;
    for (char byte_ : bytes) {
      auto byte = static_cast<uint8_t>(byte_);
      for (int b = 7; b >= 0; --b) {
        ss << static_cast<uint16_t>(byte >> b & 1u);
      }
    }
    return ss.str();
  }

  static inline std::string to_hex(const std::string &bytes) {
    std::stringstream ss;
    for (auto byte : bytes) {
      ss << std::hex
         << static_cast<unsigned>(static_cast<uint8_t>(byte));
    }
    return ss.str();
  }

  // This describes a region of decoded bytes where
  // it makes sense to talk about an "entity" -- register, immediate operand,
  // etc. While it will usually be one contignous entry, we don't need to constraint
  // ourselves yet.
  // {from, size}
  using region_t = std::map<uint64_t, uint64_t>;
  using maybe_region_t = std::optional<region_t>;


  static inline region_t Invert(region_t what, uint64_t lenght) {
    region_t out;
    uint64_t current = 0;
    for (auto &[from, size] : what) {
      if (current != from) {
        out[current] = from - current;
      }
      current = from + size;
    }

    if (current != lenght) {
      out[current] = lenght - current;
    }
    return out;
  }

  static inline region_t FromToFormat(const region_t &region) {
    region_t out;
    for (auto &[from, size] : region) {
      out[from] = from + size;
    }
    return out;
  }

  struct has_regions {
    region_t regions;

    has_regions() = default;

    has_regions(const std::vector<bool> &bits) {
      for (std::size_t i = 0; i < bits.size(); ++i) {
        if (!bits[i]) {
          continue;
        }
        uint64_t offset = i;
        uint32_t count = 0;
        for(; i < bits.size() && bits[i]; ++i) {
          ++count;
        }

        regions.emplace(offset, count);
      }
    }

    std::size_t region_bitsize() const {
      std::size_t acc = 0;
      for (auto &[from, size] : regions) {
        acc += size;
      }
      return acc;
    }

    std::vector<uint64_t> region_idxs() const {
      std::vector<uint64_t> idxs;
      for (auto &[from, size] : regions) {
        for (uint64_t i = 0; i < size; ++i) {
          idxs.push_back(from + i);
        }
      }
      return idxs;
    }

    auto begin() const { return regions.begin(); }
    auto end() const { return regions.end(); }
    auto size() const { return regions.size(); }
    auto empty() const { return size() == 0; }

    std::string to_string(uint8_t indent=0) const {
      std::stringstream ss;
      for (auto [from, size] : regions) {
        ss << std::string(indent * 2, ' ') << from << " , " << size << std::endl;
      }
      return ss.str();
    }
  };

  struct Reg : has_regions {
    // Actual instances for given reg
    using materialized_t = std::vector<bool>;
    using materializations_t = std::unordered_set<materialized_t>;
    using reg_t = std::string;

    using has_regions::has_regions;

    std::unordered_map<reg_t, materializations_t> translation_map;

    std::string to_string(uint8_t indent=0) const {
      std::stringstream ss;
      std::string _indent(indent * 2, ' ');
      ss << _indent << "Regions:" << std::endl;
      ss << this->has_regions::to_string(indent + 1);
      ss << _indent << "Translation map:" << std::endl;
      for (auto &[reg, all_mats] : translation_map) {
        ss << std::string((indent + 1) * 2, ' ' ) << reg << std::endl;
        for (auto &mat : all_mats) {
          ss << std::string((indent + 2) * 2, ' ');
          if (mat.empty()) {
            ss << "( none )";
          } else {
            for (auto b : mat) {
              ss << b;
            }
          }
          ss << std::endl;
        }
      }
      return ss.str();
    }
  };

  struct Immediate : has_regions {
    using has_regions::has_regions;

    Immediate &operator=(Immediate other) {
      std::swap(regions, other.regions);
      return *this;
    }
  };

  struct Shift : has_regions {
    using has_regions::has_regions;
  };

  struct Address : has_regions {
    using has_regions::has_regions;
  };

  struct Operand {
    using op_type = remill::Operand::Type;

    std::optional<Immediate> immediate;
    std::optional<Reg> reg;
    maybe_region_t shift;
    maybe_region_t address;

    template<typename T, typename ... Args>
    static auto As(Args && ... args) {
      static_assert(std::is_same_v<T, Immediate> || std::is_same_v<T, Reg>);

      Operand op;
      if constexpr (std::is_same_v<T, Immediate>) {
        op.immediate = Immediate(std::forward<Args>(args)...);
      } else if (std::is_same_v<T, Reg>) {
        op.reg = Reg(std::forward<Args>(args)...);
      }
      return op;
    }

    bool IsHusk() const {
      // No operand is specified, therefore this is not a husk but a hardcoded op
      if (!reg && !immediate && !shift && !address) {
        return false;
      }
      CHECK(!address.has_value()) << "Cannot handle address";
      CHECK(!shift.has_value()) << "Cannot handle shift";
      CHECK(  static_cast<uint8_t>(reg.has_value())
            + static_cast<uint8_t>(immediate.has_value())
            + static_cast<uint8_t>(address.has_value())
            + static_cast<uint8_t>(shift.has_value())
      ) << "shadowinst::operand is of multiple types!";

      return (reg && reg->empty())     || (immediate && immediate->empty()) ||
             (shift && shift->empty()) || (address && address->empty());
    }

    bool empty() const {
      auto is_empty = [](const auto &op) {
        return op && op->size();
      };
      return is_empty(immediate) && is_empty(reg);
    }
  };

  struct Instruction {
    // We need to fullfil that is pointers to operands are never invalidated!
    // TODO(lukas): See if ^ cannot be relaxed, as it is a propbable source of errors.
    std::deque<Operand> operands;

    auto size() const { return operands.size(); }
    const auto &operator[](std::size_t idx) const { return operands[idx]; }
    auto &operator[](std::size_t idx) { return operands[idx]; }

    template<typename T, typename ...Args>
    auto &Add(Args && ... args) {
      return operands.emplace_back(Operand::As<T>(std::forward<Args>(args)...));
    }

    template<typename ...Args>
    void Replace(std::size_t idx, remill::Operand::Type type, Args && ...args) {
      switch(type) {
        case remill::Operand::Type::kTypeRegister: {
          operands[idx] = Operand::As<Reg>(std::forward<Args>(args)...);
          break;
        }
        case remill::Operand::Type::kTypeImmediate : {
          operands[idx] = Operand::As<Immediate>(std::forward<Args>(args)...);
          break;
        }
        default :
          LOG(FATAL)
            << "Cannot replace shadow operand with type that is neither reg nor imm.";
      }
    }

    region_t IdentifiedRegions() const {
      region_t out;

      for (const auto &op : operands) {
        // TODO(lukas): Sanity check may be in order here
        if (op.immediate) {
          for (auto [from, size] : *op.immediate) {
            out[from] = size;
          }
        }
        // TODO(lukas): Looks like copy pasta, but eventually these
        //              attributes will have different structure
        if (op.reg) {
          for (auto [from, size] : op.reg->regions) {
            out[from] = size;
          }
        }
        if (op.shift) {
          for (auto [from, size] : *op.shift) {
            out[from] = size;
          }
        }
        if (op.address) {
          for (auto [from, size] : *op.address) {
            out[from] = size;
          }
        }
      }
      return out;
    }

    // We need lenght of the entire region to be able to calculate last region
    region_t UnknownRegions(uint64_t lenght) const {
      return Invert(IdentifiedRegions(), lenght);
    }

    std::string to_string() const {
      std::stringstream ss;
      ss << "Shadowinst:" << std::endl;
      for (const auto &op : operands) {
        ss << " OP" << std::endl;
        // TODO(lukas): Sanity check may be in order here
        if (op.immediate) {
          ss << "  Immediate:" << std::endl;
          for (auto [from, size] : *op.immediate) {
            ss << "    " << from << " , " << size << std::endl;;
          }
        }
        // TODO(lukas): Looks like copy pasta, but eventually these
        //              attributes will have different structure
        if (op.reg) {
          ss << "  Reg:" << std::endl;
          ss << op.reg->to_string(2);
        }
        if (op.shift) {
          ss << "  Shift:" << std::endl;
          for (auto [from, size] : *op.shift) {
            LOG(INFO) << " " << from << " , " << size << std::endl;
          }
        }
        if (op.address) {
          ss << "  Address" << std::endl;
          for (auto [from, size] : *op.address) {
            ss << "    " << from << " , " << size << std::endl;
          }
        }
      }
      return ss.str();
    }

  };

} // namespace circuitous::shadowinst