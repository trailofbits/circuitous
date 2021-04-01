/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
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
    has_regions(const region_t &others) : regions(others) {}
    has_regions(std::vector<bool> &bits) {
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
        LOG(INFO) << "ADDING: " << from << " " << size;
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
  };

  struct Reg : has_regions {
    // Actual instances for given reg
    using materialized_t = std::vector<bool>;
    using materializations_t = std::unordered_set<materialized_t>;
    using reg_t = std::string;

    using has_regions::has_regions;

    std::unordered_map<reg_t, materializations_t> translation_map;
  };

  struct Immediate : has_regions {
    using has_regions::has_regions;
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
  };

  struct Instruction {
    std::vector<Operand> operands;

    template<typename T, typename ...Args>
    void Add(Args && ... args) {
      operands.push_back(Operand::As<T>(std::forward<Args>(args)...));
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
          for (auto [from, size] : op.reg->regions) {
            ss << "    " << from << " , " << size << std::endl;
          }
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