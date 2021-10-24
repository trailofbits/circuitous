/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

namespace circ::ifuzz::permutate {


  enum class diff_result : uint32_t
  {
    pure = 0, dirty = 1, unrelated = 2, exact = 3, unknown = 4
  };

  static inline std::string to_string(const diff_result &a) {
    switch (a) {
      case diff_result::pure:      return "pure";
      case diff_result::dirty:     return "dirty";
      case diff_result::unrelated: return "unrelated";
      case diff_result::exact:     return "exact";
      case diff_result::unknown:   return "unknown";
      default: LOG(FATAL) << "Unreachable.";
    }
  }

  static inline diff_result join(const diff_result &a, const diff_result &b) {
    if (a == b)
      return a;
    if (a == diff_result::exact || b == diff_result::exact)
      return diff_result::exact;

    if (a == diff_result::dirty || b == diff_result::dirty)
      return diff_result::dirty;

    if (a == diff_result::unknown) return b;
    if (b == diff_result::unknown) return a;

    if (a == diff_result::pure) return a;
    if (b == diff_result::pure) return b;

    LOG(FATAL) << "Cannot join " << static_cast< uint32_t >(a) << " with "
               << static_cast< uint32_t >(b);
  }

  using struct_icheck_res_t = std::vector< diff_result >;
  using struct_check_result_t = std::optional< struct_icheck_res_t >;

  std::string to_string(const struct_check_result_t &a) {
    std::stringstream ss;
    if (!a)
      return "( nullopt )";
    for (std::size_t i = 0; i < a->size(); ++i)
      ss << i << " " << to_string((*a)[i]) << std::endl;
    return ss.str();
  }

} // namespace circ::ifuzz::permutate
