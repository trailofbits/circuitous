/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

namespace circ {

  // helper for variant visitor
  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

} // namespace circ
