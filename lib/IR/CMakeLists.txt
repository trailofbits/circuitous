# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( IR CIRCUITOUS_IR_HEADERS
  Circuit.hpp
  Cost.hpp
  Intrinsics.hpp
  IntrinsicsHelpers.hpp
  IR.hpp
  Memory.hpp
  Metadata.hpp
  Shapes.hpp
  Storage.hpp
  Trace.hpp
  Verify.hpp
  Visitors.hpp
)

add_circuitous_library( ir
  SOURCES
    IR.cpp
    Serialize.cpp
    Verify.cpp
  HEADERS
    ${CIRCUITOUS_IR_HEADERS}
)

find_package( spdlog REQUIRED )

target_link_libraries( circuitous_ir
  PRIVATE
    spdlog::spdlog
    fmt::fmt
)
