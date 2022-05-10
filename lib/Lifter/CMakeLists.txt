# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Lifter CIRCUITOUS_LIFTER_HEADERS
  BaseLifter.hpp
  CircuitBuilder.hpp
  CircuitSmithy.hpp
  Component.hpp
  Context.hpp
  Decoder.hpp
  DepBreaker.hpp
  DependencyVisitor.hpp
  Error.hpp
  Flatten.hpp
  Instruction.hpp
  LLVMToCircIR.hpp
  Memory.hpp
  SelectFold.hpp
  ShadowMat.hpp
  Shadows.hpp
  SReg.hpp
)

add_circuitous_library( lifter
  SOURCES
    BaseLifter.cpp
    CircuitBuilder.cpp
    CircuitSmithy.cpp
    Decoder.cpp
    Instruction.cpp
    Remill.cpp
  LINK_LIBS
    circuitous::dbg
    circuitous::fuzz
  HEADERS
    ${CIRCUITOUS_LIFTER_HEADERS}
)
