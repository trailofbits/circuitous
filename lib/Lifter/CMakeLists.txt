# Copyright (c) 2022 Trail of Bits, Inc.

add_subdirectory( Components )

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
  ISELBank.hpp
  Lifter.hpp
  LLVMToCircIR.hpp
  Memory.hpp
  ToLLVM.hpp
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
    ISELBank.cpp
    Remill.cpp
    ShadowMat.cpp
    ToLLVM.cpp
    Shadows.cpp
  LINK_LIBS
    circuitous::dbg
    circuitous::fuzz
    circuitous::components
  HEADERS
    ${CIRCUITOUS_LIFTER_HEADERS}
)
