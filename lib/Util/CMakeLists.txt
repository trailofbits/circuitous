# Copyright (c) 2022 Trail of Bits, Inc.

# TODO fix gap overlaps
add_headers( Util CIRCUITOUS_UTIL_HEADERS
  CmdParser.hpp
  ConstExprVector.hpp
  FixedString.hpp
  InstructionBytes.hpp
  LLVMUtil.hpp
  Overloads.hpp
  StrongType.hpp
  TypeList.hpp
  TypeTraits.hpp
  UseDef.hpp
  Warnings.hpp
)

add_circuitous_header_library( util
  HEADERS
    ${CIRCUITOUS_UTIL_HEADERS}
)
