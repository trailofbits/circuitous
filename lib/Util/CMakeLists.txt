# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Util CIRCUITOUS_UTIL_HEADERS
  CmdParser.hpp
  ConstExprVector.hpp
  FixedString.hpp
  InstructionBytes.hpp
  LLVMUtil.hpp
  Warnings.hpp
  Overloads.hpp
  Parser.hpp
  StrongType.hpp
  TypeList.hpp
  UseDef.hpp
)

add_circuitous_header_library( util
  HEADERS
    ${CIRCUITOUS_UTIL_HEADERS}
)
