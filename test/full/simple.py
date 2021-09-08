# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS
from tools.byte_generator import intel
from tools.model_test import ModelTest

test_mov = {
  Test("mov imm rdx") .bytes("ba12000000").tags({"mov"})
  .DI(S(0x100).RIP(0x1000))
  .case(DE = MS().RDX(0x12).RIP(0x1005).ts(1), R = True)
  .case(DE = MS().RDX(0x12000000).RIP(0x1005).ts(1), R = False)
  .case(DE = MS().RDX(0x13).RIP(0x1005).ts(1), run_bytes = "ba13000000", R = True)
  .case(
    I = S(0x0).RIP(0x1000),
    E = S(0x0).RDX(0x13).RIP(0x1005).ts(1),
    run_bytes = "ff13000000",
    R = False
  ),

  Test("mov imm eax/ebx/ecx/edx") \
  .bytes("b812000000bb12000000b912000000ba12000000")
  .tags({"min", "mov"})
  .DI(S(0x100).RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42).RIP(0x1000).ts(0))
  .case("mov rax",
    DE = MS().RAX(0x12).RIP(0x1005).ts(1),
    run_bytes = "b812000000",
    R = True
  ).case("mov rbx",
    DE = MS().RBX(0x12).RIP(0x1005).ts(1),
    run_bytes = "bb12000000",
    R = True
  ).case("mov rcx",
    DE = MS().RCX(0x12).RIP(0x1005).ts(1),
    run_bytes = "b912000000",
    R = True
  ).case("mov rdx",
    DE = MS().RDX(0x12).RIP(0x1005).ts(1),
    run_bytes = "ba12000000",
    R = True
  ).case("mov rbx, modify rax",
    DE = MS().RBX(0x12).RAX(0x12).RIP(0x1005).ts(1),
    run_bytes = "bb12000000",
    R = False
  ).case("false",
    DE = MS().RAX(0x14).RIP(0x1005).ts(1),
    run_bytes = "b812000000",
    R = False
  ),
  Test("mov reg reg") \
  .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
  .tags({"min", "mov"})
  .DI(S(0x100).RAX(0x12).RBX(0x22).RCX(0x32).RIP(0x1000).ts(0))
  .case("rax:=rbx",
    DE = MS().RAX(0x22).RIP(0x1003).ts(1),
    run_bytes = 0,
    R = True
  ).case("rcx:=rax",
    DE = MS().RCX(0x12).RIP(0x1003).ts(1),
    run_bytes = 1,
    R = True
  ).case("rbx:=rax",
    DE = MS().RBX(0x12).RIP(0x1003).ts(1),
    run_bytes = 2,
    R = True
  ),
  Test("F: mov reg reg") \
  .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
  .tags({"min", "mov"})
  .DI(S(0x100).RAX(0x12).RBX(0x22).RCX(0x32).ts(0))
  .case("rax:=rbx",
    DE = MS().RAX(0x32).RIP(0x1003).ts(1),
    run_bytes = 0,
    R = False
  ).case("rcx:=rax",
    DE = MS().RCX(0x32).RIP(0x1003).ts(1),
    run_bytes = 1,
    R = False
  ).case("rbx:=rax",
    DE = MS().RBX(0x22).RIP(0x1003).ts(1),
    run_bytes = 2,
    R = False
  ),
  ModelTest("T: mov 0xffffffffffffffff rdx")\
  .bytes(intel(["mov rdx, 0xffffffffffffffff"]))
  .tags({"min", "mov"})
  .DI(S(0x1))
  .case(
    R = True
  ),
  ModelTest("T: mov 0x7fffffffffffffff rdx")\
  .bytes(intel(["mov rdx, 0x7fffffffffffffff"]))
  .tags({"min", "mov"})
  .DI(S(0x1))
  .case(
    R = True
  )
}

test_lea = {
  ModelTest("T:lea ").bytes(intel(["lea rdi, [rsi - 0x15]"])).tags({"lea"})
  .case("rsi:=0x15", I = S(0x100).RSI(0x15), R = True)
  .case("rsi:=0x0", I = S(0x100).RSI(0x0), R = True)
  .case("rsi:=0xffffffffffffff", I = S(0x100).RSI(0xffffffffffffffff), R = True)
}

test_idiv = {
  ModelTest("T:idiv").bytes(intel(["idiv rsi"])).tags({"min", "idiv"}).
  case(
    I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x22).RIP(0x1000).aflags(0),
    DE = MS().aflags(0),
    R = True
  ).case(
    I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x1).RIP(0x1000).aflags(0),
    DE = MS().aflags(0),
    R = True
  ).case(
    I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x66).RIP(0x1000).aflags(0),
    DE = MS().aflags(0),
    R = True
  )
}

test_add = {
  ModelTest("T:add rax imm") \
  .bytes(intel(["add rax, 8"])).tags({"add", "reduce_imms"})
  .case("rax+=8",
    I = S(0x100).RAX(0x5).aflags(0),
    R = True
  ).case("rax+=16",
    I = S(0x100).RAX(0x5).aflags(0),
    run_bytes = intel(["add rax, 16"])[0],
    R = True
  ),
  Test("T_reduced:add rax imm") \
  .bytes(intel(["add rax, 0x10"])).tags({"reduce_imms", "add"})
  .case("rax+=0x20",
    I = S(0x0).RAX(0x0).aflags(0).RIP(0x1000),
    E = S(0x0).RAX(0x20).RIP(0x1004).aflags(0).ts(1),
    run_bytes = intel(["add rax, 0x20"])[0],
    R = True
  ),
  Test("T_reduced:add rax imm") \
  .bytes(intel(["add rax, 0x10", "mov rcx, 0x10"])).tags({"reduce_imms", "add"})
  .case("rax+=0x20",
    I = S(0x0).RAX(0x0).RCX(0x0).RIP(0x1000).aflags(0),
    E = S(0x0).RAX(0x0).RCX(0x20).RIP(0x1004).ts(1),
    run_bytes = intel(["add rcx, 0x20"])[0],
    R = True
  )
}

test_mov_add = {
  ModelTest("T: add, mov") \
  .bytes(intel(["add rax, 0x20", "mov rax, 0x10"]))
  .tags({"reduce_imms", "add", "mov"})
  .DI(S(0x1).RAX(0x0).aflags(0))
  .case("add", run_bytes = 0, R = True)
  .case("mov", run_bytes = 1, R = True),
  ModelTest("add, mov") \
  .bytes(intel(["add rax, 0x20", "mov rax, 0x10"]))
  .tags({'reduce_imms', 'test'})
  .DI(S(0x1).RAX(0x0).aflags(0))
  .case("T:add", run_bytes = intel(["add rax, 0x30"])[0], R = True)
  .case("T:mov", run_bytes = intel(["mov rax, 0x40"])[0], R = True)
  .case("F:mov rbx", run_bytes = intel(["mov rbx, 0x40"])[0], R = True)
  .case("F:add rbx", run_bytes = intel(["add rbx, 0x40"])[0], R = True),
}

circuitous_tests = [test_mov, test_lea, test_idiv, test_add, test_mov_add]
