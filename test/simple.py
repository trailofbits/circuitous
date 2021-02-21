# Copyright (c) 2021 Trail of Bits, Inc.

from tc import State, Test
from byte_generator import intel
from model_test import ModelTest

test_mov = {
  Test("mov imm rdx") .bytes("ba12000000").tags("imm_reduce")
  .case(
    E = State().RDX(0x12),
    R = True,
  ).case(
    E = State().RDX(0x12000000),
    R = False
  ).case(
    E = State().RDX(0x13),
    run_bytes = "ba13000000",
    R = False
  ).case(
    E = State().RDX(0x13),
    run_bytes = "ff13000000",
    R = False
  ),
  Test("mov imm eax/ebx/ecx/edx") \
  .bytes("b812000000bb12000000b912000000ba12000000")
  .tags("imm_reduce")
  .case("mov rax",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RAX(0x12),
    run_bytes = "b812000000",
    R = True
  ).case("mov rbx",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RBX(0x12),
    run_bytes = "bb12000000",
    R = True
  ).case("mov rcx",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RCX(0x12),
    run_bytes = "b912000000",
    R = True
  ).case("mov rdx",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RDX(0x12),
    run_bytes = "ba12000000",
    R = True
  ).case("mov rbx, modify rax",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RBX(0x12).RAX(0x12),
    run_bytes = "bb12000000",
    R = False
  ).case("false",
    I = State().RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42),
    E = State().RAX(0x14),
    run_bytes = "b812000000",
    R = False
  ),
  Test("mov reg reg") \
  .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
  .tags("min")
  .case("rax:=rbx",
    I = State().RAX(0x12).RBX(0x22).RCX(0x32),
    E = State().RAX(0x22),
    run_bytes = 0,
    R = True
  ).case("rcx:=rax",
    I = State().RAX(0x12).RBX(0x22).RCX(0x32),
    E = State().RCX(0x12),
    run_bytes = 1,
    R = True
  ).case("rbx:=rax",
    I = State().RAX(0x12).RBX(0x22).RCX(0x32),
    E = State().RBX(0x12),
    run_bytes = 2,
    R = True
  ),
  Test("F: mov reg reg") \
  .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
  .tags("min")
  .DI(State().RAX(0x12).RBX(0x22).RCX(0x32))
  .case("rax:=rbx",
    E = State().RAX(0x32),
    run_bytes = 0,
    R = False
  ).case("rcx:=rax",
    E = State().RCX(0x32),
    run_bytes = 1,
    R = False
  ).case("rbx:=rax",
    E = State().RBX(0x22),
    run_bytes = 2,
    R = False
  )
}

test_lea = {
  ModelTest("T:lea ").bytes(intel(["lea rdi, [rsi - 0x15]"])).tags("min").
  case("rsi:=0x15",
    I = State().RSI(0x15),
    R = True
  ).case("rsi:=0x0",
    I = State().RSI(0x0),
    R = True
  ).case("rsi:=0xffffffffffffff",
    I = State().RSI(0xffffffffffffff),
    R = True
  )
}

test_idiv = {
  ModelTest("T:idiv").bytes(intel(["idiv rsi"])).tags({"min", "idiv"}).
  case(
    I = State().RDX(0x0).RAX(0x66).RSI(0x22).RIP(0x1000).aflags(0),
    R = True
  ).case(
    I = State().RDX(0x0).RAX(0x66).RSI(0x1).RIP(0x1000).aflags(0),
    R = True
  ).case(
    I = State().RDX(0x0).RAX(0x66).RSI(0x66).RIP(0x1000).aflags(0),
    R = True
  )
}

circuitous_tests = [test_mov, test_lea, test_idiv]