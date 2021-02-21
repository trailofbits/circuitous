# Copyright (c) 2021 Trail of Bits, Inc.

from tc import State, Test
from byte_generator import intel

mov = [
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
]