# Copyright (c) 2021 Trail of Bits, Inc.

from byte_generator import intel
from model_test import ModelTest, Test
from tc import State, S, MS, if_has

test_adc = {
  ModelTest("adc").tags({"min", "adc"})
  .bytes(intel(["adc al, 0x12"]))
  .case("ff.1", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True)
  .case("0.1", I = S(0x150).RAX(0x0).CF(0x1), R = True)
  .case("0.0", I = S(0x250).RAX(0x0).CF(0x0), R = True)
  .case("ff.0", I = S(0x350).RAX(0x7fffffffffffffff).CF(0x1), R = True),

  ModelTest("adc 2 variants").tags({"min", "adc"})
  .bytes(intel(["adc al, 0x12", "adc rax, 0x22"]))
  .DI(S(0x350).RAX(0x435).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(
    run_bytes = intel(["adc rax, 0x32"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    run_bytes = intel(["adc eax, 0x52"]),
    RG = if_has("reduce_imms", False),
    R = False
  ).case(
    run_bytes = intel(["adc al, 0x52"]),
    RG = if_has("reduce_imms", True),
    R = False
  ),

  ModelTest("adc 2 variants opt").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
  .DI(S(0x200).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True),

  ModelTest("adc 2 variants opt, 0 state").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
  .DI(S(0x200).RAX(0x0).RSI(0x0).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True),

  ModelTest("adc 3 variants").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15", "adc rdi, 0x17"]))
  .DI(S(0x200).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = 2, R = True)
  .case(run_bytes = intel(["adc eax, 0x12"]), R = False)
  .case(run_bytes = intel(["adc rax, 0x0"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["adc rsi, 0x0"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["adc rdi, 0x0"]), RG = if_has("reduce_imms", True), R = False)
}

test_sbb = {
  ModelTest("sbb").tags({"min", "sbb"})
  .bytes(intel(["sbb al, 0x12"]))
  .case("ff.1", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True)
  .case("0.1", I = S(0x100).RAX(0x0).CF(0x1), R = True)
  .case("0.0", I = S(0x100).RAX(0x0).CF(0x0), R = True)
  .case("ff.0", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True),

  ModelTest("sbb 2 variants").tags({"min", "sbb"})
  .bytes(intel(["sbb al, 0x12", "sbb rax, 0x22"]))
  .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = 0, R = True)
  .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = 1, R = True)
  .case(
    I = S(0x100).RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb rax, 0x32"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    I = S(0x100).RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb eax, 0x52"]),
    RG = if_has("reduce_imms", False),
    R = False
  ).case(
    I = S(0x100).RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb al, 0x52"]),
    RG = if_has("reduce_imms", True),
    R = False
  ),

  ModelTest("sbb 3 variants").tags({"min", "sbb"})
  .bytes(intel(["sbb rax, 0x12", "sbb rsi, 0x15", "sbb rdi, 0x17"]))
  .DI(S(0x100).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = 2, R = True)
  .case(run_bytes = intel(["sbb eax, 0x12"]), R = False)
  .case(run_bytes = intel(["sbb rax, 0x0"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["sbb rsi, 0x0"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["sbb rdi, 0x0"]), RG = if_has("reduce_imms", True), R = False)
}

test_shl = {
  ModelTest("shl").tags({"min", "shl"})
  .bytes(intel(["shl rdx, 0x20"]))
  .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0), R = True)
  .case(I = S(0x100).RDX(0xff).CF(0x0), R = True)
  .case(
    I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
    run_bytes = intel(["shl rdx, 0x4"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
    run_bytes = intel(["shl rcx, 0x4"]),
    RG = if_has("reduce_imms", True),
    R = False
  )
}

test_shr = {
  ModelTest("shr rax").tags({"min", "shr"}).bytes(intel(["shr rax"]))
  .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

  ModelTest("shr rbx imm8").tags({"min", "shr"}).bytes(intel(["shr rbx, 0x5"]))
  .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

  ModelTest("shr rbx 0").tags({"min", "shr"}).bytes(intel(["shr rbx, 0"]))
  .case(I = S(0x100).RBX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b101).aflags(1), R = True),

  ModelTest("shr rdx cl").tags({"min", "shr"}).bytes(intel(["shr rdx, cl"]))
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63), R = True)
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65), R = True)
}

test_xor = {
  ModelTest("xor").tags({"min", "xor"})
  .bytes(intel(["xor rsi, rdi"]))
  .DI(S(0x100).RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["xor rdi, 0x12"]), R = False)
  .case(run_bytes = intel(["xor rdi, rax"]), RG = if_has("reduce_imms", True), R = False),

  ModelTest("xor 2 variants").tags({"min", "xor"})
  .bytes(intel(["xor rsi, 0xff00ff", "xor rdi, 0x1"]))
  .DI(S(0x100).RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["xor rax, 0x12"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["xor rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["xor rsi, rax"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["xor rdi, 0x7fff"]),
        RG = if_has("reduce_imms", True), R = False)
}

test_or = {
  ModelTest("or").tags({"min", "or"})
  .bytes(intel(["or rsi, rdi"]))
  .DI(S(0x100).RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["or rdi, 0x12"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["or rdi, rax"]), RG = if_has("reduce_imms", True), R = False),

  ModelTest("or 2 variants").tags({"min", "or"})
  .bytes(intel(["or rsi, 0xff00ff", "or rdi, 0x1"]))
  .DI(S(0x100).RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["or rax, 0x12"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["or rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["or rsi, rax"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["or rdi, 0x7fff"]),
        RG = if_has("reduce_imms", True), R = False)
}

test_and = {
  ModelTest("and").tags({"min", "and"})
  .bytes(intel(["and rsi, rdi"]))
  .DI(S(0x100).RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["and rdi, 0x12"]), R = False)
  .case(run_bytes = intel(["and rdi, rax"]), RG = if_has("reduce_imms", True), R = False),

  ModelTest("and 2 variants").tags({"min", "and"})
  .bytes(intel(["and rsi, 0xff00ff", "and rdi, 0x1"]))
  .DI(S(0x100).RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["and rax, 0x12"]), RG = if_has("reduce_imms", True), R = False)
  .case(run_bytes = intel(["and rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["and rsi, rbx"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["and rdi, 0x7fff"]),
        RG = if_has("reduce_imms", True), R = False)


}

test_div = {
  ModelTest("div").tags({"min", "div"}).bytes(intel(["div rdi"]))
  .DI(S(0x100))
  .case(DI = MS().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
        DE = MS().aflags(0), R = True)
}

test_idiv = {
  ModelTest("idiv").tags({"min", "idiv"})
  .bytes(intel(["idiv rdi"]))
  .DI(S(0x100).RIP(0x1000).aflags(0))
  .case(DI = MS().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
        DE = MS().aflags(0), R = True)
}

test_rcr = {
  ModelTest("rcr rax").tags({"min", "rcr"}).bytes(intel(["rcr rax"]))
  .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

  ModelTest("rcr rbx imm8").tags({"min", "rcr"}).bytes(intel(["rcr rbx, 0x5"]))
  .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

  ModelTest("rcr rbx 0").tags({"min", "rcr"}).bytes(intel(["rcr rbx, 0"]))
  .case(I = S(0x100).RBX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b101).aflags(1), R = True),

  ModelTest("rcr rdx cl").tags({"min", "rcr"}).bytes(intel(["rcr rdx, cl"]))
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(0), R = True)
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65).aflags(1), R = True)
}

test_ror = {
  ModelTest("ror rax").tags({"min", "ror"}).bytes(intel(["ror rax"]))
  .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

  ModelTest("ror rbx imm8").tags({"min", "ror"}).bytes(intel(["ror rbx, 0x5"]))
  .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

  ModelTest("ror rbx 0").tags({"min", "ror"}).bytes(intel(["ror rbx, 0"]))
  .case(I = S(0x100).RBX(0b101).aflags(0), DE = MS(), R = True)
  .case(I = S(0x100).RBX(0b101).aflags(1), DE = MS(), R = True),

  ModelTest("ror rdx cl").tags({"min", "ror"}).bytes(intel(["ror rdx, cl"]))
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65).aflags(0), DE = MS().uOF(), R = True)
}

test_rcl = {
  ModelTest("rcl rax").tags({"min", "rcl"}).bytes(intel(["rcl rax"]))
  .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

  ModelTest("rcl rbx imm8").tags({"min", "rcl"}).bytes(intel(["rcl rbx, 0x5"]))
  .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

  ModelTest("rcl rbx 0").tags({"min", "rcl"}).bytes(intel(["rcl rbx, 0"]))
  .case(I = S(0x100).RBX(0b101).aflags(0), R = True)
  .case(I = S(0x100).RBX(0b101).aflags(1), R = True),

  ModelTest("rcl rdx cl").tags({"min", "rcl"}).bytes(intel(["rcl rdx, cl"]))
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
  .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65).aflags(1), DE = MS().uOF(), R = True)
}


test_ip = {
  Test("ip.update").bytes("ba12000000").tags({"min", "ip"})
  .case(I = S(0x100).RDX(0x0).RIP(0x1000), E = S(0x100).RDX(0x12).RIP(0x1005), R = True)
}

test_all = {
        ModelTest("adc, add, sub, sbb, shl, shr, xor, or, and, div, idiv").tags({"min", "big"})
  .bytes(intel(["adc rax, 0x12", "add rcx, rax", "add rax, 0x45",
                "sub rcx, 0x15", "sub rax, rcx", "sbb rax, 0x12",
                "shr rax, 0x2", "shr rcx, 0x2", "shl rax, 0x2", "shl rcx, 0x2",
                "xor rax, rax", "xor rax, rcx",
                "or rax, rax","or rax, rcx",
                "and rax, 0xfffff", "and rax, rcx",
                "div rax", "idiv rcx", "idiv rax"]))
  .DI(S(0x1).RAX(0x12).RCX(0x45).aflags(0))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = 1, R=True)
  .case(run_bytes = 2, R=True)
  .case(run_bytes = 3, R=True)
  .case(run_bytes = 4, R=True)
  .case(run_bytes = 5, R=True)
  .case(run_bytes = 6, R=True)
  .case(run_bytes = 7, R=True)
  .case(run_bytes = 8, R=True)
  .case(run_bytes = 9, R=True)
  .case(run_bytes = 10, R=True)
  .case(run_bytes = 11, R=True)
  .case(run_bytes = 12, R=True)
  .case(run_bytes = 13, R=True)
  .case(run_bytes = 14, R=True)
  .case(run_bytes = 15, R=True)
  .case(run_bytes = 16, DE = MS().aflags(0), R=True)
  .case(run_bytes = 17, DE = MS().aflags(0), R=True)
  .case(run_bytes = 18, DE = MS().aflags(0), R=True),

  # If in reduce_imms we do not check that opcodes (as well as operands)
  # must be equal, this should fire
  ModelTest("encoding adc, sbb").tags({"min", "reduce"})
  .bytes(intel(["adc rax, 0x12",
                "sbb rax, 0x12"]))
  .DI(S(0x100).RAX(0x12).RCX(0x45).aflags(0))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = 1, R=True)
}

test_reduce_regs = {
  ModelTest("mov reg, imm").tags({"reduce_regs", "todo1"})
  .bytes(intel(["mov rax, 0x12"]))
  .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
  .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rax, 0x22"]), R=True)
  .case(run_bytes = intel(["mov rbx, 0x22"]), R=True)
  .case(run_bytes = intel(["mov rcx, 0x22"]), R=True)
  .case(run_bytes = intel(["mov rdx, 0x22"]), R=True),

  ModelTest("mov reg, reg").tags({"reduce_regs", "todo2"})
  .bytes(intel(["mov rcx, rax"]))
  .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
  .case(run_bytes = intel(["mov rax, rcx"]), R=True)
  .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
  .case(run_bytes = intel(["mov rcx, rax"]), R=True)
  .case(run_bytes = intel(["mov rdx, rax"]), R=True),

  ModelTest("add reg, reg").tags({"reduce_regs", "todo3"})
  .bytes(intel(["add rcx, rax"]))
  .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
  .case(run_bytes = intel(["add rax, rcx"]), R=True)
  .case(run_bytes = intel(["add rbx, rdx"]), R=True)
  .case(run_bytes = intel(["add rcx, rax"]), R=True)
  .case(run_bytes = intel(["add rdx, rax"]), R=True),


  ModelTest("add reg, imm/reg").tags({"reduce_regs", "todo4"})
  .bytes(intel(["mov rax, 0x12", "mov rcx, rax"]))
  .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
  .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rax, rcx"]), R=True)
  .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
  .case(run_bytes = intel(["mov rcx, rax"]), R=True)
  .case(run_bytes = intel(["mov rdx, rax"]), R=True),

  ModelTest("add/mov reg/reg, reg/imm").tags({"reduce_regs"})
  .bytes(intel(["mov rax, 0x12", "add rax, 0x12", "mov rbx, rcx", "add r8, rax"]))
  .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).R8(0x12).R9(0x22).RSP(0x2000).aflags(0))
  .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
  .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
  .case(run_bytes = intel(["add rax, 0x22"]), R=True)
  .case(run_bytes = intel(["add rbx, 0x22"]), R=True)
  .case(run_bytes = intel(["add rcx, 0x22"]), R=True)
  .case(run_bytes = intel(["add rdx, 0x22"]), R=True)
  .case(run_bytes = intel(["mov rax, rcx"]), R=True)
  .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
  .case(run_bytes = intel(["mov rcx, rax"]), R=True)
  .case(run_bytes = intel(["mov rdx, rax"]), R=True)
  .case(run_bytes = intel(["add rax, rcx"]), R=True)
  .case(run_bytes = intel(["add rbx, rdx"]), R=True)
  .case(run_bytes = intel(["add rcx, rax"]), R=True)
  .case(run_bytes = intel(["add rdx, rax"]), R=True)
  .case(run_bytes = intel(["add rdx, r8"]), R=True)
  .case(run_bytes = intel(["add r9, rax"]), R=True)
  .case(run_bytes = intel(["add r9, r8"]), R=True)
  .case(run_bytes = intel(["add r8, 0x42"]), R=True)
  .case(run_bytes = intel(["add r9, 0x53"]), R=True)
  .case(run_bytes = intel(["mov rdx, r8"]), R=True)
  .case(run_bytes = intel(["mov r9, rax"]), R=True)
  .case(run_bytes = intel(["mov r9, r8"]), R=True)
  .case(run_bytes = intel(["mov r8, 0x42"]), R=True)
  .case(run_bytes = intel(["mov r9, 0x53"]), R=True)
}

test_identity_ops = {
  ModelTest("add rax, rax").bytes(intel(["add rax, rax"])).tags({"reduce_regs", "identity"})
  .DI(S(0x100).aflags(0).R13(0x2).RBX(0x3))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = intel(["add rax, rbx"]), R=True)
  .case(run_bytes = intel(["add rcx, r13"]), R=True)
}

test_reg_parts = {
  ModelTest("mov reg16, reg16").tags({"reduce_regs"})
  .bytes(intel(["mov ax, bx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov cx, dx"]), R=True),

  ModelTest("mov reg16/32, reg16/32").tags({"reduce_regs"})
  .bytes(intel(["mov ax, bx", "mov eax, ebx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov cx, dx"]), R=True)
  .case(run_bytes = intel(["mov ecx, edx"]), R=True),

  # TODO(lukas): So look at the following encodings:
  #              89 d8           mov eax, ebx
  #              44 89 c0        mov eax, r8d
  # Eventually we want to be able to derive one from the other but
  # we cannot atm; test is marked as todo.
  ModelTest("mov reg32/r8d reg32/r8d - partial").tags({"reduce_regs", "todo"})
  .bytes(intel(["mov eax, ebx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov ecx, edx"]), R=True)
  .case(run_bytes = intel(["mov edx, r8d"]), R=True)
  .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
  .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

  # TODO(lukas): Same as above
  ModelTest("mov reg64/32/16 reg64/32/16/imm64/32/16 - partial").tags({"reduce_regs", "todo"})
  .bytes(intel(["mov rax, rbx", "mov eax, ebx", "mov ax, bx",
                "mov rax, 0x12", "mov eax, 0x12", "mov ax, 0x12"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov cx, dx"]), R=True)
  .case(run_bytes = intel(["mov cx, r8w"]), R=False)
  .case(run_bytes = intel(["mov r9w, bx"]), R=False)
  .case(run_bytes = intel(["mov ecx, edx"]), R=True)
  .case(run_bytes = intel(["mov ecx, r8d"]), R=True)
  .case(run_bytes = intel(["mov r9d, ebx"]), R=True)
  .case(run_bytes = intel(["mov cx, 0x7fff"]), R=True)
  .case(run_bytes = intel(["mov r8w, 0xffff"]), R=True)
  .case(run_bytes = intel(["mov r9w, 0xab"]), R=True)
  .case(run_bytes = intel(["mov ecx, 0x7fffffff"]), R=True)
  .case(run_bytes = intel(["mov r8d, 0xffffffff"]), R=True)
  .case(run_bytes = intel(["mov r9d, 0xab"]), R=True),

  ModelTest("mov reg32/r8d reg32/r8d - full").tags({"reduce_regs"})
  .bytes(intel(["mov eax, ebx", "mov r10d, ebx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov ecx, edx"]), R=True)
  .case(run_bytes = intel(["mov edx, r8d"]), R=True)
  .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
  .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

  ModelTest("mov rnd rnd - full").tags({"reduce_regs"})
  .bytes(intel(["mov r10d, ebx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov edx, r8d"]), R=True)
  .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
  .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

  ModelTest("mov rnw rnw - full").tags({"reduce_regs"})
  .bytes(intel(["mov r10w, ax"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov dx, r8w"]), R=True)
  .case(run_bytes = intel(["mov r8w, cx"]), R=True)
  .case(run_bytes = intel(["mov r8w, r9w"]), R=True),

  ModelTest("mov rnb rnb - full").tags({"reduce_regs"})
  .bytes(intel(["mov r10b, bl"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov al, r8b"]), R=True)
  .case(run_bytes = intel(["mov r8b, cl"]), R=True)
  .case(run_bytes = intel(["mov r8b, r9b"]), R=True),

  ModelTest("xor rnw rnw - full").tags({"reduce_regs", "wip2"})
  .bytes(intel(["xor r10w, ax"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["xor dx, r8w"]), R=True)
  .case(run_bytes = intel(["xor r8w, cx"]), R=True)
  .case(run_bytes = intel(["xor r8w, r9w"]), R=True),

  ModelTest("xor rnb rnb - full").tags({"reduce_regs", "wip2"})
  .bytes(intel(["xor r10b, bl"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["xor al, r8b"]), R=True)
  .case(run_bytes = intel(["xor r8b, cl"]), R=True)
  .case(run_bytes = intel(["xor r8b, r9b"]), R=True),


  ModelTest("mov rnd rnd - full").tags({"reduce_regs"})
  .bytes(intel(["mov eax, ebx"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov edx, eax"]), R=True)
  .case(DI=MS().RAX(0xffffffffffff), run_bytes = intel(["mov edx, eax"]), R=True)
  .case(run_bytes = intel(["mov ecx, ecx"]), R=True)
  .case(DI=MS().RCX(0xffffffffffff), run_bytes = intel(["mov ecx, ecx"]), R=True)
  .case(run_bytes = intel(["mov eax, ebx"]), R=True),

  ModelTest("mov al/ah versions").tags({"reduce_regs"})
  .bytes(intel(["mov ah, al"]))
  .DI(S(0x200).RAX(0xffee). RBX(0xccbb).RCX(0xaa99))
  .case(run_bytes = intel(["mov bh, al"]), R=True)
  .case(run_bytes = intel(["mov ch, bh"]), R=True)
  .case(run_bytes = intel(["mov cl, bh"]), R=True)
  .case(run_bytes = intel(["mov cl, bl"]), R=True),

  ModelTest("xor 16 lift").tags({"reduce_regs", "wip3"})
  .DI(S(0x250))
  .bytes(intel(["xor al, r8b"])).case(run_bytes = 0, R=True),

  ModelTest("xor 32 lift").tags({"reduce_regs", "wip3"})
  .DI(S(0x250))
  .bytes(intel(["xor eax, ebx"])).case(run_bytes = 0, R=True),

  ModelTest("adc al 0x0").tags({"reduce_regs", "todo"})
  .DI(S(0x250))
  .bytes(intel(["adc al, 0x0"]))
  .case(run_bytes = intel(["adc ah, 0xf"]), R=True),


  ModelTest("xor/adc fractions").tags({"reduce_args", "wip"})
  .bytes(intel(["xor al, ah", "xor al, 0xa", "xor ax, bx", "xor ax, 0x1",
                "xor ah, al", "xor ah, 0xb",
                "adc r8b, al", "adc r8b, 0xe", "adc ax, bx", "adc ax, 0x2",
                "adc al, 0x0", "adc ah, 0x5",
                "xor al, r8b", "xor al, 0x4", "xor r8w, bx", "xor r9w, 0x0",
                "adc r8b, al", "adc r8b, 0x5", "adc r8w, bx", "adc r10w, 0x1"]))
  .DI(S(0x200).RAX(0xffee).RBX(0xddcc).RCX(0xbbaa).RDX(0x9988).R8(0x7766)
              .R9(0x5544).R10(0x3322).aflags(1))
  .case(run_bytes = intel(["xor cx, r8w"]), R=True)
  .case(run_bytes = intel(["xor al, r8b"]), R=True)
  .case(run_bytes = intel(["xor r9b, cl"]), R=True)
  .case(run_bytes = intel(["xor eax, ebx"]), R=False)
  .case(run_bytes = intel(["xor r10w, r8w"]), R=True)
  .case(run_bytes = intel(["xor r9b, r8b"]), R=True)
  .case(run_bytes = intel(["xor r9b, 0xf"]), R=False)
  .case(run_bytes = intel(["xor ch, 0xf"]), R=True)
  .case(run_bytes = intel(["xor r9w, 0xf"]), R=True)
  .case(run_bytes = intel(["xor cl, 0xf"]), R=True)
  .case(run_bytes = intel(["xor cx, 0xf"]), R=True)
  .case(run_bytes = intel(["adc cx, r8w"]), R=True)
  .case(run_bytes = intel(["adc al, r8b"]), R=True)
  .case(run_bytes = intel(["adc r9b, cl"]), R=True)
  .case(run_bytes = intel(["adc eax, ebx"]), R=False)
  .case(run_bytes = intel(["adc r10w, r8w"]), R=True)
  .case(run_bytes = intel(["adc r9b, r8b"]), R=True)
  .case(run_bytes = intel(["adc r9b, 0xf"]), R=True)
  .case(run_bytes = intel(["adc ch, 0xf"]), R=True)
  .case(run_bytes = intel(["adc r9w, 0xf"]), R=True)
  .case(run_bytes = intel(["adc cl, 0xf"]), R=True)
  .case(run_bytes = intel(["adc cx, 0xf"]), R=True),


  ModelTest("mov reg64/32/16 reg64/32/16/imm64/32/16 - full").tags({"reduce_regs"})
  .bytes(intel(["mov rax, rbx", "mov eax, ebx", "mov ax, bx",
                "mov rax, 0x12", "mov eax, 0x12", "mov ax, 0x12",
                "mov r8d, 0x12", "mov r8d, eax", "mov r8w, 0x12",
                "mov r8w, ax"]))
  .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
  .case(run_bytes = intel(["mov cx, dx"]), R=True)
  .case(run_bytes = intel(["mov cx, r8w"]), R=True)
  .case(run_bytes = intel(["mov r9w, bx"]), R=True)
  .case(run_bytes = intel(["mov ecx, edx"]), R=True)
  .case(run_bytes = intel(["mov ecx, r8d"]), R=True)
  .case(run_bytes = intel(["mov r9d, ebx"]), R=True)
  .case(run_bytes = intel(["mov cx, 0x7fff"]), R=True)
  .case(run_bytes = intel(["mov r8w, 0xffff"]), R=True)
  .case(run_bytes = intel(["mov r9w, 0xab"]), R=True)
  .case(run_bytes = intel(["mov ecx, 0x7fffffff"]), R=True)
  .case(run_bytes = intel(["mov r8d, 0xffffffff"]), R=True)
  .case(run_bytes = intel(["mov r9d, 0xab"]), R=True)
}

# TODO(lukas): Division by zero

circuitous_tests = \
  [ test_adc, test_sbb,
    test_shl, test_shr,
    test_rcl, test_ror, test_rcr,
    test_xor, test_or, test_and,
    test_div, test_idiv,
    test_ip,
    test_all,
    test_reduce_regs,
    test_identity_ops,
    test_reg_parts
  ]
