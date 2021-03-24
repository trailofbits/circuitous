# Copyright (c) 2021 Trail of Bits, Inc.

from byte_generator import intel
from model_test import ModelTest, Test
from tc import State, S, MS, if_has

test_adc = {
  ModelTest("adc").tags({"min", "adc"})
  .bytes(intel(["adc al, 0x12"]))
  .case("ff.1", I = S().RAX(0x7fffffffffffffff).CF(0x1), R = True)
  .case("0.1", I = S().RAX(0x0).CF(0x1), R = True)
  .case("0.0", I = S().RAX(0x0).CF(0x0), R = True)
  .case("ff.0", I = S().RAX(0x7fffffffffffffff).CF(0x1), R = True),

  ModelTest("adc 2 variants").tags({"min", "adc"})
  .bytes(intel(["adc al, 0x12", "adc rax, 0x22"]))
  .case(I = S().RAX(0x435).CF(0x1), run_bytes = 0, R = True)
  .case(I = S().RAX(0x435).CF(0x1), run_bytes = 1, R = True)
  .case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["adc rax, 0x32"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["adc eax, 0x52"]),
    RG = if_has("reduce_imms", False),
    R = False
  ).case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["adc al, 0x52"]),
    RG = if_has("reduce_imms", True),
    R = False
  ),

  ModelTest("adc 2 variants opt").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
  .DI(S().RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True),

  ModelTest("adc 2 variants opt, 0 state").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
  .DI(S().RAX(0x0).RSI(0x0).RDI(0x0).CF(0x1))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True),

  ModelTest("adc 3 variants").tags({"min", "adc"})
  .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15", "adc rdi, 0x17"]))
  .DI(S().RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
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
  .case("ff.1", I = S().RAX(0x7fffffffffffffff).CF(0x1), R = True)
  .case("0.1", I = S().RAX(0x0).CF(0x1), R = True)
  .case("0.0", I = S().RAX(0x0).CF(0x0), R = True)
  .case("ff.0", I = S().RAX(0x7fffffffffffffff).CF(0x1), R = True),

  ModelTest("sbb 2 variants").tags({"min", "sbb"})
  .bytes(intel(["sbb al, 0x12", "sbb rax, 0x22"]))
  .case(I = S().RAX(0x435).CF(0x1), run_bytes = 0, R = True)
  .case(I = S().RAX(0x435).CF(0x1), run_bytes = 1, R = True)
  .case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb rax, 0x32"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb eax, 0x52"]),
    RG = if_has("reduce_imms", False),
    R = False
  ).case(
    I = S().RAX(0x435).CF(0x1),
    run_bytes = intel(["sbb al, 0x52"]),
    RG = if_has("reduce_imms", True),
    R = False
  ),

  ModelTest("sbb 3 variants").tags({"min", "sbb"})
  .bytes(intel(["sbb rax, 0x12", "sbb rsi, 0x15", "sbb rdi, 0x17"]))
  .DI(S().RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
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
  .case(I = S().RDX(0x7fffffffffffffff).CF(0x0), R = True)
  .case(I = S().RDX(0xff).CF(0x0), R = True)
  .case(
    I = S().RDX(0x7fffffffffffffff).CF(0x0),
    run_bytes = intel(["shl rdx, 0x4"]),
    RG = if_has("reduce_imms", True),
    R = False
  ).case(
    I = S().RDX(0x7fffffffffffffff).CF(0x0),
    run_bytes = intel(["shl rcx, 0x4"]),
    RG = if_has("reduce_imms", False),
    R = False
  )
}

test_shr = {
  ModelTest("shr rax").tags({"min", "shr"}).bytes(intel(["shr rax"]))
  .case(I = S().RAX(0b101).aflags(0), R = True)
  .case(I = S().RAX(0b1010).aflags(0), R = True),

  ModelTest("shr rbx imm8").tags({"min", "shr"}).bytes(intel(["shr rbx, 0x5"]))
  .case(I = S().RBX(0b1011111).aflags(0), R = True)
  .case(I = S().RBX(0b1000000).aflags(0), R = True),

  ModelTest("shr rbx 0").tags({"min", "shr"}).bytes(intel(["shr rbx, 0"]))
  .case(I = S().RBX(0b101).aflags(0), R = True)
  .case(I = S().RBX(0b101).aflags(1), R = True),

  ModelTest("shr rdx cl").tags({"min", "shr"}).bytes(intel(["shr rdx, cl"]))
  .case(I = S().RDX(0x7fffffffffffffff).RCX(63), R = True)
  .case(I = S().RDX(0x7fffffffffffffff).RCX(65), R = True)
}

test_xor = {
  ModelTest("xor").tags({"min", "xor"})
  .bytes(intel(["xor rsi, rdi"]))
  .DI(S().RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["xor rdi, 0x12"]), R = False)
  .case(run_bytes = intel(["xor rdi, rax"]), R = False),

  ModelTest("xor 2 variants").tags({"min", "xor"})
  .bytes(intel(["xor rsi, 0xff00ff", "xor rdi, 0x1"]))
  .DI(S().RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["xor rax, 0x12"]), R = False)
  .case(run_bytes = intel(["xor rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["xor rsi, 0x0"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["xor rdi, 0x7fff"]),
        RG = if_has("reduce_imms", False), R = False)
}

test_or = {
  ModelTest("or").tags({"min", "or"})
  .bytes(intel(["or rsi, rdi"]))
  .DI(S().RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["or rdi, 0x12"]), R = False)
  .case(run_bytes = intel(["or rdi, rax"]), R = False),

  ModelTest("or 2 variants").tags({"min", "or"})
  .bytes(intel(["or rsi, 0xff00ff", "or rdi, 0x1"]))
  .DI(S().RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["or rax, 0x12"]), R = False)
  .case(run_bytes = intel(["or rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["or rsi, 0x0"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["or rdi, 0x7fff"]),
        RG = if_has("reduce_imms", False), R = False)
}

test_and = {
  ModelTest("and").tags({"min", "and"})
  .bytes(intel(["and rsi, rdi"]))
  .DI(S().RSI(0x42).RDI(0x42).OF(0x1))
  .case(R = True)
  .case(DI = MS().RSI(0x2), R = True)
  .case(DI = MS().RDI(0x22).CF(0x1), R = True)
  .case(run_bytes = intel(["and rdi, 0x12"]), R = False)
  .case(run_bytes = intel(["and rdi, rax"]), R = False),

  ModelTest("and 2 variants").tags({"min", "and"})
  .bytes(intel(["and rsi, 0xff00ff", "and rdi, 0x1"]))
  .DI(S().RSI(0xff12ee).RDI(0x101).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True)
  .case(run_bytes = intel(["and rax, 0x12"]), R = False)
  .case(run_bytes = intel(["and rsi, 0x101"]), RG = if_has("reduce_imms", True), R = False)
  # Following test have their immediates of incorrect sizes -- should not work even with
  # --reduce_imms
  .case(run_bytes = intel(["and rsi, 0x0"]), RG = if_has("reduce_imms", False), R = False)
  .case(run_bytes = intel(["and rdi, 0x7fff"]),
        RG = if_has("reduce_imms", False), R = False)


}

test_div = {
  ModelTest("div").tags({"min", "div"}).bytes(intel(["div rdi"]))
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
  .DI(S().RIP(0x1000).aflags(0))
  .case(DI = MS().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
        DE = MS().aflags(0), R = True)
  .case(DI = MS().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
        DE = MS().aflags(0), R = True)
}

test_rcr = {
  ModelTest("rcr rax").tags({"min", "rcr"}).bytes(intel(["rcr rax"]))
  .case(I = S().RAX(0b101).aflags(0), R = True)
  .case(I = S().RAX(0b1010).aflags(0), R = True),

  ModelTest("rcr rbx imm8").tags({"min", "rcr"}).bytes(intel(["rcr rbx, 0x5"]))
  .case(I = S().RBX(0b1011111).aflags(0), R = True)
  .case(I = S().RBX(0b1000000).aflags(0), R = True),

  ModelTest("rcr rbx 0").tags({"min", "rcr"}).bytes(intel(["rcr rbx, 0"]))
  .case(I = S().RBX(0b101).aflags(0), R = True)
  .case(I = S().RBX(0b101).aflags(1), R = True),

  ModelTest("rcr rdx cl").tags({"min", "rcr"}).bytes(intel(["rcr rdx, cl"]))
  .case(I = S().RDX(0x7fffffffffffffff).RCX(63).aflags(0), R = True)
  .case(I = S().RDX(0x7fffffffffffffff).RCX(65).aflags(1), R = True)
}

test_ror = {
  ModelTest("ror rax").tags({"min", "ror"}).bytes(intel(["ror rax"]))
  .case(I = S().RAX(0b101).aflags(0), R = True)
  .case(I = S().RAX(0b1010).aflags(0), R = True),

  ModelTest("ror rbx imm8").tags({"min", "ror"}).bytes(intel(["ror rbx, 0x5"]))
  .case(I = S().RBX(0b1011111).aflags(0), R = True)
  .case(I = S().RBX(0b1000000).aflags(0), R = True),

  ModelTest("ror rbx 0").tags({"min", "ror"}).bytes(intel(["ror rbx, 0"]))
  .case(I = S().RBX(0b101).aflags(0), DE = MS(), R = True)
  .case(I = S().RBX(0b101).aflags(1), DE = MS(), R = True),

  ModelTest("ror rdx cl").tags({"min", "ror"}).bytes(intel(["ror rdx, cl"]))
  .case(I = S().RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
  .case(I = S().RDX(0x7fffffffffffffff).RCX(65).aflags(0), DE = MS().uOF(), R = True)
}

test_rcl = {
  ModelTest("rcl rax").tags({"min", "rcl"}).bytes(intel(["rcl rax"]))
  .case(I = S().RAX(0b101).aflags(0), R = True)
  .case(I = S().RAX(0b1010).aflags(0), R = True),

  ModelTest("rcl rbx imm8").tags({"min", "rcl"}).bytes(intel(["rcl rbx, 0x5"]))
  .case(I = S().RBX(0b1011111).aflags(0), R = True)
  .case(I = S().RBX(0b1000000).aflags(0), R = True),

  ModelTest("rcl rbx 0").tags({"min", "rcl"}).bytes(intel(["rcl rbx, 0"]))
  .case(I = S().RBX(0b101).aflags(0), R = True)
  .case(I = S().RBX(0b101).aflags(1), R = True),

  ModelTest("rcl rdx cl").tags({"min", "rcl"}).bytes(intel(["rcl rdx, cl"]))
  .case(I = S().RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
  .case(I = S().RDX(0x7fffffffffffffff).RCX(65).aflags(1), DE = MS().uOF(), R = True)
}


test_ip = {
  Test("ip.update").bytes("ba12000000").tags({"min", "ip"})
  .case(I = S().RDX(0x0).RIP(0x1000), E = S().RDX(0x12).RIP(0x1005), R = True)
}

test_all = {
  ModelTest("adc, add, sub, sbb, shl, shr, xor, or, and, div, idiv").tags({"opts"})
  .bytes(intel(["adc rax, 0x12", "add rcx, rax", "add rax, 0x45",
                "sub rcx, 0x15", "sub rax, rcx", "sbb rax, 0x12",
                "shr rax, 0x2", "shr rcx, 0x2", "shl rax, 0x2", "shl rcx, 0x2",
                "xor rax, rax", "xor rax, rcx",
                "or rax, rax","or rax, rcx",
                "and rax, 0xfffff", "and rax, rcx",
                "div rax", "idiv rcx", "idiv rax"]))
  .DI(S().RAX(0x12).RCX(0x45).aflags(0))
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
  .DI(S().RAX(0x12).RCX(0x45).aflags(0))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = 1, R=True)
}

# TODO(lukas): Division by zero

circuitous_tests = \
  [ test_adc, test_sbb,
    test_shl, test_shr,
    test_rcl, test_ror, test_rcr,
    test_xor, test_or, test_and,
    test_div, test_idiv,
    test_ip,
    test_all
  ]
