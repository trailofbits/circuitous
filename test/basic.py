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
  .case(DI = S().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
        DE = MS().aflags(0), R = True)
  .case(DI = S().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
        DE = MS().aflags(0), R = True)
  .case(DI = S().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
        DE = MS().aflags(0), R = True)
}

test_idiv = {
  ModelTest("idiv").tags({"min", "idiv"})
  .bytes(intel(["idiv rdi"]))
  .DI(S().RIP(0x1000).aflags(0))
  .case(DI = S().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
        DE = MS().aflags(0), R = True)
  .case(DI = S().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
        DE = MS().aflags(0), R = True)
  .case(DI = S().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
        DE = MS().aflags(0), R = True)
}

test_ip = {
  Test("ip.update").bytes("ba12000000").tags({"min", "ip"})
  .case(I = S().RDX(0x0).RIP(0x1000), E = S().RDX(0x12).RIP(0x1005), R = True)
}

# TODO(lukas): Division by zero

circuitous_tests = \
  [test_adc, test_sbb, test_shl, test_xor, test_or, test_and, test_div, test_idiv, test_ip]
