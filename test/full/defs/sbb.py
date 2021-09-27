# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_sbb = {
    VerifyTest("sbb").tags({"sbb"})
    .bytes(intel(["sbb al, 0x12"]))
    .case("ff.1", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True)
    .case("0.1", I = S(0x100).RAX(0x0).CF(0x1), R = True)
    .case("0.0", I = S(0x100).RAX(0x0).CF(0x0), R = True)
    .case("ff.0", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True),

    VerifyTest("sbb 2 variants").tags({"sbb"})
    .bytes(intel(["sbb al, 0x12", "sbb rax, 0x22"]))
    .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = 0, R = True)
    .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = 1, R = True)
    .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = intel(["sbb rax, 0x32"]), R = True)
    .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = intel(["sbb eax, 0x52"]), R = False)
    .case(I = S(0x100).RAX(0x435).CF(0x1), run_bytes = intel(["sbb al, 0x52"]), R = True),

    VerifyTest("sbb 3 variants").tags({"min", "sbb"})
    .bytes(intel(["sbb rax, 0x12", "sbb rsi, 0x15", "sbb rdi, 0x17"]))
    .DI(S(0x100).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True)
    .case(run_bytes = 2, R = True)
    .case(run_bytes = intel(["sbb eax, 0x12"]), R = False)
    .case(run_bytes = intel(["sbb rax, 0x0"]), R = True)
    .case(run_bytes = intel(["sbb rsi, 0x0"]), R = True)
    .case(run_bytes = intel(["sbb rdi, 0x0"]), R = True)
}

circuitous_tests = [test_sbb]
