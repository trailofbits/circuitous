# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_and = {
    VerifyTest("and").tags({"and"})
    .bytes(intel(["and rsi, rdi"]))
    .DI(S(0x100).RSI(0x42).RDI(0x42).OF(0x1))
    .case(R = True)
    .case(DI = MS().RSI(0x2), R = True)
    .case(DI = MS().RDI(0x22).CF(0x1), R = True)
    .case(run_bytes = intel(["and rdi, 0x12"]), R = False)
    .case(run_bytes = intel(["and rdi, rax"]), R = True),

    VerifyTest("and 2 variants").tags({"min", "and"})
    .bytes(intel(["and rsi, 0xff00ff", "and rdi, 0x1"]))
    .DI(S(0x100).RSI(0xff12ee).RDI(0x101).aflags(0))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True)
    .case(run_bytes = intel(["and rax, 0x12"]), R = True)
    .case(run_bytes = intel(["and rsi, 0x101"]), R = True)
    # Following test have their immediates of incorrect sizes -- should not work even with
    # --reduce_imms
    .case(run_bytes = intel(["and rsi, rbx"]), R = False)
    .case(run_bytes = intel(["and rdi, 0x7fff"]), R = True)
}

circuitous_tests = [test_and]
