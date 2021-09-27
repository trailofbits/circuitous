# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_all = {
    VerifyTest("adc, add, sub, sbb, shl, shr, xor, or, and, div, idiv").tags({"min", "big"})
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
}

circuitous_tests = [test_all]
