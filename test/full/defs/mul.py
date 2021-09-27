# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_mul = {
    VerifyTest("mul-a_r32_r32").tags({"min", "mul"}).bytes(intel(["mul eax"]))
    .DI(S(0x100).RIP(0x10000).RAX(0xff4).RBX(0x5).RCX(0x21).RDX(0x121).aflags(0))
    .scases(intel, ["mul eax", "mul edx"], DE = MS().uAF().uZF().uSF().uPF(), R = True)
    .scases(intel, ["mul r8d", "mul r10d"], DE = MS().uAF().uZF().uSF().uPF(), R = False)
}

circuitous_tests = [test_mul]
