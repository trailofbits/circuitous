# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS


test_shr = {
    VerifyTest("shr rax").tags({"shr"}).bytes(intel(["shr rax"]))
    .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
    .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

    VerifyTest("shr rbx imm8").tags({"min", "shr"}).bytes(intel(["shr rbx, 0x5"]))
    .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
    .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

    VerifyTest("shr rbx 0").tags({"min", "shr"}).bytes(intel(["shr rbx, 0"]))
    .case(I = S(0x100).RBX(0b101).aflags(0), R = True)
    .case(I = S(0x100).RBX(0b101).aflags(1), R = True),

    VerifyTest("shr rdx cl").tags({"shr"}).bytes(intel(["shr rdx, cl"]))
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65), R = True)
}

circuitous_tests = [test_shr]
