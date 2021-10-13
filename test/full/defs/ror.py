# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_ror = {
    VerifyTest("ror_rax").tags({"ror"}).bytes(intel(["ror rax"]))
    .case(I = S(0x100).RAX(0b101).aflags(0), R = True)
    .case(I = S(0x100).RAX(0b1010).aflags(0), R = True),

    VerifyTest("ror_rbx_imm8").tags({"ror"}).bytes(intel(["ror rbx, 0x5"]))
    .case(I = S(0x100).RBX(0b1011111).aflags(0), R = True)
    .case(I = S(0x100).RBX(0b1000000).aflags(0), R = True),

    VerifyTest("ror_rbx_0").tags({"min", "ror"}).bytes(intel(["ror rbx, 0"]))
    .case(I = S(0x100).RBX(0b101).aflags(0), DE = MS(), R = True)
    .case(I = S(0x100).RBX(0b101).aflags(1), DE = MS(), R = True),

    # In `min` because it tests undefs.
    VerifyTest("ror_rdx_cl").tags({"ror", "min"}).bytes(intel(["rcl rdx, cl"]))
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(0), DE = MS().uOF(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(63).aflags(1), DE = MS().uOF(), R = True)
    # TODO(lukas): Not sure but 65 % 64 is 1 therefore `OF` should be defined
    #.case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65).aflags(1), DE = MS().uOF(), R = False)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(65).aflags(1), DE = MS(), R = True)
    # OF is defined since RCX is 1
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(1).aflags(0), DE = MS(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(1).aflags(1), DE = MS(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(1).aflags(0), DE = MS(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).RCX(1).aflags(1), DE = MS(), R = True)
}

circuitous_tests = [test_ror]
