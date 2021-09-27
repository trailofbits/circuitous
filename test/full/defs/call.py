# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

call = {
    VerifyTest("call-a_0xff4410").tags({"call", "cfg", "min"})
    .bytes("e800400000")
    .DI(S(0x4121).RSP(0x8116).mig(MIG().rw(0x8000, 0x1000)))
    .case(run_bytes = 0, R=True),

    VerifyTest("call-b_rax").tags({"call", "cfg", "min"})
    .bytes(intel(["call rax"]))
    .DI(S(0x4121)
        .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
        .RAX(0xfff77).RBP(0x4121a)
        .mig(MIG().rw(0x8000, 0x1000)))
    .scases(intel, ["call r8", "call r10", "call r15"], R=False)
    .scases(intel, ["call rax", "call rbp"], R=True)
    .case(run_bytes = intel(["call rsp"]), R=True),

    VerifyTest("call-c_r10").tags({"call", "cfg", "min"})
    .bytes(intel(["call r10"]))
    .DI(S(0x4121)
        .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
        .RAX(0xfff77).RBP(0x4121a)
        .mig(MIG().rw(0x8000, 0x1000)))
    .scases(intel, ["call r8", "call r10", "call r15"], R=True)
    .scases(intel, ["call rax", "call rsp"], R=False),

    VerifyTest("call-d_dreg").tags({"call", "cfg", "min"})
    .bytes(intel(["call [rsp + 8]"]))
    .DI(S(0x4121)
        .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
        .RAX(0xfff77).RBP(0x4121a)
        .rwmem(0x8108, "00405060ffae43112012405055657a98aefc31321291a3fcfecf718a981721"))
    .case(run_bytes = 0, R = True),
}

circuitous_tests = [call]
