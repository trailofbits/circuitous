# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

test_pop = {
    VerifyTest("pop-a").tags({"pop", "min", "mem"})
    .bytes(intel(["pop rax"]))
    .DI(S(0x20000).RAX(0x56).RIP(0x1000).rmem(0x20000, "0000ffeeddccbbaa998877665544332211"))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = intel(["pop rbx"]), R = True)
    .case(run_bytes = intel(["pop rdx"]), R = True)
    .case(run_bytes = intel(["pop rbp"]), R = True)
    .case(run_bytes = intel(["pop r9"]), R = False),

    VerifyTest("pop-b_[rsp]").tags({"pop", "min", "mem"})
    .bytes(intel(["pop QWORD PTR [rsp]"]))
    .DI(S(0x20000).RSP(0x3000).RIP(0x1000).RAX(0x5000)
                   .rwmem(0x3000, "0040" + 6 * "00")
                   .rwmem(0x4000, "0030" + 6 * "00")
                   .rwmem(0x5000, "00f0" + 6 * "00"))
    .case(run_bytes = 0, R = True)
    # `pop [rsp]` uses different semantic function than other `pop [gpr]``
    .case(run_bytes = intel(["pop QWORD PTR [rax]"]), R=False),

    VerifyTest("pop-c_[rax]").tags({"pop", "min", "mem"})
    .bytes(intel(["pop QWORD PTR [rsp]"]))
    .DI(S(0x20000).RSP(0x3000).RIP(0x1000).RAX(0x5000).RBX(0x6211)
                   .rwmem(0x3000, "0040" + 6 * "00")
                   .rwmem(0x4000, "0030" + 6 * "00")
                   .rwmem(0x5000, "00f0" + 6 * "00")
                   .rwmem(0x6210, "00" * 16))
    .case(run_bytes = 0, R = True)
    # `pop [rsp]` uses different semantic function than other `pop [gpr]``
    .case(run_bytes = intel(["pop QWORD PTR [rbx]"]), R=False),

    VerifyTest("pop-d_pop_[rsp_minus_0x20]").tags({"pop", "min", "mem"})
    .bytes(intel(["pop QWORD PTR [rsp - 0x20]"]))
    .DI(S(0x20000).RSP(0x3000).RIP(0x1000)
                   .rwmem(0x2ff8, "00" * 8)
                   .rwmem(0x3000, "0040" + 30 * "00")
                   .rwmem(0x4000, "0030" + 6 * "00"))
    .case(run_bytes = 0, R = True)
}


test_pop_corners = {
    # There is bug in microx, therefore test must be specified manually
    Test("pop-e-rsp").tags({"min", "pop", "mem"})
    .bytes(intel(["pop rsp"]))
    .mode("--verify")
    .DI(S(0x23000).RSP(0x3000).RIP(0x1000)
                  .rwmem(0x3000, "0040" + 6 * "00")
                  .rwmem(0x4000, "0030" + 6 * "00"))
    .case(run_bytes = 0,
          DE = MS().RSP(0x4000).RIP(0x1001).ts(1).mem_hint(MemHint.read(0x3000, 0x4000, 8)),
          R=True),

}

circuitous_tests = [test_pop, test_pop_corners]
