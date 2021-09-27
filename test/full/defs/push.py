# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint
push_corners = {
    VerifyTest("push-a_rsp").tags({"min", "push", "mem"})
    .bytes(intel(["push rsp"]))
    .DI(S(0x23000).RSP(0x3008).RIP(0x1000)
                  .rwmem(0x3000, "0040" + 6 * "00")
                  .rwmem(0x4000, "0030" + 6 * "00"))
    .case(run_bytes = 0, R = True)
}

push = {
    VerifyTest("push-b").tags({"push", "min", "mem"}).bytes(intel(["push rax"]))
    .DI(S(0x40040).RSP(0x20040).RIP(0x60000).RAX(0x56)
        .rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = intel(["push rbx"]), R = True)
    .case(run_bytes = intel(["push rdx"]), R = True)
    .case(run_bytes = intel(["push rbp"]), R = True)
    .case(run_bytes = intel(["push r9"]), R = False)
}

push_cx = {
    VerifyTest("push-c_cx").tags({"push", "mem"}).bytes(intel(["push cx"]))
    .DI(S(0x20040).RAX(0x56).RIP(0x1000).rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = intel(["push bx"]), R = True)
    .case(run_bytes = intel(["push dx"]), R = True)
    .case(run_bytes = intel(["push bp"]), R = True)
    .case(run_bytes = intel(["push rcx"]), R = False)
}

push_deref = {
    VerifyTest("push-d_deref").tags({"push", "min", "mem"})
    .bytes(intel(["push -0x5[rax]"]))
    .DI(S(0x20040).RAX(0x50056).RIP(0x1000).RDX(0x70070).RBX(0x40040).RSP(0x9008)
                  .rwmem(0x40030, "3132e3" * 25)
                  .rwmem(0x50048, "afb14c" * 64)
                  .rwmem(0x40030, "ee" * 100)
                  .rwmem(0x70000, "ba12" * 100)
                  .rwmem(0x9008, "a125be" * 25))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = intel(["push 0x4[rax]"]), R = True)
    .case(run_bytes = intel(["push 0x5[rbx]"]), R = True)
    .case(run_bytes = intel(["push -0x35[rdx]"]), R = True)
    .case(run_bytes = intel(["push [rax]"]), R = False)
}

circuitous_tests = [push_corners, push, push_cx, push_deref]
