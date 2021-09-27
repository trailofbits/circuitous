# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_xor = {
    VerifyTest("xor-a").tags({"xor"})
    .bytes(intel(["xor rsi, rdi"]))
    .DI(S(0x100).RSI(0x42).RDI(0x42).OF(0x1))
    .case(R = True)
    .case(DI = MS().RSI(0x2), DE = MS().uAF(), R =  True)
    .case(DI = MS().RDI(0x22).CF(0x1), DE = MS().uAF(), R =  True)
    .case(run_bytes = intel(["xor rdi, 0x12"]), DE = MS().uAF(), R =  False)
    .case(run_bytes = intel(["xor rdi, rax"]), DE = MS().uAF(), R =  True),

    VerifyTest("xor-b_2_variants").tags({"min", "xor"})
    .bytes(intel(["xor rsi, 0xff00ff", "xor rdi, 0x1"]))
    .DI(S(0x100).RSI(0xff12ee).RDI(0x101).aflags(0))
    .case(run_bytes = 0, DE = MS().uAF(), R =  True)
    .case(run_bytes = 1, DE = MS().uAF(), R =  True)
    .case(run_bytes = intel(["xor rax, 0x12"]), DE = MS().uAF(), R =  True)
    .case(run_bytes = intel(["xor rsi, 0x101"]), DE = MS().uAF(), R =  True)
    # Following test have their immediates of incorrect sizes -- should not work even with
    # --reduce_imms
    .case(run_bytes = intel(["xor rsi, rax"]), DE = MS().uAF(), R =  False)
    .case(run_bytes = intel(["xor rdi, 0x7fff"]), DE = MS().uAF(), R =  True)
}

test_xor_reg_parts = {
    VerifyTest("xor-c_rnw_rnw_full").tags({"xor"})
    .bytes(intel(["xor r10w, ax"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["xor dx, r8w"]), R=True)
    .case(run_bytes = intel(["xor r8w, cx"]), R=True)
    .case(run_bytes = intel(["xor r8w, r9w"]), R=True),

    VerifyTest("xor-d_rnb_rnb_full").tags({"xor"})
    .bytes(intel(["xor r10b, bl"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["xor al, r8b"]), R=True)
    .case(run_bytes = intel(["xor r8b, cl"]), R=True)
    .case(run_bytes = intel(["xor r8b, r9b"]), R=True),

    VerifyTest("xor-e_8_lift").tags({"xor"})
    .DI(S(0x250))
    .bytes(intel(["xor al, r8b"])).case(run_bytes = 0, DE = MS().uAF(), R = True),

    VerifyTest("xor-f_16_lift").tags({"xor"})
    .DI(S(0x250))
    .bytes(intel(["xor ax, r8w"])).case(run_bytes = 0, DE = MS().uAF(), R = True),

    VerifyTest("xor-g_32_lift").tags({"xor"})
    .DI(S(0x250))
    .bytes(intel(["xor eax, ebx"])).case(run_bytes = 0, DE = MS().uAF(), R = True),
}

xor_mem = {
    VerifyTest("xor mem reg").tags({"xor", "mem", "min"})
    .bytes(intel(["xor [rax], rbx"]))
    .DI(S(0x2005).wrm_RAX(0x324d).wrm_RBX(0x4521).wrm_RSI(0x51b1).wrm_RDX(0x6ac1)
                 .wrm_R8(0x7128))
    .case(run_bytes = 0, DE = MS().uAF(), R = True)
    .scases(intel, ["xor [rbx], rdx", "xor [rsi], rsi", "xor [rdx], rax"],
            DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor [r8], rax"]), DE = MS().uAF(), R = True)
}

circuitous_tests = [test_xor, xor_mem]
