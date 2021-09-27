# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest

test_lea = {
    VerifyTest("lea-a").bytes(intel(["lea rdi, [rsi - 0x15]"])).tags({"lea"})
    .case("rsi:=0x15", I = S(0x100).RSI(0x15), R = True)
    .case("rsi:=0x0", I = S(0x100).RSI(0x0), R = True)
    .case("rsi:=0xffffffffffffff", I = S(0x100).RSI(0xffffffffffffffff), R = True)
}

test_lea_addr_ops = {
    VerifyTest("lea-b_addr_op").tags({"lea", "addr_op"})
    .bytes(intel(["lea rax, [rax + 2 * rbx + 0x4]"]))
    .DI(S(0x250).aflags(0).RAX(0x10).RBX(0x20).RCX(0x30).RDX(0x40))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["lea rax, [rcx + 2 * rbx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 4 * rbx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rcx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 2 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 2 * rbx - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 1 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 2 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 4 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 8 * rbx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rbx - 0x10]"]), R=True),

    VerifyTest("lea-c_addr_op_long_disp").tags({"lea", "addr_op"})
    .bytes(intel(["lea rax, [rax + 2 * rbx + 0xffffffff4ffefff]"]))
    .DI(S(0x250).aflags(0).RAX(0x10).RBX(0x20).RCX(0x30).RDX(0x40))
    .case(run_bytes = intel(["lea rax, [rax + 2 * rbx + 0xffcff8ff4ffefff]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rbx + 0xfffff2ff4ffefff]"]), R=True),

    VerifyTest("lea-d_addr op missing scale").tags({"lea", "addr_op"})
    .bytes(intel(["lea r8, [rax + r8]"]))
    .DI(S(0x250).aflags(0).RAX(0x10).RBX(0x20).RCX(0x30).RDX(0x40).R8(0x50).R9(0x60).R10(0x70))
    .case(run_bytes = intel(["lea r9, [rbx + rcx]"]), R=True)
    .case(run_bytes = intel(["lea r9, [rbx + 0xff]"]), R=False)
    .case(run_bytes = intel(["lea r9, [rbx + r9]"]), R=True)
    .case(run_bytes = intel(["lea r9, [r10 + rcx]"]), R=True)
    .case(run_bytes = intel(["lea r9, [r10 + 2 * rax + rcx]"]), R=True),

    VerifyTest("lea-d_addr_op").tags({"lea", "addr_op", "min"})
    .bytes(intel(["lea rax, [rax + rbx]",
                  "lea rax, [rbx + 0x01]",
                  "lea rcx, [rcx + 2 * rdx + 0x5]",
                  "lea rcx, [rax + 0x0]",
                  "lea rax, [rbx + 0xf543f]",
                  "lea rcx, [rcx + 2 * rdx + 0x5fdfd12]"]))
    .DI(S(0x250).aflags(0).RAX(0x10).RBX(0x20).RCX(0x30).RDX(0x40).R8(0x50).R9(0x60).R10(0x70))
    .case(run_bytes = intel(["lea rcx, [rax - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea rdx, [rax + 8 * rbx + 0x7fff]"]), R=True)
    .case(run_bytes = intel(["lea rbx, [rcx + 0x7ffffff]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rbx + r8]"]), R=True)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rax + 0x0]"]), R=True)
    .case(run_bytes = intel(["lea rcx, [rcx + 8 * rcx"]), R=True)
    .case(run_bytes = intel(["lea rcx, [rdx + 1 * rbx - 0x60]"]), R=True)
    .case(run_bytes = intel(["lea r10, [rax - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea r9, [rax + 8 * r8 + 0x7fff]"]), R=True)
    .case(run_bytes = intel(["lea rbx, [r10 + 0x7ffffff]"]), R=True)
    .case(run_bytes = intel(["lea r10, [rbx + r8]"]), R=True)
    .case(run_bytes = intel(["lea rax, [r9 + 2 * rax + 0x0]"]), R=True)
    .case(run_bytes = intel(["lea r10, [r9 + 8 * r9"]), R=True)
    .case(run_bytes = intel(["lea r9, [rdx + 1 * r8 - 0x60]"]), R=True),

    VerifyTest("lea-d-1").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rax, [rax + rbx]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-2").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rax, [rbx + 0xff]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-3").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rcx, [rcx + 2 * rdx + 0x5]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-4").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rcx, [rax + 0x0]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-5").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rax, [rbx + 0xf543f]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-6").tags({"lea", "addr_op"})
    .DI(S(0x250))
    .bytes(intel(["lea rcx, [rcx + 2 * rdx + 0x5fdfd12]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-7").tags({"lea", "addr_op"})
    .DI(S(0xffffffffff250))
    .bytes(intel(["lea eax, [ebx + r8d]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-d-8").tags({"lea", "addr_op"})
    .DI(S(0xffffffffff250))
    .bytes(intel(["lea r8d, [ebx + eax]"])).case(run_bytes = 0, R=True),

    VerifyTest("lea-e_32b").tags({"lea", "addr_op", "min"})
    .bytes(intel(["lea eax, [ebx + 2 * ecx + 0x4]"]))
    .DI(S(0x250).aflags(0).RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["lea eax, [ecx + 2 * ebx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea eax, [eax + 4 * ebx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea eax, [eax + 2 * ecx + 0x4]"]), R=True)
    .case(run_bytes = intel(["lea eax, [eax + 2 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 2 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 2 * ebx - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 1 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 2 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 4 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 8 * ebx + 0x5]"]), R=True)
    .case(run_bytes = intel(["lea eax, [eax + 2 * ebx - 0x10]"]), R=True),

    VerifyTest("lea-f_addr_op_missing scale").tags({"lea", "addr_op"})
    .bytes(intel(["lea r8d, [eax + r8d]"]))
    .DI(S(0x250).aflags(0).RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40))
    .case(run_bytes = intel(["lea r9d, [ebx + ecx]"]), R=True)
    .case(run_bytes = intel(["lea r9d, [ebx + 0xff]"]), R=False)
    .case(run_bytes = intel(["lea r9d, [ebx + r9d]"]), R=True)
    .case(run_bytes = intel(["lea r9d, [r10d + ecx]"]), R=True)
    .case(run_bytes = intel(["lea r9d, [r10d + 2 * eax + ecx]"]), R=True),

    VerifyTest("lea-g_addr_op_reject").tags({"lea", "addr_op", "reject"})
    .bytes(intel(["lea eax, [ebx + 2 * ecx + 0x4]"]))
    .DI(S(0xffffffffffff43))
    .case(run_bytes = intel(["lea rax, [rbx + 2 * rcx + 0x4]"]), R=False),

    VerifyTest("lea-h_addr_op_32b").tags({"lea", "addr_op"})
    .bytes(intel(["lea eax, [ebx + r15d]",
                  "lea r8d, [ebx + eax]"]))
    .DI(S(0xfffffffff250).aflags(0).RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40).R8(0xffffffffff50).R9(0xffffffffff60)
    .R10(0xffffffffff70)
    .R14(0xffffffffff80)
    .R15(0xffffffffff90))
    .case(run_bytes = intel(["lea eax, [ebx + r8d]"]), R=True),

    VerifyTest("lea-i_addr_op_32b").tags({"lea", "addr_op"})
    .bytes(intel(["lea eax, [ebx + r15d]",
                  "lea r8d, [ebx + eax]"]))
    .DI(S(0xfffffffff250).aflags(0).RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40).R8(0xffffffffff50).R9(0xffffffffff60)
    .R10(0xffffffffff70)
    .R14(0xffffffffff80)
    .R15(0xffffffffff90))
    .case(run_bytes = intel(["lea eax, [ebx + r8d]"]), R=True),


    VerifyTest("lea-j").tags({"lea", "addr_op"})
    .bytes(intel(["lea eax, [eax + ebx]", "lea eax, [ebx + 0x01]"]))
    .DI(S(0xfffffffff250).aflags(0).RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40).R8(0xffffffffff50).R9(0xffffffffff60)
    .R10(0xffffffffff70)
    .R14(0xffffffffff80)
    .R15(0xffffffffff90))
    .case(run_bytes = intel(["lea eax, [eax + 2 * eax + 0x0]"]), R=True),


    VerifyTest("lea-k_addr_op permutations_32b").tags({"lea", "addr_op", "min"})
    .bytes(intel(["lea eax, [eax + ebx]", "lea eax, [ebx + 0x01]",
                  "lea ecx, [ecx + 2 * edx + 0x5]", "lea ecx, [eax + 0x0]",
                  "lea eax, [ebx + 0xf543f]", "lea ecx, [ecx + 2 * edx + 0x5fdfd12]",
                  "lea ecx, [r9d + 0x5fdfd12]", "lea ecx, [ecx + 2 * r9d + 0x5]",
                  "lea r9d, [ebx + 0x01]", "lea eax, [ebx + r15d]",
                  "lea r8d, [ebx + eax]", "lea eax, [r8d + 8 * ebx + 0x7fff]"]))
    .DI(S(0xfffffffff250).aflags(0)
    .RAX(0xffffffffff10).RBX(0xffffffffff20).RCX(0xffffffffff30)
    .RDX(0xffffffffff40).R8(0xffffffffff50).R9(0xffffffffff60)
    .R10(0xffffffffff70).R14(0xffffffffff80).R15(0xffffffffff90))
    .case(run_bytes = intel(["lea ecx, [eax - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea edx, [eax + 8 * ebx + 0x7fff]"]), R=True)
    .case(run_bytes = intel(["lea ebx, [ecx + 0x7ffffff]"]), R=True)
    .case(run_bytes = intel(["lea eax, [ebx + r8d]"]), R=True)
    .case(run_bytes = intel(["lea eax, [eax + 2 * eax + 0x0]"]), R=True) #
    .case(run_bytes = intel(["lea ecx, [ecx + 8 * ecx"]), R=True) #
    .case(run_bytes = intel(["lea ecx, [edx + 1 * ebx - 0x60]"]), R=True)
    .case(run_bytes = intel(["lea r10d, [eax - 0x5]"]), R=True)
    .case(run_bytes = intel(["lea r9d, [eax + 8 * r8d + 0x7fff]"]), R=True)
    .case(run_bytes = intel(["lea ebx, [r10d + 0x7ffffff]"]), R=True)
    .case(run_bytes = intel(["lea r10d, [ebx + r8d]"]), R=True)
    .case(run_bytes = intel(["lea eax, [r9d + 2 * eax + 0x0]"]), R=True) #
    .case(run_bytes = intel(["lea r10d, [r9d + 8 * r9d"]), R=True)
    .case(run_bytes = intel(["lea r9d, [edx + 1 * r8d - 0x60]"]), R=True)
    .case(run_bytes = intel(["lea r15d, [ebx + r8d]"]), R=True)
    .case(run_bytes = intel(["lea eax, [r15d + 2 * eax + 0x0]"]), R=True)
    .case(run_bytes = intel(["lea r15d, [r15d + 8 * r15d"]), R=True)
    .case(run_bytes = intel(["lea r15d, [edx + 1 * r8d - 0x60]"]), R=True)
    .case(run_bytes = intel(["lea r14d, [ebx + r8d]"]), R=True)
    .case(run_bytes = intel(["lea eax, [r14d + 2 * eax + 0x0]"]), R=True)
    .case(run_bytes = intel(["lea r14d, [r15d + 8 * r15d"]), R=True)
    .case(run_bytes = intel(["lea r14d, [edx + 1 * r8d - 0x60]"]), R=True),

    VerifyTest("lea-l_addr_op_permutations_32b_reject").tags({"lea","addr_op", "reject"})
    .bytes(intel(["lea eax, [eax + ebx]", "lea eax, [ebx + 0x01]",
                  "lea ecx, [ecx + 2 * edx + 0x5]", "lea ecx, [eax + 0x0]",
                  "lea eax, [ebx + 0xf543f]", "lea ecx, [ecx + 2 * edx + 0x5fdfd12]",
                  "lea r8d, [ebx + eax]", "lea eax, [r8d + 8 * ebx + 0x7fff]"]))
    .DI(S(0x250).aflags(0).RAX(0x10).RBX(0x20).RCX(0x30).RDX(0x40).R8(0x50).R9(0x60).R10(0x70))
    .case(run_bytes = intel(["lea rcx, [rax - 0x5]"]), R=False)
    .case(run_bytes = intel(["lea rdx, [rax + 8 * rbx + 0x7fff]"]), R=False)
    .case(run_bytes = intel(["lea rbx, [rcx + 0x7ffffff]"]), R=False)
    .case(run_bytes = intel(["lea rax, [rbx + r8]"]), R=False)
    .case(run_bytes = intel(["lea rax, [rax + 2 * rax + 0x0]"]), R=False)
    .case(run_bytes = intel(["lea rcx, [rcx + 8 * rcx"]), R=False)
    .case(run_bytes = intel(["lea rcx, [rdx + 1 * rbx - 0x60]"]), R=False)
    .case(run_bytes = intel(["lea r10, [rax - 0x5]"]), R=False)
    .case(run_bytes = intel(["lea r9, [rax + 8 * r8 + 0x7fff]"]), R=False)
    .case(run_bytes = intel(["lea rbx, [r10 + 0x7ffffff]"]), R=False)
    .case(run_bytes = intel(["lea r10, [rbx + r8]"]), R=False)
    .case(run_bytes = intel(["lea rax, [r9 + 2 * rax + 0x0]"]), R=False)
    .case(run_bytes = intel(["lea r10, [r9 + 8 * r9"]), R=False)
    .case(run_bytes = intel(["lea r9, [rdx + 1 * r8 - 0x60]"]), R=False),

    VerifyTest("lea-m_self").tags({"lea", "addr_op"})
    .bytes(intel(["lea eax, [eax]"]))
    .DI(S(0x2fff50).aflags(0).RAX(0xffffff10).RBX(0xfffffff20).RCX(0xfffff30).R15(0xfffff40))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["lea rax, [rax]"]), R=False)
    .case(run_bytes = intel(["lea r15d, [r15d]"]), R=False)
    .case(run_bytes = intel(["lea ecx, [ecx + 1 * ebx]"]), R=False)
    .case(run_bytes = intel(["lea eax, [ecx + 2 * eax - 0x5]"]), R=False)
    .case(run_bytes = intel(["lea ebx, [ecx + 0x5]"]), R=False)
    .case(run_bytes = intel(["lea ebx, [ecx - 0x5]"]), R=False)
    .case(run_bytes = intel(["lea eax, [ebx]"]), R=True)
    .case(run_bytes = intel(["lea ecx, [ecx]"]), R=True),
}

circuitous_tests = [test_lea, test_lea_addr_ops]
