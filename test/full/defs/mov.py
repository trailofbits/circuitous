# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint
import tools.tgen as tgen

test_mov = {
    Test("mov-a_imm_rdx") .bytes("ba12000000").tags({"mov"})
    .DI(S(0x100).RIP(0x1000))
    .mode("--verify")
    .case(DE = MS().RDX(0x12).RIP(0x1005).ts(1), R = True)
    .case(DE = MS().RDX(0x12000000).RIP(0x1005).ts(1), R = False)
    .case(DE = MS().RDX(0x13).RIP(0x1005).ts(1), run_bytes = "ba13000000", R = True)
    .case(I = S(0x0).RIP(0x1000),
          E = S(0x0).RDX(0x13).RIP(0x1005).ts(1),
          run_bytes = "ff13000000",
          R = False),

    Test("mov-b_imm_eax.ebx.ecx.edx") \
    .bytes("b812000000bb12000000b912000000ba12000000")
    .tags({"min", "mov"})
    .mode("--verify")
    .DI(S(0x100).RAX(0x42).RBX(0x42).RCX(0x42).RDX(0x42).RIP(0x1000).ts(0))
    .case("mov rax",
          DE = MS().RAX(0x12).RIP(0x1005).ts(1),
          run_bytes = "b812000000",
          R = True)
    .case("mov rbx",
          DE = MS().RBX(0x12).RIP(0x1005).ts(1),
          run_bytes = "bb12000000",
          R = True)
    .case("mov rcx",
          DE = MS().RCX(0x12).RIP(0x1005).ts(1),
          run_bytes = "b912000000",
          R = True)
    .case("mov rdx",
          DE = MS().RDX(0x12).RIP(0x1005).ts(1),
          run_bytes = "ba12000000",
          R = True)
    .case("mov rbx, modify rax",
          DE = MS().RBX(0x12).RAX(0x12).RIP(0x1005).ts(1),
          run_bytes = "bb12000000",
          R = False)
    .case("false",
          DE = MS().RAX(0x14).RIP(0x1005).ts(1),
          run_bytes = "b812000000",
          R = False),

    Test("mov-c_reg_reg") \
    .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
    .tags({"min", "mov"})
    .mode("--verify")
    .DI(S(0x100).RAX(0x12).RBX(0x22).RCX(0x32).RIP(0x1000).ts(0))
    .case("rax:=rbx",
          DE = MS().RAX(0x22).RIP(0x1003).ts(1),
          run_bytes = 0,
          R = True)
    .case("rcx:=rax",
          DE = MS().RCX(0x12).RIP(0x1003).ts(1),
          run_bytes = 1,
          R = True)
    .case("rbx:=rax",
          DE = MS().RBX(0x12).RIP(0x1003).ts(1),
          run_bytes = 2,
          R = True)
    .case("F:rbx:=rax",
          DE = MS().RBX(0x11).RIP(0x1003).ts(1),
          run_bytes = 2,
          R = False),

    Test("mov-d_reg_reg") \
    .bytes(intel(["mov rax, rbx", "mov rcx, rax", "mov rbx, rax"]))
    .tags({"min", "mov"})
    .mode("--verify")
    .DI(S(0x100).RAX(0x12).RBX(0x22).RCX(0x32).ts(0))
    .case("rax:=rbx",
          DE = MS().RAX(0x32).RIP(0x1003).ts(1),
          run_bytes = 0,
          R = False)
    .case("rcx:=rax",
          DE = MS().RCX(0x32).RIP(0x1003).ts(1),
          run_bytes = 1,
          R = False)
    .case("rbx:=rax",
          DE = MS().RBX(0x22).RIP(0x1003).ts(1),
          run_bytes = 2,
          R = False),

    VerifyTest("mov-e_0xffffffffffffffff_rdx")\
    .bytes(intel(["mov rdx, 0xffffffffffffffff"]))
    .tags({"min", "mov"})
    .DI(S(0x1))
    .case(R = True),

    VerifyTest("mov-f_0x7fffffffffffffff_rdx")\
    .bytes(intel(["mov rdx, 0x7fffffffffffffff"]))
    .tags({"min", "mov"})
    .DI(S(0x1))
    .case(R = True)
}

test_mov_reduce = {
    VerifyTest("mov-g_reg_imm").tags({"mov"})
    .bytes(intel(["mov rax, 0x12"]))
    .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
    .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rax, 0x22"]), R=True)
    .case(run_bytes = intel(["mov rbx, 0x22"]), R=True)
    .case(run_bytes = intel(["mov rcx, 0x22"]), R=True)
    .case(run_bytes = intel(["mov rdx, 0x22"]), R=True),

    VerifyTest("mov-h_reg_reg").tags({"mov"})
    .bytes(intel(["mov rcx, rax"]))
    .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
    .case(run_bytes = intel(["mov rax, rcx"]), R=True)
    .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
    .case(run_bytes = intel(["mov rcx, rax"]), R=True)
    .case(run_bytes = intel(["mov rdx, rax"]), R=True),

    VerifyTest("mov-i_reduce_imm_reg").tags({"mov"})
    .bytes(intel(["mov rax, 0x12", "mov rcx, rax"]))
    .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
    .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rax, rcx"]), R=True)
    .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
    .case(run_bytes = intel(["mov rcx, rax"]), R=True)
    .case(run_bytes = intel(["mov rdx, rax"]), R=True),
}

test_mov_reg_parts = {
    VerifyTest("mov-j_reg16_reg16").tags({"mov", "min"})
    .bytes(intel(["mov ax, bx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov cx, dx"]), R=True),

    VerifyTest("mov-k-reg16.32_reg16.32").tags({"mov"})
    .bytes(intel(["mov ax, bx", "mov eax, ebx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov cx, dx"]), R=True)
    .case(run_bytes = intel(["mov ecx, edx"]), R=True),

    # TODO(lukas): So look at the following encodings:
    #              89 d8           mov eax, ebx
    #              44 89 c0        mov eax, r8d
    # Eventually we want to be able to derive one from the other but
    # we cannot atm; test is marked as todo.
    VerifyTest("mov-l_reg32.r8d_reg32.r8d_partial").tags({"mov", "todo"})
    .bytes(intel(["mov eax, ebx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov ecx, edx"]), R=True)
    .case(run_bytes = intel(["mov edx, r8d"]), R=True)
    .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
    .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

    # TODO(lukas): Same as above
    VerifyTest("mov-m_reg64.32.16_reg64.32.16.imm64.32.16_partial")
    .tags({"mov", "todo"})
    .bytes(intel(["mov rax, rbx", "mov eax, ebx", "mov ax, bx",
                  "mov rax, 0x12", "mov eax, 0x12", "mov ax, 0x12"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov cx, dx"]), R=True)
    .case(run_bytes = intel(["mov cx, r8w"]), R=False)
    .case(run_bytes = intel(["mov r9w, bx"]), R=False)
    .case(run_bytes = intel(["mov ecx, edx"]), R=True)
    .case(run_bytes = intel(["mov ecx, r8d"]), R=True)
    .case(run_bytes = intel(["mov r9d, ebx"]), R=True)
    .case(run_bytes = intel(["mov cx, 0x7fff"]), R=True)
    .case(run_bytes = intel(["mov r8w, 0xffff"]), R=True)
    .case(run_bytes = intel(["mov r9w, 0xab"]), R=True)
    .case(run_bytes = intel(["mov ecx, 0x7fffffff"]), R=True)
    .case(run_bytes = intel(["mov r8d, 0xffffffff"]), R=True)
    .case(run_bytes = intel(["mov r9d, 0xab"]), R=True),

    VerifyTest("mov-n_reg32.r8d_reg32.r8d_full").tags({"mov", "min"})
    .bytes(intel(["mov eax, ebx", "mov r10d, ebx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov ecx, edx"]), R=True)
    .case(run_bytes = intel(["mov edx, r8d"]), R=True)
    .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
    .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

    VerifyTest("mov-o-rnd_rnd_full").tags({"mov"})
    .bytes(intel(["mov r10d, ebx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov edx, r8d"]), R=True)
    .case(run_bytes = intel(["mov r8d, ecx"]), R=True)
    .case(run_bytes = intel(["mov r8d, r9d"]), R=True),

    VerifyTest("mov-p_rnw_rnw_full").tags({"mov"})
    .bytes(intel(["mov r10w, ax"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov dx, r8w"]), R=True)
    .case(run_bytes = intel(["mov r8w, cx"]), R=True)
    .case(run_bytes = intel(["mov r8w, r9w"]), R=True),

    VerifyTest("mov-q_rnb_rnb_full").tags({"mov"})
    .bytes(intel(["mov r10b, bl"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov al, r8b"]), R=True)
    .case(run_bytes = intel(["mov r8b, cl"]), R=True)
    .case(run_bytes = intel(["mov r8b, r9b"]), R=True),

    VerifyTest("mov-r-rnd_rnd_full").tags({"mov"})
    .bytes(intel(["mov eax, ebx"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov edx, eax"]), R=True)
    .case(DI=MS().RAX(0xffffffffffff), run_bytes = intel(["mov edx, eax"]), R=True)
    .case(run_bytes = intel(["mov ecx, ecx"]), R=True)
    .case(DI=MS().RCX(0xffffffffffff), run_bytes = intel(["mov ecx, ecx"]), R=True)
    .case(run_bytes = intel(["mov eax, ebx"]), R=True),

    VerifyTest("mov-s_al/ah_versions").tags({"mov", "min"})
    .bytes(intel(["mov ah, al"]))
    .DI(S(0x200).RAX(0xffee). RBX(0xccbb).RCX(0xaa99))
    .case(run_bytes = intel(["mov bh, al"]), R=True)
    .case(run_bytes = intel(["mov ch, bh"]), R=True)
    .case(run_bytes = intel(["mov cl, bh"]), R=True)
    .case(run_bytes = intel(["mov cl, bl"]), R=True),


    VerifyTest("mov-t_reg64.32.16_reg64.32.16.imm64.32.16_full").tags({"mov"})
    .bytes(intel(["mov rax, rbx", "mov eax, ebx", "mov ax, bx",
                  "mov rax, 0x12", "mov eax, 0x12", "mov ax, 0x12",
                  "mov r8d, 0x12", "mov r8d, eax", "mov r8w, 0x12",
                  "mov r8w, ax"]))
    .DI(S(0x250).aflags(0).RAX(0x12).RBX(0x13).RCX(0x14).RDX(0x15).R8(0x16).R9(0x17))
    .case(run_bytes = intel(["mov cx, dx"]), R=True)
    .case(run_bytes = intel(["mov cx, r8w"]), R=True)
    .case(run_bytes = intel(["mov r9w, bx"]), R=True)
    .case(run_bytes = intel(["mov ecx, edx"]), R=True)
    .case(run_bytes = intel(["mov ecx, r8d"]), R=True)
    .case(run_bytes = intel(["mov r9d, ebx"]), R=True)
    .case(run_bytes = intel(["mov cx, 0x7fff"]), R=True)
    .case(run_bytes = intel(["mov r8w, 0xffff"]), R=True)
    .case(run_bytes = intel(["mov r9w, 0xab"]), R=True)
    .case(run_bytes = intel(["mov ecx, 0x7fffffff"]), R=True)
    .case(run_bytes = intel(["mov r8d, 0xffffffff"]), R=True)
    .case(run_bytes = intel(["mov r9d, 0xab"]), R=True)
}

mov_mem = {
    VerifyTest("mov-u_reg_dreg").tags({"mov", "min", "mem"})
    .bytes(intel(["mov rax, [rbx]"]))
    .DI(S(0x20040).wrm_RAX(0x50056, 65).wrm_RBX(0x4008, 0x100).wrm_RAX(0x9800, 0x100))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["mov [rax], rbx"]), R=False)
    .case(run_bytes = intel(["mov rdx, [rax]"]), R=True),

    VerifyTest("mov-v_dreg_reg").tags({"mov", "min", "mem"})
    .bytes(intel(["mov [rax], rbx"]))
    .DI(S(0x20040).wrm_RAX(0x50056, 65).wrm_RBX(0x4008, 0x100).wrm_RDX(0x9800, 0x100))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["mov rax, [rbx]"]), R=False)
    .case(run_bytes = intel(["mov [rdx], rax"]), R=True),

    VerifyTest("mov-w_rex_prefix").tags({"mov", "mem", "min"})
    .bytes(intel(["mov [r9], rax", "mov rax, [r9]"]))
    .DI(S(0x30048).wrm_RAX(0x30054, 0x100).wrm_RDX(0x40032, 0x100)
                  .wrm_R9(0x60421, 0x100).wrm_R8(0x70212, 0x100))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = 1, R=True)
    .scases(intel, ["mov [r8], r9", "mov [rdx], r8", "mov [r9], rax"], R=True),

    VerifyTest("mov-q_displaement").tags({"mov", "mem", "min"})
    .bytes(intel(["mov [rax + 2 * r9 - 0x5], rdx", "mov rax, [rcx + 2 * r9 + 0x4]"]))
    .DI(S(0x300049).wrm_RAX(0x8000).wrm_RBX(0x2000).wrm_RCX(0x3000).wrm_RSI(0x4000)
                   .wrm_R8(0x5000).wrm_R9(0x6000).wrm_R10(0x7000)
                   .mig(MIG().rw(0x13000, 0x1000)
                             .rw(0x14000, 0x1000)
                             .rw(0xe000, 0x1000)
                             .rw(0x12000, 0x1000)
                             .rw(0xd000, 0x1000)
                        )
        )
    .case(run_bytes = 0, R=True)
    .case(run_bytes = 0, R=True)
    .scases(intel, ["mov rax, [r9 + 2*rsi -0x5]", "mov r10, [rsi + 2 * r10 + 0x5]"], R=True)
    .scases(intel, ["mov [r9 + 2*rsi -0x5], rcx", "mov [rsi + 2 * r10 + 0x5], r8"], R=True),

    VerifyTest("mov-x_mem_imm").tags({"mov", "mem", "min"})
    .bytes(intel(["mov QWORD PTR [0x6531], 0x412"]))
    .DI(S(0x32100).mig(MIG().rw(0x6000, 0x1000).rw(0x10000, 0x1000).rw(0x20000, 0x1000)))
    .case(run_bytes = 0, R = True)
    .scases(intel, ["mov QWORD PTR [0x6531], 0x412a12", "mov QWORD PTR [0x101ab], 0x4ffbe",
                    "mov QWORD PTR [0x20aaa], 0x0"], R=True),

    VerifyTest("mov-y_mem_reg").tags({"mov", "mem", "min"})
    .bytes(intel(["mov QWORD PTR [0x6531], r9"]))
    .DI(S(0x32100).mig(MIG().rw(0x6000, 0x1000).rw(0x10000, 0x1000).rw(0x20000, 0x1000))
        .RAX(0xffaa).R10(0x431).RCX(0xbeac4))
    .case(run_bytes = 0, R = True)
    .scases(intel, ["mov QWORD PTR [0x6531], r10",
                    "mov QWORD PTR [0x101ab], rcx",
                    "mov QWORD PTR [0x20aaa], rax"], R=True)
    .case(run_bytes = intel(["mov QWORD PTR [0x101ab], 0x0"]), R=False),

    VerifyTest("mov-z_mem_reg32").tags({"mov", "mem", "32b"})
    .bytes(intel(["mov [0x6531], r9d"]))
    .DI(S(0x32100).mig(MIG().rw(0x6000, 0x1000).rw(0x10000, 0x1000).rw(0x20000, 0x1000))
        .RAX(0xaebbefc4ffaa).R10(0x44444ffbbcc31).RCX(0xb55ffcb1eac4).R12(0x432))
    .case(run_bytes = 0, R = True)
    .scases(intel, ["mov [0x6531], r10d",
                    "mov [0x101ab], r11d",
                    "mov [0x20aaa], r12d"], R=True)
    .case(run_bytes = intel(["mov QWORD PTR [0x101ab], 0x0"]), R=False)
    # This will fail because of prefixes
    .case(run_bytes = intel(["mov DWORD PTR [0x101ab], ecx"]), R=False)
    .case(run_bytes = intel(["mov QWORD PTR [0x101ab], rax"]), R=False),


    VerifyTest("mov-aa_rip_imm").tags({"mov", "mem"})
    .bytes(intel(["mov [rip + 0x1400], rax"]))
    .DI(S(0x3400).RIP(0x7000).mig(MIG().rw(0x8000, 0x1000)))
    .case(run_bytes = 0, R = True)
}

mov_selftest = {
  VerifyTest("selftest").tags({"mov", "reject"})
  .bytes(intel(["mov rdx, 0x12100"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, DE = MS().RDX(0x12), R = False)
}

mov_gen = {
    VerifyTest("mov-ab_gen") \
    .bytes(intel(tgen.mov()))
    .tags({"mov", "generated"})
    .DI(S(0x1).RAX(0x0).aflags(0))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True),
}

# To verify that cirucit fails is memory hints are incorrect
memhint_fails = {
    Test("mov-ac_incorrent_memhint") \
    .bytes(intel(["mov QWORD PTR [rax], 0x20"]))
    .tags({"mov", "min", "fail"})
    .mode("--verify")
    .DI(S(0x3040).RIP(0x8012).R8(0x2100).RAX(0x30080).aflags(0)
                 .rwmem(0x300500, "11" * 0x100).ts(0))
    .case(run_bytes = 0,
          DE = MS().RIP(0x8012 + 7)
                   .mem_hint(MemHint.write(0x2854, 0x20, 8)),
          R=False)
    .case(run_bytes = 0,
          DE = MS().RIP(0x8012 + 7)
                   .mem_hint(MemHint.write(0x300080, 0x21, 8)),
          R=False),
}

circuitous_tests = [test_mov, test_mov_reduce, test_mov_reg_parts,
                    mov_mem, mov_selftest, mov_gen, memhint_fails]
