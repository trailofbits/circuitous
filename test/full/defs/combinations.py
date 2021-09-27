# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, random_state

test_combinations = {
    # If in reduce_imms we do not check that opcodes (as well as operands)
    # must be equal, this should fire
    VerifyTest("encoding adc, sbb").tags({"multiple", "adc", "sbb"})
    .bytes(intel(["adc rax, 0x12",
                  "sbb rax, 0x12"]))
    .DI(S(0x100).RAX(0x12).RCX(0x45).aflags(0))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = 1, R=True),

    VerifyTest("add/mov reg/reg, reg/imm").tags({"multiple", "min", "mov", "add"})
    .bytes(intel(["mov rax, 0x12", "add rax, 0x12", "mov rbx, rcx", "add r8, rax"]))
    .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff)
                .R8(0x12).R9(0x22).RSP(0x2000).aflags(0))
    .case(run_bytes = intel(["mov rax, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rbx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rcx, 0x12"]), R=True)
    .case(run_bytes = intel(["mov rdx, 0x12"]), R=True)
    .case(run_bytes = intel(["add rax, 0x22"]), R=True)
    .case(run_bytes = intel(["add rbx, 0x22"]), R=True)
    .case(run_bytes = intel(["add rcx, 0x22"]), R=True)
    .case(run_bytes = intel(["add rdx, 0x22"]), R=True)
    .case(run_bytes = intel(["mov rax, rcx"]), R=True)
    .case(run_bytes = intel(["mov rbx, rdx"]), R=True)
    .case(run_bytes = intel(["mov rcx, rax"]), R=True)
    .case(run_bytes = intel(["mov rdx, rax"]), R=True)
    .case(run_bytes = intel(["add rax, rcx"]), R=True)
    .case(run_bytes = intel(["add rbx, rdx"]), R=True)
    .case(run_bytes = intel(["add rcx, rax"]), R=True)
    .case(run_bytes = intel(["add rdx, rax"]), R=True)
    .case(run_bytes = intel(["add rdx, r8"]), R=True)
    .case(run_bytes = intel(["add r9, rax"]), R=True)
    .case(run_bytes = intel(["add r9, r8"]), R=True)
    .case(run_bytes = intel(["add r8, 0x42"]), R=True)
    .case(run_bytes = intel(["add r9, 0x53"]), R=True)
    .case(run_bytes = intel(["mov rdx, r8"]), R=True)
    .case(run_bytes = intel(["mov r9, rax"]), R=True)
    .case(run_bytes = intel(["mov r9, r8"]), R=True)
    .case(run_bytes = intel(["mov r8, 0x42"]), R=True)
    .case(run_bytes = intel(["mov r9, 0x53"]), R=True),

    VerifyTest("xor/adc fractions").tags({"multiple", "xor", "adc", "min"})
    .bytes(intel(["xor al, ah", "xor al, 0xa", "xor ax, bx", "xor ax, 0x1",
                  "xor ah, al", "xor ah, 0xb",
                  "adc r8b, al", "adc r8b, 0xe", "adc ax, bx", "adc ax, 0x2",
                  "adc al, 0x0", "adc ah, 0x5",
                  "xor al, r8b", "xor al, 0x4", "xor r8w, bx", "xor r9w, 0x0",
                  "adc r8b, al", "adc r8b, 0x5", "adc r8w, bx", "adc r10w, 0x1"]))
    .DI(S(0x200).RAX(0xffee).RBX(0xddcc).RCX(0xbbaa).RDX(0x9988).R8(0x7766)
                .R9(0x5544).R10(0x3322).aflags(1))
    .case(run_bytes = intel(["xor cx, r8w"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor al, r8b"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor r9b, cl"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor eax, ebx"]), DE = MS().uAF(), R = False)
    .case(run_bytes = intel(["xor r10w, r8w"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor r9b, r8b"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor r9b, 0xf"]), DE = MS().uAF(), R = False)
    .case(run_bytes = intel(["xor ch, 0xf"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor r9w, 0xf"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor cl, 0xf"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["xor cx, 0xf"]), DE = MS().uAF(), R = True)
    .case(run_bytes = intel(["adc cx, r8w"]), R=True)
    .case(run_bytes = intel(["adc al, r8b"]), R=True)
    .case(run_bytes = intel(["adc r9b, cl"]), R=True)
    .case(run_bytes = intel(["adc eax, ebx"]), R=False)
    .case(run_bytes = intel(["adc r10w, r8w"]), R=True)
    .case(run_bytes = intel(["adc r9b, r8b"]), R=True)
    .case(run_bytes = intel(["adc r9b, 0xf"]), R=True)
    .case(run_bytes = intel(["adc ch, 0xf"]), R=True)
    .case(run_bytes = intel(["adc r9w, 0xf"]), R=True)
    .case(run_bytes = intel(["adc cl, 0xf"]), R=True)
    .case(run_bytes = intel(["adc cx, 0xf"]), R=True),
}

aflag_insts = {
    # TODO(lukas): This is painful because of bugs in microx
    VerifyTest("aflags").bytes(intel(["popf", "pushf", "clc", "cld", "stc"]))
    .tags({"aflags", "todo"})
    .all_defined(I = random_state(42))
    .all_defined(I = random_state(43).aflags(0))
    .all_defined(I = random_state(44).aflags(1))
}

circuitous_tests = [test_combinations, aflag_insts]
