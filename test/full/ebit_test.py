# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG

_verify_test = {
  VerifyTest("selftest").tags({"selftest"})
  .bytes(intel(["mov rdx, 0x12100"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, DE = MS().RDX(0x12), R = False)
}

timestamp_inc = {
  VerifyTest("timestamp basic").tags({"ts", "min"})
  .bytes(intel(["mov rdx, 0xba120000"])).DI(S(0x350))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DE = MS().ts(0x52), R = False)
  .case(run_bytes = 0, DE = MS().ts(0), R = False)
  .case(run_bytes = 0, DE = MS().ts(2), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x52), R = True)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0x52), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0x55), R = False),
}

saturation_property = {
  VerifyTest("saturation_property").tags({"mov", "ebit"})
  .bytes(intel(["mov rdx, 0xba120000"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().ebit(True), R = False),

  VerifyTest("idiv by 0").tags({"idiv", "ebit", "todo"})
  .bytes(intel(["idiv rdi"])).DI(S(0x350).RDI(0x0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().ebit(False), R = False),

  VerifyTest("Preserve on error idiv").tags({"idiv", "ebit", "todo"})
  .bytes(intel(["idiv rdi"])).DI(S(0x300).RDI(0x0).ebit(True))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().RDI(0x3), DE = MS().RAX(0x100), R = False),
}

pop = {
  ModelTest("pop basic").tags({"pop", "min", "mem"}).bytes(intel(["pop rax"]))
  .DI(S(0x20000).RAX(0x56).RIP(0x1000).rmem(0x20000, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["pop rbx"]), R = True)
  .case(run_bytes = intel(["pop rdx"]), R = True)
  .case(run_bytes = intel(["pop rbp"]), R = True)
  .case(run_bytes = intel(["pop r9"]), R = False)
}

push = {
  ModelTest("push basic").tags({"push", "min", "mem"}).bytes(intel(["push rax"]))
  .DI(S(0x40040).RSP(0x20040).RIP(0x60000).RAX(0x56)
      .rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["push rbx"]), R = True)
  .case(run_bytes = intel(["push rdx"]), R = True)
  .case(run_bytes = intel(["push rbp"]), R = True)
  .case(run_bytes = intel(["push r9"]), R = False)
}

push_cx = {
  ModelTest("push cx").tags({"push", "mem"}).bytes(intel(["push cx"]))
  .DI(S(0x20040).RAX(0x56).RIP(0x1000).rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["push bx"]), R = True)
  .case(run_bytes = intel(["push dx"]), R = True)
  .case(run_bytes = intel(["push bp"]), R = True)
  .case(run_bytes = intel(["push rcx"]), R = False)
}

push_deref = {
  ModelTest("push_deref").tags({"push", "min", "mem"})
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

mov_mem = {
  ModelTest("mov reg, dreg").tags({"mov", "min", "mem"})
  .bytes(intel(["mov rax, [rbx]"]))
  .DI(S(0x20040).wrm_RAX(0x50056, 65).wrm_RBX(0x4008, 0x100).wrm_RAX(0x9800, 0x100))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = intel(["mov [rax], rbx"]), R=False)
  .case(run_bytes = intel(["mov rdx, [rax]"]), R=True),

  ModelTest("mov dreg, reg").tags({"mov", "min", "mem"})
  .bytes(intel(["mov [rax], rbx"]))
  .DI(S(0x20040).wrm_RAX(0x50056, 65).wrm_RBX(0x4008, 0x100).wrm_RDX(0x9800, 0x100))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = intel(["mov rax, [rbx]"]), R=False)
  .case(run_bytes = intel(["mov [rdx], rax"]), R=True),

  ModelTest("rex prefix").tags({"mov", "mem", "min"})
  .bytes(intel(["mov [r9], rax", "mov rax, [r9]"]))
  .DI(S(0x30048).wrm_RAX(0x30054, 0x100).wrm_RDX(0x40032, 0x100)
                .wrm_R9(0x60421, 0x100).wrm_R8(0x70212, 0x100))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = 1, R=True)
  .scases(intel, ["mov [r8], r9", "mov [rdx], r8", "mov [r9], rax"], R=True),

  ModelTest("displacement").tags({"mov", "mem", "min"})
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
  .scases(intel, ["mov [r9 + 2*rsi -0x5], rcx", "mov [rsi + 2 * r10 + 0x5], r8"], R=True)
}

xor_mem = {
  ModelTest("xor mem reg").tags({"xor", "mem", "min"})
  .bytes(intel(["xor [rax], rbx"]))
  .DI(S(0x2005).wrm_RAX(0x324d).wrm_RBX(0x4521).wrm_RSI(0x51b1).wrm_RDX(0x6ac1)
               .wrm_R8(0x7128))
  .case(run_bytes = 0, R=True)
  .scases(intel, ["xor [rbx], rdx", "xor [rsi], rsi", "xor [rdx], rax"], R=True)
  .case(run_bytes = intel(["xor [r8], rax"]), R=True)
}



circuitous_tests = [ saturation_property, _verify_test, timestamp_inc,
  push, pop, push_cx, push_deref, mov_mem, xor_mem ]