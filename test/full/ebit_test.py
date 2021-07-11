# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

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

  VerifyTest("idiv by 0").tags({"idiv", "ebit"})
  .bytes(intel(["idiv rdi"])).DI(S(0x350).RDI(0x0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DE = MS().ebit(False), R = False),

  VerifyTest("Preserve on error idiv").tags({"idiv", "ebit"})
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
  .case(run_bytes = intel(["pop r9"]), R = False),

  ModelTest("pop [rsp]").tags({"pop", "min", "mem", "todo"})
  .bytes(intel(["pop QWORD PTR [rsp]"]))
  .DI(S(0x20000).RSP(0x3000).RIP(0x1000).RAX(0x5000)
                 .rwmem(0x3000, "0040" + 6 * "00")
                 .rwmem(0x4000, "0030" + 6 * "00")
                 .rwmem(0x5000, "00f0" + 6 * "00"))
  .case(run_bytes = 0, R = True)
  # `pop [rsp]` uses different semantic function than other `pop [gpr]``
  .case(run_bytes = intel(["pop QWORD PTR [rax]"]), R=False),

  ModelTest("pop [rax]").tags({"pop", "min", "mem"})
  .bytes(intel(["pop QWORD PTR [rsp]"]))
  .DI(S(0x20000).RSP(0x3000).RIP(0x1000).RAX(0x5000).RBX(0x6211)
                 .rwmem(0x3000, "0040" + 6 * "00")
                 .rwmem(0x4000, "0030" + 6 * "00")
                 .rwmem(0x5000, "00f0" + 6 * "00")
                 .rwmem(0x6210, "00" * 16))
  .case(run_bytes = 0, R = True)
  # `pop [rsp]` uses different semantic function than other `pop [gpr]``
  .case(run_bytes = intel(["pop QWORD PTR [rbx]"]), R=False),

  ModelTest("pop [rsp - 0x20]").tags({"pop", "min", "mem", "todo"})
  .bytes(intel(["pop QWORD PTR [rsp - 0x20]"]))
  .DI(S(0x20000).RSP(0x3000).RIP(0x1000)
                 .rwmem(0x2ff8, "00" * 8)
                 .rwmem(0x3000, "0040" + 30 * "00")
                 .rwmem(0x4000, "0030" + 6 * "00"))
  .case(run_bytes = 0, R = True)
}


pop_corners = {
  Test("pop rsp").tags({"min", "pop", "mem", "todo"})
  .bytes(intel(["pop rsp"]))
  .DI(S(0x23000).RSP(0x3000).RIP(0x1000)
                .rwmem(0x3000, "0040" + 6 * "00")
                .rwmem(0x4000, "0030" + 6 * "00"))
  .case(run_bytes = 0,
        DE = MS().RSP(0x4008).RIP(0x1001).ts(1).mem_hint(MemHint.read(0x3000, 0x4000, 8)),
        R=True),

  ModelTest("pop rsp microx").tags({"min", "pop", "mem", "todo"})
  .bytes(intel(["pop rsp"]))
  .DI(S(0x23000).RSP(0x3000).RIP(0x1000)
                .rwmem(0x3000, "0040" + 6 * "00")
                .rwmem(0x4000, "0030" + 6 * "00"))
  .case(run_bytes = 0, R=True)
}

push_corners = {
  ModelTest("push rsp").tags({"min", "push", "mem"})
  .bytes(intel(["push rsp"]))
  .DI(S(0x23000).RSP(0x3008).RIP(0x1000)
                .rwmem(0x3000, "0040" + 6 * "00")
                .rwmem(0x4000, "0030" + 6 * "00"))
  .case(run_bytes = 0, R = True)
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
  .scases(intel, ["mov [r9 + 2*rsi -0x5], rcx", "mov [rsi + 2 * r10 + 0x5], r8"], R=True),

  ModelTest("mov mem imm").tags({"mov", "mem", "min"})
  .bytes(intel(["mov QWORD PTR [0x6531], 0x412"]))
  .DI(S(0x32100).mig(MIG().rw(0x6000, 0x1000).rw(0x10000, 0x1000).rw(0x20000, 0x1000)))
  .case(run_bytes = 0, R = True)
  .scases(intel, ["mov QWORD PTR [0x6531], 0x412a12", "mov QWORD PTR [0x101ab], 0x4ffbe",
                  "mov QWORD PTR [0x20aaa], 0x0"], R=True),

  ModelTest("mov mem reg").tags({"mov", "mem", "min"})
  .bytes(intel(["mov QWORD PTR [0x6531], r9"]))
  .DI(S(0x32100).mig(MIG().rw(0x6000, 0x1000).rw(0x10000, 0x1000).rw(0x20000, 0x1000))
      .RAX(0xffaa).R10(0x431).RCX(0xbeac4))
  .case(run_bytes = 0, R = True)
  .scases(intel, ["mov QWORD PTR [0x6531], r10",
                  "mov QWORD PTR [0x101ab], rcx",
                  "mov QWORD PTR [0x20aaa], rax"], R=True)
  .case(run_bytes = intel(["mov QWORD PTR [0x101ab], 0x0"]), R=False),

  ModelTest("mov mem reg32").tags({"mov", "mem", "32b"})
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


  ModelTest("mov rip, imm").tags({"mov", "mem"})
  .bytes(intel(["mov [rip + 0x1400], rax"]))
  .DI(S(0x3400).RIP(0x7000).mig(MIG().rw(0x8000, 0x1000)))
  .case(run_bytes = 0, R = True)
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

cfg = {
  ModelTest("jmp reg").tags({"jmp", "cfg", "min"})
  .bytes(intel(["jmp rax"]))
  .DI(S(0x4212).RBX(0x123).RCX(0x4212))
  .scases(intel, ["jmp rax", "jmp rbx", "jmp rcx", "jmp rsp"], R=True)
  .scases(intel, ["jmp r8"], R=False),

  ModelTest("jmp rxreg").tags({"jmp", "cfg"})
  .bytes(intel(["jmp r10"]))
  .DI(S(0x4212).RBX(0x123).RCX(0x4212).R8(0xff4312).R10(0xfff421))
  # All below should in reality fail, since they will be generated as shorter forms
  .scases(intel, ["jmp rax", "jmp rbx", "jmp rcx", "jmp rsp"], R=False)
  .scases(intel, ["jmp r8", "jmp r12"], R=True),

  ModelTest("jmp [rip + imm8]").tags({"jmp", "cfg", "min"})
  .bytes(intel(["jmp [rip + 0x43]"]))
  .DI(S(0x41212).RAX(0x2000).mig(MIG().rw(0x6000, 0x1000)))
  .scases(raw, ["ff2500500000"], R=True)
  .scases(intel, ["jmp rax"], R=False),


  ModelTest("call 0xff4410").tags({"call", "cfg", "min"})
  .bytes("e800400000")
  .DI(S(0x4121).RSP(0x8116).mig(MIG().rw(0x8000, 0x1000)))
  .case(run_bytes = 0, R=True),

  ModelTest("call rax").tags({"call", "cfg", "min"})
  .bytes(intel(["call rax"]))
  .DI(S(0x4121)
      .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
      .RAX(0xfff77).RBP(0x4121a)
      .mig(MIG().rw(0x8000, 0x1000)))
  .scases(intel, ["call r8", "call r10", "call r15"], R=False)
  .scases(intel, ["call rax", "call rbp"], R=True)
  .case(run_bytes = intel(["call rsp"]), R=True),

  ModelTest("call r10").tags({"call", "cfg", "min"})
  .bytes(intel(["call r10"]))
  .DI(S(0x4121)
      .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
      .RAX(0xfff77).RBP(0x4121a)
      .mig(MIG().rw(0x8000, 0x1000)))
  .scases(intel, ["call r8", "call r10", "call r15"], R=True)
  .scases(intel, ["call rax", "call rsp"], R=False),

  ModelTest("call dreg").tags({"call", "cfg", "min"})
  .bytes(intel(["call [rsp + 8]"]))
  .DI(S(0x4121)
      .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
      .RAX(0xfff77).RBP(0x4121a)
      .rwmem(0x8108, "00405060ffae43112012405055657a98aefc31321291a3fcfecf718a981721"))
  .case(run_bytes = 0, R = True)
}

evil = {
  ModelTest("call dreg").tags({"call", "cfg", "min"})
  .bytes(intel(["call [rsp + 8]"]))
  .DI(S(0x4121)
      .RSP(0x8116).R8(0x0).R10(0x3411e).R15(0xfff12)
      .RAX(0xfff77).RBP(0x4121a)
      .rwmem(0x8108, "00405060ffae43112012405055657a98aefc31321291a3fcfecf718a981721"))
  .case(run_bytes = 0, R = True)
}

big_mix = {
  ModelTest("adc, add, sub, sbb, shl, shr, xor, or, and, div, idiv").tags({"min", "big"})
  .bytes(intel(["adc rax, 0x12", "add rcx, rax", "add rax, 0x45",
                "sub rcx, 0x15", "sub rax, rcx", "sbb rax, 0x12",
                "shr rax, 0x2", "shr rcx, 0x2", "shl rax, 0x2", "shl rcx, 0x2",
                "xor rax, rax", "xor rax, rcx",
                "or rax, rax","or rax, rcx",
                "and rax, 0xfffff", "and rax, rcx",
                "div rax", "idiv rcx", "idiv rax"]))
  .DI(S(0x1).RAX(0x12).RCX(0x45).aflags(0))
  .case(run_bytes = 0, R=True)
  .case(run_bytes = 1, R=True)
  .case(run_bytes = 2, R=True)
  .case(run_bytes = 3, R=True)
  .case(run_bytes = 4, R=True)
  .case(run_bytes = 5, R=True)
}


circuitous_tests = [ saturation_property, _verify_test, timestamp_inc,
  push, pop, push_cx, push_deref, mov_mem, xor_mem, pop_corners, push_corners,
  big_mix, cfg, evil ]