# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

jmp = {
  VerifyTest("jmp-a_reg").tags({"jmp", "cfg", "min"})
  .bytes(intel(["jmp rax"]))
  .DI(S(0x4212).RBX(0x123).RCX(0x4212))
  .scases(intel, ["jmp rax", "jmp rbx", "jmp rcx", "jmp rsp"], R=True)
  .scases(intel, ["jmp r8"], R=False),

  VerifyTest("jmp-b_rxreg").tags({"jmp", "cfg"})
  .bytes(intel(["jmp r10"]))
  .DI(S(0x4212).RBX(0x123).RCX(0x4212).R8(0xff4312).R10(0xfff421))
  # All below should in reality fail, since they will be generated as shorter forms
  .scases(intel, ["jmp rax", "jmp rbx", "jmp rcx", "jmp rsp"], R=False)
  .scases(intel, ["jmp r8", "jmp r12"], R=True),

  VerifyTest("jmp-c_rip_plus_imm8").tags({"jmp", "cfg", "min"})
  .bytes(intel(["jmp [rip + 0x43]"]))
  .DI(S(0x41212).RAX(0x2000).mig(MIG().rw(0x6000, 0x1000)))
  .scases(raw, ["ff2500500000"], R=True)
  .scases(intel, ["jmp rax"], R=False),

  VerifyTest("jmp-d").tags({"jmp", "cfg", "min"})
  .bytes(["ff25ff340000"])
  .DI(S(0x41212).RAX(0x2000).RIP(0x5000).mig(MIG().rw(0x6000, 0x1000)))
  .scases(raw, ["ff2500500000"], R=True)
  .scases(intel, ["jmp rax"], R=False),

  Test("jmp-e").tags({"jmp", "cfg", "min"})
  .mode("--verify")
  .bytes(["ff25ff340000"])
  .DI(S(0x41212).RAX(0x2000).RIP(0x5000)
                .rwmem(0xa000, 6 * "00" + "1fe0" + 32 * "00"))
  .scases(raw, ["ff2500500000"],
          DE = MS().RIP(0xe01f).ts(1)
                   .mem_hint(MemHint.read(0xa006, 0xe01f, 8)), R=True)
  .scases(raw, ["ff2500500000"],
          DE = MS().RIP(0x1fe0).ts(1)
                   .mem_hint(MemHint.read(0xa006, 0xe01f, 8)), R=False)
  .scases(intel, ["jmp rax"], R=False),
}

circuitous_tests = [jmp]
