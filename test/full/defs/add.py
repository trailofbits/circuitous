# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

test_add = {
    VerifyTest("add-a_rax_imm") \
    .bytes(intel(["add rax, 8"])).tags({"add", "min"})
    .case("rax+=8",
          I = S(0x100).RAX(0x5).aflags(0),
          R = True)
    .case("rax+=16",
          I = S(0x100).RAX(0x5).aflags(0),
          run_bytes = intel(["add rax, 16"])[0],
          R = True),

    Test("add-b_rax_imm") \
    .bytes(intel(["add rax, 0x10"])).tags({"add", "min"})
    .mode("--verify")
    .case("rax+=0x20",
          I = S(0x0).RAX(0x0).aflags(0).RIP(0x1000),
          E = S(0x0).RAX(0x20).RIP(0x1004).aflags(0).ts(1),
          run_bytes = intel(["add rax, 0x20"])[0],
          R = True)
    .case("add-c_rax+=0x20",
          I = S(0x0).RAX(0x0).aflags(0).RIP(0x1000),
          E = S(0x0).RAX(0x20).RBX(0x20).RIP(0x1004).aflags(0).ts(1),
          run_bytes = intel(["add rax, 0x20"])[0],
          R = False)
    .case("add-d_rax+=0x20",
          I = S(0x0).RAX(0x0).aflags(0).RIP(0x1000),
          E = S(0x0).RAX(0x21).RIP(0x1004).aflags(0).ts(1),
          run_bytes = intel(["add rax, 0x20"])[0],
          R = False),

    Test("add-e_rax_imm") \
    .bytes(intel(["add rax, 0x10", "mov rcx, 0x10"])).tags({"add"})
    .mode("--verify")
    .case("rax+=0x20",
          I = S(0x0).RAX(0x0).RCX(0x0).RIP(0x1000).aflags(0),
          E = S(0x0).RAX(0x0).RCX(0x20).RIP(0x1004).ts(1),
          run_bytes = intel(["add rcx, 0x20"])[0],
          R = True),

    VerifyTest("add-f_reg_reg").tags({"add"})
    .bytes(intel(["add rcx, rax"]))
    .DI(S(0x100).RAX(0x15).RBX(0x55).RCX(0x45).RDX(0xff).RSP(0x2000))
    .case(run_bytes = intel(["add rax, rcx"]), R=True)
    .case(run_bytes = intel(["add rbx, rdx"]), R=True)
    .case(run_bytes = intel(["add rcx, rax"]), R=True)
    .case(run_bytes = intel(["add rdx, rax"]), R=True),

    VerifyTest("add-g_rax_rax").bytes(intel(["add rax, rax"])).tags({"add", "min"})
    .DI(S(0x100).aflags(0).R13(0x2).RBX(0x3))
    .case(run_bytes = 0, R=True)
    .case(run_bytes = intel(["add rax, rbx"]), R=True)
    .case(run_bytes = intel(["add rcx, r13"]), R=True)
}

add_gen = {
  VerifyTest("add-h_gen") \
  .bytes(intel(tgen.IDef("add").iadd().get()))
  .tags({"add", "generated", "gen_add"})
  .DI(S(0x41124))
  .all_defined(),

  VerifyTest("add-i_special_gen") \
  .bytes(["674401848012121212"])
  .tags({"add", "min"})
  .DI(S(0x41124))
  .all_defined(),
}


circuitous_tests = [test_add, add_gen]
