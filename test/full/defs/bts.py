# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

# NOTE(lukas): Please do not use `ms = MS()`.
def aflag_mut(ms): return ms.uAF().uOF().uZF().uSF().uPF()

bts = {
    VerifyTest("bts-a_gen") \
    .bytes(tgen.compile(intel, [tgen.bts()]))
    .tags({"generated", "bts"}).seed(4123)
    .all_defined(random=True, DE=aflag_mut(MS())),

    VerifyTest("bts-s_z_ext_lifter") \
    .bytes(intel(["bts QWORD PTR [rax + 2 *rsi + 0xff], 0xf2"]))
    .tags({"bts", "min"}).seed(4123)
    .DI(random_state(4123))
    .case(run_bytes = 0, DE=aflag_mut(MS()), R=True),

    # NOTE(lukas): Microx has bug when immediate is grater than size
    Test("bts-b_corner_big_imm") \
    .bytes(intel(["bts WORD PTR [r8d + 2 * eax + 0x50], 0x20"]))
    .tags({"bts", "min"})
    .mode("--verify")
    .DI(S(0x3040).RIP(0x8012).R8(0x2100).RAX(0x380).aflags(0).ts(0)
                 .rwmem(0x2800, "11" * 0x100)
                 .rwmem(0x3800, "00" * 0x100))
    .case(run_bytes = 0,
          DE = aflag_mut(MS()).CF(1).ts(1).RIP(0x8012 + 9).RAX(0x380)
                          .mem_hint(MemHint.read(0x2854, 0x1111, 2))
                          .mem_hint(MemHint.write(0x2854, 0x1111, 2)),
          R=True)
    .case(run_bytes = intel(["bts WORD PTR [r8d + 2 * eax + 0x50], 0x21"]),
          DI = MS().RAX(0xb80),
          DE = aflag_mut(MS()).RAX(0xb80).CF(0).ts(1).RIP(0x8012 + 9)
                          .mem_hint(MemHint.read(0x3854, 0x0000, 2))
                          .mem_hint(MemHint.write(0x3854, 0b10, 2)),
          R=True),

    VerifyTest("bts-c") \
    .bytes(["660fabc0"])
    .tags({"generated", "bts"}).seed(4123)
    .all_defined(random=True, DE=aflag_mut(MS())),
}

circuitous_tests = [bts]
