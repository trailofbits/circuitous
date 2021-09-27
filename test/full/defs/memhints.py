# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

# To verify that cirucit fails is memory hints are incorrect
memhint_fails = {
    Test("mov_incorrent_memhint") \
    .bytes(intel(["mov QWORD PTR [rax], 0x20"]))
    .tags({"mov", "min", "reject"})
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

circuitous_tests = [memhint_fails]
