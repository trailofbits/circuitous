# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

popf = {
    # NOTE(lukas): Bug in microx crashes the python (probably leaving DF set/unset)
    #              therefore these must be defined manually.
    Test("popf-a").bytes(intel(["popf"])).tags({"popf", "min"}).seed(4212)
    .mode("--verify")
    .DI(S(0x4000).RSP(0x8010).RIP(0x5000).aflags(0).rwmem(0x8000, 32 * "ff"))
    .case(run_bytes = 0, R = True,
          DE = MS().aflags(1).ts(1).RIP(0x5001).RSP(0x8018)
                   .mem_hint(MemHint.read(0x8010, 0xffffffffffffffff, 8))),

    Test("popf-b").bytes(intel(["popf"])).tags({"popf", "min"}).seed(4212)
    .mode("--verify")
    .DI(S(0x4000).RSP(0x8010).RIP(0x5000).aflags(1).rwmem(0x8000, 32 * "00"))
    .case(run_bytes = 0, R = True, DE = MS().aflags(0).ts(1).RIP(0x5001).RSP(0x8018)
                                            .mem_hint(MemHint.read(0x8010, 0, 8)))

    #VerifyTest("popf-c").bytes(intel(["popf"])).tags({"popf", "min", "todo"}).seed(4212)
    #.random(5, run_bytes = 0, R = True),
}

circuitous_tests = [popf]
