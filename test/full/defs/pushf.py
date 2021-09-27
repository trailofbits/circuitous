# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

pushf = {
    Test("pushf-a").bytes(intel(["pushf"])).tags({"pushf", "min"}).seed(4212)
    .mode("--verify")
    .DI(S(0x2000).RSP(0x4008).RIP(0x2000).aflags(0).rwmem(0x4000, 32 * "00"))
    .case(run_bytes = 0,
          E = S(0x2000).RSP(0x4000).RIP(0x2001).aflags(0).ts(1)
                       .mem_hint(MemHint.write(0x4000, 0, 8)),
          R=True),

    Test("pushf-a").bytes(intel(["pushf"])).tags({"pushf", "min"}).seed(4212)
    .mode("--verify")
    .DI(S(0x2000).RSP(0x4008).RIP(0x2000).aflags(1).rwmem(0x4000, 32 * "00"))
    .case(run_bytes = 0,
          E = S(0x2000).RSP(0x4000).RIP(0x2001).aflags(1).ts(1)
                       .mem_hint(MemHint.write(0x4000, 0xcd5, 8)),
          R=True),



    # TODO(lukas): Bug in microx?
    VerifyTest("pushf-c").bytes(intel(["pushf"])).tags({"pushf", "min", "todo"}).seed(4212)
    .case(run_bytes = 0, I = S(0x2000).aflags(1), R=True)
    .random(5, run_bytes = 0, R = True),
}

circuitous_tests = [pushf]
