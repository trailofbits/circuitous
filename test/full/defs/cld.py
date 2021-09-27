# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

cld = {
    Test("cld").bytes(intel(["cld"]))
    .tags({"cld", "min"})
    .mode("--verify")
    .DI(random_state(42).RIP(0x98120))
    .case(run_bytes = 0, DI = MS().aflags(1),
          DE = MS().aflags(1).DF(0x0).RIP(0x98121).ts(1), R=True)
    .case(run_bytes = 0, DI = MS().aflags(0),
          DE = MS().aflags(0).DF(0x0).RIP(0x98121).ts(1), R=True),
}

circuitous_tests = [cld]
