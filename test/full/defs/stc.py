# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

stc = {
    VerifyTest("stc-a").bytes(intel(["stc"])).tags({"stc", "min"})
    .DI(random_state(42))
    .case(run_bytes = 0, DI = MS().aflags(1), R=True)
    .case(run_bytes = 0, DI = MS().aflags(0), R=True),
}

circuitous_tests = [stc]
