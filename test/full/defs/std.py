# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

std = {
    # NOTE(lukas): There is some microx related problem which causes sigsev
    Test("std-a").bytes(intel(["std"])).tags({"std", "min", "todo"})
    .mode("--verify")
    .DI(random_state(42))
    .case(run_bytes = 0, DE = MS().DF(1), DI = MS().aflags(1), R=True)
    .case(run_bytes = 0, DE = MS().DF(1), DI = MS().aflags(0), R=True),
}

circuitous_tests = [std]
