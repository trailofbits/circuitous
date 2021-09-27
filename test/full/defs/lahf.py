# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

lahf = {
    VerifyTest("lahf-a") \
    .bytes(intel(["lahf"]))
    .tags({"lahf", "min"})
    .case(run_bytes = 0, I = random_state(42).ts(24), R=True)
    .case(run_bytes = 0, I = random_state(4212), R=True)
    .case(run_bytes = 0, I = random_state(4213).ts(4551), R=True),


    VerifyTest("lahf-b").bytes(intel(["lahf"])).tags({"lahf", "min"})
    .DI(random_state(42))
    .case(run_bytes = 0, DI = MS().aflags(1), R=True)
    .case(run_bytes = 0, DI = MS().aflags(0), R=True),
}

circuitous_tests = [lahf]
