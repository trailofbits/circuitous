# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

ret = {
    VerifyTest("ret") \
    .bytes(intel(["ret"]))
    .tags({"ret"})
    .case(run_bytes = 0, I = random_state(42), R=True)
    .case(run_bytes = 0, I = random_state(4212), R=True)
    .case(run_bytes = 0, I = random_state(4213), R=True),
}

circuitous_tests = [ret]
