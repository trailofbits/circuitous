# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

neg = {
    VerifyTest("neg-a") \
    .bytes(intel(["neg rax"]))
    .tags({"neg", "min"})
    .case(run_bytes = 0, I = random_state(42).ts(24), R=True)
    .case(run_bytes = intel(["neg rbx"]), I = random_state(4212), R=True)
    .case(run_bytes = intel(["neg rsp"]), I = random_state(4213).ts(4551), R=True),
}

circuitous_tests = [neg]
