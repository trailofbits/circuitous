# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

movsx = {
    VerifyTest("movsx-a_gen") \
    .bytes(tgen.compile(intel, [tgen.movsx()]))
    .tags({"movsx", "generated"}).seed(4123)
    .all_defined(random = True),

    VerifyTest("movsx-b") \
    .bytes(intel(["movsx rax, word ptr [rax + 2 * rcx + 0x2122]"]))
    .tags({"movsx", "generated", "min"}).seed(4123)
    .DI(S(0x4212).RCX(0x400).RIP(0xdcae6e9fb39cfd4d))
    .case(run_bytes = 0, R=True),
}

circuitous_tests = [movsx]
