# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

bsr = {
    VerifyTest("bsr-a") \
    .bytes(tgen.compile(intel, [tgen.bsr()]))
    .tags({"bsr", "generated"})
    .DI(S(0x1).RAX(0x0).aflags(0))
    .all_defined()
}

circuitous_tests = [bsr]
