# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

bt = {
    # TODO(lukas): Add definitions.
    #VerifyTest("bt=a_gen") \
    #.bytes(tgen.compile(intel, [tgen.bsr()]))
    #.tags({"bt", "generated"})
    #.DI(random_state(42))
    #.all_defined()
}

circuitous_tests = [bt]
