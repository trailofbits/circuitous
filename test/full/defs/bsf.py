# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

bsf = {
    VerifyTest("bsf-a") \
    .bytes(tgen.compile(intel, [tgen.bsf()]))
    .tags({"generated", "bsf"}).seed(4123)
    .all_defined(random=True, DE=MS().uAF().uCF().uSF().uOF()),

    VerifyTest("bsf-b") \
    .bytes(["660fbcc0"])
    .tags({"bsf"}).seed(4123)
    .all_defined(random=True, DE=MS().uAF().uCF().uSF().uOF()),



}

circuitous_tests = [bsf]
