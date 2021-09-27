# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

# NOTE(lukas): Please do not use `ms = MS()`.
def aflag_mut(ms): return ms.uAF().uOF().uZF().uSF().uPF()

btr = {
    VerifyTest("btr-a_gen") \
    .bytes(tgen.compile(intel, [tgen.btr()]))
    .tags({"generated", "btr"}).seed(4123)
    .all_defined(random=True, DE = aflag_mut(MS())),

    VerifyTest("btr-a_gen") \
    .bytes(["660fb3c0"])
    .tags({"min", "btr"}).seed(4123)
    .all_defined(random=True, DE = aflag_mut(MS())),


}

circuitous_tests = [btr]
