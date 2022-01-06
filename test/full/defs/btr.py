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

    VerifyTest("btr-b_spec") \
    .bytes(["660fb3c0"])
    .tags({"min", "btr"}).seed(4123)
    .all_defined(random=True, DE = aflag_mut(MS())),

    VerifyTest("btr-c_spec") \
    .bytes(["490fbab4810000000009"])
    .tags({"todo", "btr"})
    .DI(S(0x4000).RAX(0x100).R8(0x20).rwmem(0x430, "00" * 0x100))
    .case(run_bytes=0, DE = aflag_mut(MS()), R=True),



}

circuitous_tests = [btr]
