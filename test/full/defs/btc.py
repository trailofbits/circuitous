# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

# NOTE(lukas): Please do not use `ms = MS()`.
def aflag_mut(ms): return ms.uAF().uOF().uZF().uSF().uPF()

btc = {
    VerifyTest("btc-a_gen") \
    .bytes(tgen.compile(intel, [tgen.btc()]))
    .tags({"generated", "btc", "todo"}).seed(4123)
    .all_defined(random=True, DE=aflag_mut(MS())),

    VerifyTest("btc-b").tags({"btc", "min"})
    .bytes(["67660fbb848012121212"]).seed(4123)
    .all_defined(random=True, DE=aflag_mut(MS())),

    VerifyTest("btc-c").tags({"btc", "min", "todo"})
    .bytes(["490fbabc801212121209"]).seed(4123)
    .all_defined(random=True, DE=aflag_mut(MS())),
}

circuitous_tests = [btc]
