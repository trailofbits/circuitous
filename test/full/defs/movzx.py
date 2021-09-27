# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

movzx = {
    VerifyTest("movzx-a_gen") \
    .bytes(tgen.compile(intel, [tgen.movsx()]))
    .tags({"movzx", "generated"}).seed(4123)
    .DI(S(0x1).RAX(0x0).aflags(0))
    .all_defined(random=True),

    VerifyTest("movzx-b") \
    .bytes(intel(["movzx rax, word ptr [rax + 2 * rcx + 0x2122]"]))
    .tags({"movzx", "generated"}).seed(4123)
    .DI(S(0x1).RAX(0x0).aflags(0))
    .all_defined(random=True),
}

circuitous_tests = [movzx]
