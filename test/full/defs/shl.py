# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_shl = {
    VerifyTest("shl-a").tags({"min", "shl"})
    .bytes(intel(["shl rdx, 0x20"]))
    .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0), DE = MS().uOF().uAF(), R = True)
    .case(I = S(0x100).RDX(0xff).CF(0x0), DE = MS().uOF().uAF(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
          run_bytes = intel(["shl rdx, 0x4"]),
          DE = MS().uOF().uAF(),
          R = True)
    # NOTE(lukas): Different encoding
    .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
          run_bytes = intel(["shl rcx, 0x1"]),
          DE = MS().uAF(),
          R = False),

    VerifyTest("shl-b").tags({"min", "shl"})
    .bytes(intel(["shl rdx, 0x20", "shl rax, 0x1"]))
    .case(run_bytes = 1,
          I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
          # TODO(lukas): We ideally want to check that setting uOF() would reject
          DE = MS().uAF(),
          R = True)
    .case(run_bytes = 0, I = S(0x100).RDX(0xff).CF(0x0), DE = MS().uOF().uAF(), R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
          run_bytes = intel(["shl rdx, 0x4"]),
          DE = MS().uOF().uAF(),
          R = True)
    .case(I = S(0x100).RDX(0x7fffffffffffffff).CF(0x0),
          run_bytes = intel(["shl rcx, 0x1"]),
          DE = MS().uAF(),
          R = True),
}

circuitous_tests = [test_shl]
