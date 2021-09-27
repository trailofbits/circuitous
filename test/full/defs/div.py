# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_div = {
    VerifyTest("div").tags({"min", "div"}).bytes(intel(["div rdi"]))
    .DI(S(0x100))
    .case(DI = MS().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
          DE = MS().aflags(0), R = True)
    .case(DI = MS().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
          DE = MS().aflags(0), R = True)
    .case(DI = MS().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
          DE = MS().aflags(0), R = True)
}

circuitous_tests = [test_div]
