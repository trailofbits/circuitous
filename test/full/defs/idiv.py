# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest

test_idiv = {
    VerifyTest("idiv-a").bytes(intel(["idiv rsi"])).tags({"min", "idiv"})
    .case(I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x22).RIP(0x1000).aflags(0),
          DE = MS().aflags(0),
          R = True)
    .case(I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x1).RIP(0x1000).aflags(0),
          DE = MS().aflags(0),
          R = True)
    .case(I = S(0x1).RDX(0x0).RAX(0x66).RSI(0x66).RIP(0x1000).aflags(0),
          DE = MS().aflags(0),
          R = True),

    VerifyTest("idiv-b").tags({"min", "idiv"})
    .bytes(intel(["idiv rdi"]))
    .DI(S(0x100).RIP(0x1000).aflags(0))
    .case(DI = MS().RIP(0x1000).RAX(0x10).RDX(0x0).RDI(0x1),
          DE = MS().aflags(0), R = True)
    .case(DI = MS().RIP(0x1000).RAX(0x30).RDX(0x0).RDI(0x15),
          DE = MS().aflags(0), R = True)
    .case(DI = MS().RIP(0x1000).RAX(0xffffff).RDX(0x1).RDI(0xfffff),
          DE = MS().aflags(0), R = True)
}


circuitous_tests = [test_idiv]
