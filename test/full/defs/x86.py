# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint, MS32, S32

from defs.parse_dbg import load_dbg

test_x86 = {
    Test("full-a").tags({"tiny86", "x86_wip"})
    .mode("--verify")
    .arch("x86")
    .bytes(load_dbg('x86.dbg'))
    .DI(S32(0x5000).segments(0).EAX(0x60).EBX(0xffffffff).EIP(0x6000))
    #.case(run_bytes=["f7eb"],
    #      DE = MS32().EIP(0x6002).EAX(0xffffffa0).EDX(0xffffffff).ts(1),
    #      R=True)
    #.case(run_bytes=["76ff"], DE = MS32().EIP(0x6002).ts(1), R = True)
    .case(run_bytes=["76ff"],
          DI = MS32().aflags(1),
          DE = MS32().EIP(0x6001).ts(1).aflags(1), R = True)
}

circuitous_tests = [test_x86]
