# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

def make_string_test_spec(base, suffix):
    t = VerifyTest(base + suffix + "-a").tags({"string", base, base + suffix})
    t.bytes(intel([base + suffix]))
    t.DI(S(0x1000).RAX(0x4000).RDI(0x5000))
    t.case(run_bytes = 0, R=True)
    t.case(run_bytes = 0, DI=MS().DF(0), R=True)
    #t.case(run_bytes = 0, DI=MS().DF(1),R=True)
    #t.case(run_bytes = 0, DI=MS().aflags(1), R=True)
    t.case(run_bytes = 0, DI=MS().aflags(0), R=True)
    #t.case(run_bytes = 0, DI=MS().DF(1).RAX(0x5500),R=True)
    t.case(run_bytes = 0, DI=MS().DF(0).RAX(0x5500),R=True)
    return t

def make_string_test(base):
    out = set()
    for suffix in ['b', 'w', 'd']:
        out.add(make_string_test_spec(base, suffix))
    return out


test_scas = make_string_test("scas")
test_movs = make_string_test("movs")
test_lods = make_string_test("lods")
test_stos = make_string_test("stos")

test_stos_dbg = {
     VerifyTest("stosb-b").tags({"string", "stos", "wip"})
     .bytes(intel(["stosb"]))
     .DI(S(0x1000).RAX(0x4000).RDI(0x5000))
     #.case(run_bytes = 0, DI=MS().DF(1),R=True)
     #.case(run_bytes = 0, DI=MS().aflags(1), R=True)
     .case(run_bytes = 0, DI=MS().aflags(0), R=True)
     #.case(run_bytes = 0, DI=MS().DF(1).RAX(0x5500),R=True)
     .case(run_bytes = 0, DI=MS().DF(0).RAX(0x5500),R=True)
}

circuitous_tests = [test_scas, test_movs, test_lods, test_stos, test_stos_dbg]
