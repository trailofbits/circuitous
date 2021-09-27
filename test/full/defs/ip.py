# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, random_state

test_ip = {
    Test("ip.update").bytes("ba12000000").tags({"min", "ip"})
    .mode("--verify")
    .case(I = S(0x100).RDX(0x0).RIP(0x1000), E = S(0x100).RDX(0x12).RIP(0x1005).ts(1), R = True)
}

test_rip_split = {
    VerifyTest("test_rip_split").tags({"min", "selftest"})
    .bytes(intel(["mov [rax + 2 * r8 + 0xff], rdx"]))
    .DI(random_state(431).RIP(0x9ffe))
    .all_defined()
}
circuitous_tests = [test_ip, test_rip_split]
