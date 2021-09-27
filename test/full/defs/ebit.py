# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

saturation_property = {
    VerifyTest("ebit-a_saturation_property").tags({"mov", "ebit", "min"})
    .bytes(intel(["mov rdx, 0xba120000"])).DI(S(0x350).ebit(False))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 0, DI = MS().ebit(True), R = False),

    VerifyTest("ebit-b_idiv_by_0").tags({"idiv", "ebit", "min"})
    .bytes(intel(["idiv rdi"])).DI(S(0x350).RDI(0x0))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 0, DE = MS().ebit(False), R = False),

    VerifyTest("ebit-c_preserve_on_error_idiv").tags({"idiv", "ebit", "min"})
    .bytes(intel(["idiv rdi"])).DI(S(0x300).RDI(0x0).ebit(True))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 0, DI = MS().RDI(0x3), DE = MS().RAX(0x100), R = False)
    .case(run_bytes = 0, DI = MS().RDI(0x0), DE = MS().ebit(False), R = False),
}

circuitous_tests = [saturation_property]
