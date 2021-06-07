# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, if_has

_verify_test = {
  VerifyTest("selftest").tags({"selftest"})
  .bytes(intel(["mov rdx, 0x12100"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, DE = MS().RDX(0x12), R = False)
}

saturation_property = {
  VerifyTest("saturation_property").tags({"mov", "ebit"})
  .bytes(intel(["mov rdx, 0xba120000"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().ebit(True), R = False),

  VerifyTest("idiv by 0").tags({"idiv", "ebit", "todo"})
  .bytes(intel(["idiv rdi"])).DI(S(0x350).RDI(0x0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().ebit(False), R = False),

  VerifyTest("Preserve on error idiv").tags({"idiv", "ebit", "todo"})
  .bytes(intel(["idiv rdi"])).DI(S(0x300).RDI(0x0).ebit(True))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DI = MS().RDI(0x3), DE = MS().RAX(0x100), R = False),
}

circuitous_tests = [ saturation_property, _verify_test ]