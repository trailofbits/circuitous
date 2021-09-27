# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel, raw
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, MIG, MemHint

timestamp_inc = {
  VerifyTest("timestamp basic").tags({"ts", "min"})
  .bytes(intel(["mov rdx, 0xba120000"])).DI(S(0x350))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 0, DE = MS().ts(0x52), R = False)
  .case(run_bytes = 0, DE = MS().ts(0), R = False)
  .case(run_bytes = 0, DE = MS().ts(2), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x52), R = True)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0x52), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0), R = False)
  .case(run_bytes = 0, DI = MS().ts(0x53), DE = MS().ts(0x55), R = False),
}

circuitous_tests = [timestamp_inc]
