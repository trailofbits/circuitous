# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

_verify_test = {
  VerifyTest("selftest").tags({"selftest"})
  .bytes(intel(["mov rdx, 0x12100"])).DI(S(0x350).ebit(False))
  .case(run_bytes = 0, DE = MS().RDX(0x12), R = False)
}

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

pop = {
  ModelTest("pop basic").tags({"pop", "min", "mem"}).bytes(intel(["pop rax"]))
  .DI(S(0x20000).RAX(0x56).RIP(0x1000).rmem(0x20000, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["pop rbx"]), R = True)
  .case(run_bytes = intel(["pop rdx"]), R = True)
  .case(run_bytes = intel(["pop rbp"]), R = True)
  .case(run_bytes = intel(["pop r9"]), R = False)
}

push = {
  ModelTest("push basic").tags({"push", "min", "mem"}).bytes(intel(["push rax"]))
  .DI(S(0x40040).RSP(0x20040).RIP(0x60000).RAX(0x56)
      .rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["push rbx"]), R = True)
  .case(run_bytes = intel(["push rdx"]), R = True)
  .case(run_bytes = intel(["push rbp"]), R = True)
  .case(run_bytes = intel(["push r9"]), R = False)
}

push_cx = {
  ModelTest("push cx").tags({"push", "mem"}).bytes(intel(["push cx"]))
  .DI(S(0x20040).RAX(0x56).RIP(0x1000).rwmem(0x20038, "0000ffeeddccbbaa998877665544332211"))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["push bx"]), R = True)
  .case(run_bytes = intel(["push dx"]), R = True)
  .case(run_bytes = intel(["push bp"]), R = True)
  .case(run_bytes = intel(["push rcx"]), R = False)
}

push_deref = {
  ModelTest("push_deref").tags({"push", "min", "mem", "todo"})
  .bytes(intel(["push -0x5[rax]"]))
  .DI(S(0x20040).RAX(0x50056).RIP(0x1000).RDX(0x70070).RBX(0x40040)
                .rwmem(0x40030, "3132e3" * 25)
                .rwmem(0x50050, "ff" * 25)
                .rwmem(0x40030, "ee" * 100)
                .rwmem(0x70000, "ba12" * 100))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = intel(["push [rax]"]), R = True)
  .case(run_bytes = intel(["push 0x5[rbx]"]), R = True)
  .case(run_bytes = intel(["push -0x35[rdx]"]), R = True)
  .case(run_bytes = intel(["push [rax]"]), R = False)
}

circuitous_tests = [ saturation_property, _verify_test, timestamp_inc,
  push, pop, push_cx, push_deref ]