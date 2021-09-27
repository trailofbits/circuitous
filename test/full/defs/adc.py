# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS

test_adc = {
    VerifyTest("adc-a").tags({"min", "adc"})
    .bytes(intel(["adc al, 0x12"]))
    .case("ff.1", I = S(0x100).RAX(0x7fffffffffffffff).CF(0x1), R = True)
    .case("0.1", I = S(0x150).RAX(0x0).CF(0x1), R = True)
    .case("0.0", I = S(0x250).RAX(0x0).CF(0x0), R = True)
    .case("ff.0", I = S(0x350).RAX(0x7fffffffffffffff).CF(0x1), R = True),

    VerifyTest("adc-b_2_variants").tags({"adc"})
    .bytes(intel(["adc al, 0x12", "adc rax, 0x22"]))
    .DI(S(0x350).RAX(0x435).CF(0x1))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True)
    .case(run_bytes = intel(["adc rax, 0x32"]), R = True)
    .case(run_bytes = intel(["adc eax, 0x52"]), R = False)
    .case(run_bytes = intel(["adc al, 0x52"]), R = True),

    VerifyTest("adc-c_2_variants opt").tags({"adc"})
    .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
    .DI(S(0x200).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True),

    VerifyTest("adc-d_2_variants opt, 0 state").tags({"adc"})
    .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15"]))
    .DI(S(0x200).RAX(0x0).RSI(0x0).RDI(0x0).CF(0x1))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True),

    VerifyTest("adc-e_3_variants").tags({"min", "adc"})
    .bytes(intel(["adc rax, 0x12", "adc rsi, 0x15", "adc rdi, 0x17"]))
    .DI(S(0x200).RAX(0x7fffffffffffffff).RSI(0xffffffffffffffff).RDI(0x0).CF(0x1))
    .case(run_bytes = 0, R = True)
    .case(run_bytes = 1, R = True)
    .case(run_bytes = 2, R = True)
    .case(run_bytes = intel(["adc eax, 0x12"]), R = False)
    .case(run_bytes = intel(["adc rax, 0x0"]), R = True)
    .case(run_bytes = intel(["adc rsi, 0x0"]), R = True)
    .case(run_bytes = intel(["adc rdi, 0x0"]), R = True)
}

test_adc_reg_parts = {
    VerifyTest("adc-f_al_0x0").tags({"reduce_regs", "todo"})
    .DI(S(0x250))
    .bytes(intel(["adc al, 0x0"]))
    .case(run_bytes = intel(["adc ah, 0xf"]), DE = MS().uAF(), R = True),
}

circuitous_tests = [test_adc, test_adc_reg_parts]
