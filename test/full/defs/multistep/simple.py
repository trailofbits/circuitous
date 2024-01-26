# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint, TraceTest
from tools.byte_generator import intel
from tools.verify_test import VerifyTest
from tools.model_test import microx_gen

import tools.tgen as tgen


def mk_nop():
    tt = TraceTest('nop-trace-a', state=random_state(42)) \
        .tags({'trace', 'min'}) \
        .lift_opts(["--lift-with", "mux-heavy"]) \
        .raw_circuit(intel(['nop', 'mov rax, 0x0']))
    base = tt.make_case(R=True) \
        .add_maker(microx_gen())

    base.add_steps(intel(['nop', 'mov rax, 0x0', 'nop', 'mov rbx, 0x10']))
    return { tt }

circuitous_tests = [mk_nop()]
