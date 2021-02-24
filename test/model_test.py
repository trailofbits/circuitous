# Copyright (c) 2021 Trail of Bits, Inc.

from tc import State, Test, _all_regs

import microx
from microx_core import Executor

class ModelTest(Test):
  def case(self, name_=None, **kwargs):
    assert 'E' not in kwargs and 'DE' not in kwargs
    super().case(name_, **kwargs)
    return self

  def _expected_state(self, **kwargs):
    return State()

  def generate(self):
    for case in self.cases:
      result = True if case.expected.result is None else case.expected.result
      case.expected = MicroxGen().get(case.input)
      case.expected.result = result
    return self

class MicroxGen:
  def __init__(self):
    pass

  def get(self, input):
    # So far we don't care about memory ops anyway
    assert "RSP" not in input.registers

    rip = input.registers.get("RIP", 0x1000)
    size = 0x1000

    o = microx.Operations()
    code = microx.ArrayMemoryMap(o, rip, rip + size, can_write=False, can_execute=True)
    bytes = []
    for i in range(0, len(input.bytes), 2):
      bytes.append(int(input.bytes[i] + input.bytes[i + 1], 16))
    code.store_bytes(0x1000, bytes)

    t = microx.EmptyThread(o)
    for reg, val in input.registers.items():
      t.write_register(reg, val)
    t.write_register("RIP", rip)

    m = microx.Memory(o, 64)
    m.add_map(code)
    e = microx.Process(o, m)

    e.execute(t, 1)

    out = State()
    for reg in _all_regs:
      out.set_reg(reg, t.read_register(reg, t.REG_HINT_NONE))
    return out