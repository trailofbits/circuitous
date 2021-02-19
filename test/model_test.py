# Copyright (c) 2021 Trail of Bits, Inc.

from tc import State, Test, _regs

import microx
from microx_core import Executor

class ModelTest(Test):
  def case(self, name_=None, **kwargs):
    assert 'E' not in kwargs
    super().case(name_, **kwargs)
    self.expected = None
    return self

  def generate(self):
    for case in self.cases:
      assert self.expected is None
      case.expected = MicroxGen().get(case.input)
      case.expected.result = True
    return self

class MicroxGen:
  def __init__(self):
    pass

  def get(self, input):
    # So far we don't care about memory ops anyway
    assert "RIP" not in input.registers
    assert "RSP" not in input.registers

    o = microx.Operations()
    code = microx.ArrayMemoryMap(o, 0x1000, 0x2000, can_write=False, can_execute=True)
    bytes = []
    for i in range(0, len(input.bytes), 2):
      bytes.append(int(input.bytes[i] + input.bytes[i + 1], 16))
    code.store_bytes(0x1000, bytes)

    t = microx.EmptyThread(o)
    for reg, val in input.registers:
      t.write_register(reg, val)
    t.write_register("RIP", 0x1000)

    m = microx.Memory(o, 64)
    m.add_map(code)
    e = microx.Process(o, m)

    e.execute(t, 1)

    out = State()
    for reg in _regs:
      out.set_reg(reg, t.read_register(reg, t.REG_HINT_NONE))
    return out