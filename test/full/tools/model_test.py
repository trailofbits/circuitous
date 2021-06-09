# Copyright (c) 2021 Trail of Bits, Inc.

import copy

from tools.tc import State, Test, _regs, _aflags, MS

import microx
from microx_core import Executor

class ModelTest(Test):

  def case(self, name_=None, **kwargs):
    assert 'E' not in kwargs
    super().case(name_, **kwargs)
    self.e_mutators.append(kwargs.get('DE', MS()))
    return self

  def _expected_state(self, **kwargs):
    return State()

  def generate(self, **kwargs):
    super().generate(**kwargs)
    for idx, case in enumerate(self.cases):
      result = True if case.expected.result is None else case.expected.result
      try:
        case.expected = MicroxGen().get(case.input).mutate(self.e_mutators[idx])
      except Exception as e:
        print("Microx fail in: " + self.name + " case " + case.name)
        print("Bytes: " + case.input.bytes)
        raise e
      case.expected.result = result
    return self

class MicroxGen:
  def __init__(self):
    pass

  def get(self, input):
    # TODO(lukas): We need to handle RSP once we support memory ops.
    # assert "RSP" not in input.registers

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
      if val is not None:
        t.write_register(reg, val)
    t.write_register("RIP", rip)

    m = microx.Memory(o, 64)
    m.add_map(code)
    e = microx.Process(o, m)

    e.execute(t, 1)

    out = State()

    try:
      e.execute(t, 1)
    except FloatingPointError as e:
      out = copy.deepcopy(input)
      out.ebit(True)
      return out

    for reg in _regs + _aflags:
      out.set_reg(reg, t.read_register(reg, t.REG_HINT_NONE))
    out.ebit(False)
    return out
