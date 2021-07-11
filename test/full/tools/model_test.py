# Copyright (c) 2021 Trail of Bits, Inc.

import copy
import math

from tools.tc import State, Test, _regs, _aflags, MS, MemHint

import microx
from microx_core import Executor

class ModelTest(Test):

  def case(self, name_=None, **kwargs):
    assert 'E' not in kwargs
    super().case(name_, **kwargs)
    return self

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


# Watch out, exceptions works funky
class Process_(microx.Process):

  def __init__(self, ops, mem):
    super(Process_, self).__init__(ops, mem)
    self._mem_hints = []

  def write_memory(self, addr, data):
    as_str = str(self._ops.convert_to_integer(data))
    hint = MemHint(addr, int(as_str), MemHint.WRITE, len(data))
    self._mem_hints.append(hint)

    return super(Process_, self).write_memory(addr, data)

  def read_memory(self, addr, num_bytes, hint):
    read = super(Process_, self).read_memory(addr, num_bytes, hint)
    as_str = str(self._ops.convert_to_integer(read))

    if hint in self.MEM_READ_HINTS and hint not in self.MEM_EXEC_HINTS:
      hint_ = MemHint(addr, int(as_str), MemHint.READ, len(read))
      self._mem_hints.append(hint_)

    return super(Process_, self).read_memory(addr, num_bytes, hint)

class MicroxGen:
  def __init__(self):
    pass

  def get(self, input):
    # TODO(lukas): We need to handle RSP once we support memory ops.
    # assert "RSP" not in input.registers

    rip = input.registers.get("RIP", 0x1000)
    rsp = input.registers.get("RSP")

    o = microx.Operations()
    m = microx.Memory(o, 64)

    size = 0x1000
    code = microx.ArrayMemoryMap(o, rip, rip + size, can_write=True, can_execute=True)
    m.add_map(code)

    #TODO(lukas): Stack & other memory
    if input.memory is not None:
      for region in input.memory.entries:
        addr, bytes, (r, w, e) = region
        assert len(bytes) <= size
        aligned_addr = (addr >> 12) << 12
        amm = microx.ArrayMemoryMap(o, aligned_addr, aligned_addr + size,
                                    can_read=r, can_write=w, can_execute=e)
        m.add_map(amm)
        amm.store_bytes(addr, bytes)

    MS()

    bytes = []
    for i in range(0, len(input.bytes), 2):
      bytes.append(int(input.bytes[i] + input.bytes[i + 1], 16))
    code.store_bytes(rip, bytes)

    t = microx.EmptyThread(o)
    for reg, val in input.registers.items():
      if val is not None:
        t.write_register(reg, val)
    t.write_register("RIP", rip)

    e = Process_(o, m)

    out = State()

    try:
      e.execute(t, 1)
    except FloatingPointError as e:
      out = copy.deepcopy(input)
      out.ebit(True)
      out.timestamp += 1
      return out
    out.timestamp = input.timestamp + 1

    for mem_hint in e._mem_hints:
      out.mem_hint(mem_hint)

    for reg in _regs + _aflags:
      out.set_reg(reg, t.read_register(reg, t.REG_HINT_NONE))
    out.ebit(False)
    return out
