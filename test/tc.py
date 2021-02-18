# Copyright (c) 2021 Trail of Bits, Inc.

import json

class State:
  def __init__(self):
    self.registers = {}
    self.bits = None
    self.result = None

  def set_reg(self, reg, value):
    self.registers[reg] = value
    return self

  def set_bits(self, bits):
    self.bits = bits
    return self

  def get(self):
    return {"instruction_bits" : self.bits,
            "input_regs" : self.registers }

  def as_json_file(self):
    with open("input_x", 'w') as out:
      json.dump(self.get(), out)
    return "input_x"


# Add methods same as register names to the State to make configuration easier
_regs = ["RAX", "RBX", "RCX", "RDX", "RSI", "RDI",
          "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"]

for reg in _regs:
  setattr(State, reg, lambda s,v,r=reg : s.set_reg(r, v))

class TestCase:
  def __init__(self, input_, expected_, name_):
    self.name = name_
    self.input = input_
    self.expected = expected_
    self.simulated = State()

class Test:
  def __init__(self, name_=""):
    self.name = name_
    #self.state = State()
    #self.out_state = State()
    self.cases = []
    self.dir = None
    self.metafiles = {}
    self._bytes = None
    self._tags = []
    self._names = 0

  def bytes(self, bytes_):
    self._bytes = bytes_
    return self

  def add(self, input, expected, verdict):
    self._names += 1
    case = TestCase(input, expected, str(self._names))
    case.expected.result = verdict
    if (input.bits is None):
      input.bits = self._bytes
    self.cases.append(case)
    return self

  def tags(self, tags_):
    self._tags += tags_
    return self


class NotImplemented(Exception):
  pass

class CompareType:
  IGNORE_REST = 0

class TC:
  def __init__(self):
    pass

  def bytes(self):
    if self.bytes is None:
      self.bytes = self.generate_bytes()
    return self.bytes

  # TODO(lukas): Idea here is that we can provide assembly string
  #              and let the script generate bytes.
  def generate_bytes(self):
    raise NotImplemented("TC::generate_bytes is not implemented yet")


class Hardcoded(TC):
  tag = "hc"

  def __init__(self, tags_=set()):
    self.tags = tags_ + { self.tag }
    self.invocations = []


x = Test("mov imm rdx") \
.bytes("ba12000000") \
.tags("imm_reduce") \
.add(State(), State().RDX(0x12), True) \
.add(State(), State().RDX(0x12000000), True)
