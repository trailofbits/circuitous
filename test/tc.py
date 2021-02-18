# Copyright (c) 2021 Trail of Bits, Inc.

import json

class State:
  __slots__ = ('registers', 'bytes', 'result')

  def __init__(self):
    self.registers = {}
    self.bytes = None
    self.result = None

  def set_reg(self, reg, value):
    self.registers[reg] = value
    return self

  def set_bytes(self, bytes_):
    self.bytes = bytes_
    return self

  def get(self):
    return {"instruction_bits" : self.bytes,
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
  __slots__ = ('name', 'bytes', 'input', 'expected', 'simulated')

  def __init__(self, name_=None, bytes_=None, input_=None, expected_=None):
    self.name = name_
    self.bytes = bytes_
    self.input = input_
    self.expected = expected_
    self.simulated = State()

class Test:
  def __init__(self, name_=""):
    self.name = name_
    self.cases = []
    self.dir = None
    self.metafiles = {}
    self._bytes = None
    self._tags = []
    self._names = 0

  def bytes(self, bytes_):
    self._bytes = bytes_
    return self

  def case(self, **kwargs):
    case = TestCase()
    self._names += 1
    case.name = kwargs.get('name', str(self._names))
    case.bytes = kwargs.get('lift_bytes', self._bytes)
    case.input = kwargs.get('I', State())
    case.input.bytes = kwargs.get('run_bytes', self._bytes)
    case.expected = kwargs.get('E', State())
    case.expected.result = kwargs.get('R', None)
    assert case.expected.result is not None
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