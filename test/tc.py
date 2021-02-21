# Copyright (c) 2021 Trail of Bits, Inc.

import copy
import json

class State:
  __slots__ = ('registers', 'bytes', 'result')

  def __init__(self):
    self.registers = {}
    self.bytes = None
    self.result = None

  def aflags(self, val):
    for flag in _aflags:
      self.registers[flag] = val
    return self

  def mutate(self, other):
    mutated = copy.deepcopy(self)
    for reg, val in other.registers.items():
      mutated[reg] = val
    return mutated

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
_aflags = ["AF", "CF", "OF", "ZF", "PF", "SF"]

for reg in _regs + _aflags:
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
  __slots__ = ('name', 'cases', 'dir', 'metafiles', '_bytes',
               '_tags', '_names', '_lift_opts', '_inst_arr',
               'default_istate')

  def __init__(self, name_=""):
    self.name = name_
    self.cases = []
    self.dir = None
    self.metafiles = {}
    self._bytes = None
    self._tags = set()
    self._names = 0
    self._lift_opts = []
    self._inst_arr = []
    self.default_istate = State()

  def bytes(self, bytes_):
    if isinstance(bytes_, list):
      self._inst_arr = bytes_
      self._bytes = "".join(bytes_)
    else:
      self._bytes = bytes_
    return self

  def DI(self, state):
    self.default_istate = state
    return self

  def lift_opts(self, args):
    self._lift_opts += args
    return self

  def _input_state(self, **kwargs):
    assert not(bool('I' in kwargs) and bool('DI' in kwargs))
    istate = kwargs.get('I', None)
    if istate is not None:
      return istate

    # We know it is present due to lead assert
    distate = kwargs.get('DI', None)
    if distate is not None:
      return self.default_istate.mutate(distate)
    return copy.deepcopy(self.default_istate)

  def _expected_state(self, **kwargs):
    assert not(bool('E' in kwargs) and bool('DE' in kwargs))
    estate = kwargs.get('E', None)
    if estate is not None:
      return estate

    # We know it is present due to lead assert
    distate = kwargs.get('DE', None)
    if distate is not None:
      return self.default_istate.mutate(distate)
    return State()

  def case(self, name_=None, **kwargs):
    case = TestCase()
    self._names += 1
    case.name = name_
    case.name = kwargs.get('name',
                           case.name if case.name is not None else str(self._names))
    case.bytes = kwargs.get('lift_bytes', self._bytes)

    case.expected = self._expected_state(**kwargs)
    case.input = self._input_state(**kwargs)

    inst_chooser = kwargs.get('run_bytes', self._bytes)
    # TODO(lukas): A bit bleh.
    if isinstance(inst_chooser, int):
      case.input.bytes = self._inst_arr[inst_chooser]
    else:
      case.input.bytes = inst_chooser

    case.expected.result = kwargs.get('R', None)
    assert case.expected.result is not None
    self.cases.append(case)
    return self

  def tags(self, tags_):
    self._tags.update(set(tags_))
    return self

  def generate(self):
    pass