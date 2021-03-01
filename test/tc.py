# Copyright (c) 2021 Trail of Bits, Inc.

import copy
import json

# Add methods same as register names to the State to make configuration easier
_regs = [ "RIP",
          "RAX", "RBX", "RCX", "RDX", "RSI", "RDI",
          "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"]
_e_regs = ["EAX, EBX", "ECX", "EDX"]
_b_regs = ["AX", "BX", "CX", "DX"]
_a_regs = ["AL", "AH"]
_aflags = ["AF", "CF", "OF", "ZF", "PF", "SF"]
_all_regs = _regs + _aflags + _e_regs + _b_regs + _a_regs

class State:
  __slots__ = ('registers', 'bytes', 'result')

  def __init__(self, default_val = 0, default_rip = 0x1000):
    self.registers = {}
    self.bytes = None
    self.result = None
    for x in _regs + _aflags:
      self.registers[x] = default_val
    self.registers["RIP"] = default_rip

  def aflags(self, val):
    for flag in _aflags:
      self.registers[flag] = val
    return self

  def mutate(self, other):
    mutated = copy.deepcopy(self)
    for reg, val in other.registers.items():
      mutated.set_reg(reg, val)
    return mutated

  def set_reg(self, reg, value):
    self.registers[reg] = value
    return self

  def set_bytes(self, bytes_):
    self.bytes = bytes_
    return self

  def get(self):
    out = {"instruction_bits" : self.bytes,
           "input_regs" : {}}
    for reg, val in self.registers.items():
      out["input_regs"][reg] = str(val)
    return out

  def as_json_file(self, prefix="def."):
    with open(prefix + "input.json", 'w') as out:
      json.dump(self.get(), out)
    return prefix + "input.json"


def S():
  return State()

# Add methods same as register names to the State to make configuration easier
_regs = [ "RIP",
          "RAX", "RBX", "RCX", "RDX", "RSI", "RDI",
          "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"]
_aflags = ["AF", "CF", "OF", "ZF", "PF", "SF"]
_all_regs = _regs + _aflags


for reg in _regs + _aflags:
  setattr(State, reg, lambda s,v,r=reg : s.set_reg(r, v))


class Acceptance():
  def generate(self, case, **kwargs):
    return self.should_accept(case, **kwargs)

class AcceptByLiftOpts(Acceptance):
  __slots__ = ('opts', 'val')

  def __init__(self, opts_, val_):
    if isinstance(opts_, list):
      self.opts = opts_
    else:
      self.opts = [opts_]
    self.val = val_

  def should_accept(self, case, **kwargs):
    lift_args = kwargs.get('lift')
    assert lift_args is not None
    valid = self.is_valid(lift_args)
    return valid, self.val

class HasLiftArgs(AcceptByLiftOpts):
  def is_valid(self, lift_args):
    return all(x in lift_args for x in self.opts)

class HasNotLiftArgs(AcceptByLiftOpts):
  def is_valid(self, lift_args):
    return all(x not in lift_args for x in self.opts)

class Accept(Acceptance):
  def should_accept(self, case, **kwargs):
    return True, True;

class Reject(Acceptance):
  def should_accept(self, case, **kwargs):
    return True, False;

def if_has(what, val):
  return HasLiftArgs(what, val)

def if_nhas(what, val):
  return HasNotLiftArgs(what, val)

def accept():
  return Accept()

def reject():
  return Reject()

class TestCase:
  __slots__ = ('name', 'bytes', 'input', 'expected', 'simulated', '_result_generator')

  def __init__(self, name_=None, bytes_=None, input_=None, expected_=None):
    self.name = name_
    self.bytes = bytes_
    self.input = input_
    self.expected = expected_
    self.simulated = State()
    self._result_generator = None

# Base class to define tests with -- unfortunately it already does a lot of custom options
# feel free to come with better decomposition
# Core method is the `case` method which adds the tests to be run. It has a lot of options,
# some of which can be "inherited" from the `Test` itself (to avoid code repetition).
class Test:
  __slots__ = ('name', 'cases', 'dir', 'metafiles', '_bytes',
               '_tags', '_names', '_lift_opts', '_inst_arr',
               'default_istate')

  def __init__(self, name_=""):
    # TODO(lukas): Split this into
    #                * inheritable attributes
    #                * helpers
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

  # Options -- [] denotes optional, <> denotes that it is inherited if not specified:
  #  [*] 'name' - name of the case for better orientation
  #  <*> 'lift_bytes' - bytes to lift
  #  <*> 'run_bytes' - string representation of bytes or a number, which is an index
  #                    into `Test` lift bytes
  #   *  'R' - expected output
  #  [*] 'RG' - generator of expected output -- see `Acceptance` class. Useful is result
  #             is different with different lift options.
  #  <*> 'I'  - input state, inherited from `Test` if not present
  #  [*] 'DI' - use `Test` input state but modify it
  #   *  'E'  - expected output state
  #  [*] 'DE' - same as `DI` but for expected
  # TODO(lukas): Fix `run_bytes` indexing into `Test` lift_bytes.
  #              Test if `lift_bytes` actually work.
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
    elif isinstance(inst_chooser, list):
      assert len(inst_chooser) == 1, "Cannot run more than one inst"
      case.input.bytes = "".join(inst_chooser[0])
      print(case.input.bytes)
    else:
      case.input.bytes = inst_chooser

    case.expected.result = kwargs.get('R', None)
    assert case.expected.result is not None
    verdict = kwargs = kwargs.get('RG', None)
    case._result_generator = verdict
    self.cases.append(case)
    return self

  def tags(self, tags_):
    if isinstance(tags_, str):
      self._tags.add(tags_)
    else:
      self._tags.update(tags_)
    return self

  def generate(self, **kwargs):
    for case in self.cases:
      if case._result_generator is not None:
        should, val = case._result_generator.generate(self, **kwargs)
        if should:
          case.expected.result = val
