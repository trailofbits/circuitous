# Copyright (c) 2021 Trail of Bits, Inc.

import copy
import json
import os
import random

from itertools import permutations

# Add methods same as register names to the State to make configuration easier
_regs = [ "RIP", "RSP", "RBP",
          "RAX", "RBX", "RCX", "RDX", "RSI", "RDI",
          "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"]
_e_regs = ["EAX, EBX", "ECX", "EDX"]
_b_regs = ["AX", "BX", "CX", "DX"]
_a_regs = ["AL", "AH"]
_aflags = ["AF", "CF", "OF", "ZF", "PF", "SF"]
_all_regs = _regs + _aflags + _e_regs + _b_regs + _a_regs

class MemHint:
  READ = 0
  WRITE = 1
  __slots__ = ('used','addr', 'value', 'type', 'size')

  def __init__(self, addr_, val_, type_, size_):
    assert isinstance(addr_, int)
    assert isinstance(val_, int)
    assert isinstance(type_, int)
    assert isinstance(size_, int)

    self.used = 1
    self.addr = addr_
    self.value = val_

    assert type_ in [MemHint.READ, MemHint.WRITE]
    self.type = type_

    self.size = size_

  def zero_hint():
    x = MemHint(None, None, MemHint.READ, None)
    x.used = False
    return x

  def get_unused(self):
    return "0"

  def get(self):
    out  = self.ts << (64 + 64 + 8)
    out += self.value << (64 + 8)
    out += self.addr << 8
    out += (self.size << 4) + (self.mode << 1) + 1
    return str(out)

  def __eq__(self, other):
    if other is None:
      return False

    if other.used != self.used:
      return False

    if self.used == False:
      return True

    return self.addr == other.addr \
           and self.value == other.value \
           and self.type == other.type \
           and self.size == other.size

  def __str__(self):
    if self.used == False:
      return "[ unused ]"

    out = "[ "
    out += "READ " if self.type == MemHint.READ else "WRITE: "
    out += hex(self.addr) + " := "
    out += str(self.value) + " , "
    out += str(self.size) + " ]"
    return out

def split_hints(hints):
  write = []
  read = []
  unused = []
  for h in hints:
    if h.used == False:
      unused.append(h)
    elif h.type == MemHint.READ:
      read.append(h)
    elif h.type == MemHint.WRITE:
      write.append(h)
    else:
      assert False
  return (unused, read, write)

def permute_compare(fst, snd):
  if len(fst) == 0 and len(snd) == 0:
    return True
  eqs = 0
  for tmp in permutations(snd):
    if tmp == tuple(fst):
      eqs += 1
  return eqs == 1

#TODO(lukas): Handle unused better
def compare_hints(fst, snd):
  assert fst is not None and snd is not None
  _, fst_read, fst_write = split_hints(fst)
  _, snd_read, snd_write = split_hints(snd)
  reads = permute_compare(fst_read, snd_read)
  writes = permute_compare(fst_write, snd_write)
  return reads and writes


class MI_gen:
  __slot__ = ('seed', 'generated')

  def __init__(self, seed_=None):
    seed_ = seed_ if seed_ is not None else 42
    self.seed = seed_
    self.generated = MemInput()

    random.seed(self.seed)

  def data(self, size):
    out = []
    for _ in range(size):
      out.append(random.randint(0, 16))
    return out

  def mem_(self, addr, size, **kwargs):
    self.generated.add(addr, self.data(size), **kwargs)
    return self

  def r(self, addr, size):
    return self.mem_(addr, size, r=True)

  def rw(self, addr, size):
    return self.mem_(addr, size, r=True, w=True)


def MIG(seed=None): return MI_gen(seed)

class MemInput:
  __slots__ = ('entries')

  def __init__(self):
    self.entries = []

  def add(self, addr, data, **kwargs):
    if isinstance(data, str):
      bytes = []
      for i in range(0, len(data), 2):
        bytes.append(int(data[i] + data[i + 1], 16))
    elif isinstance(data, list):
      bytes = data
    else:
      assert False
    r = kwargs.get('r', False)
    w = kwargs.get('w', False)
    e = kwargs.get('e', False)
    self.entries.append((addr, bytes, (r, w, e)))

  def get(self):
    out = {}
    for addr, data, _ in self.entries:
      out[hex(addr)[2:]] = "".join([f"{x:02x}" for x in data])
    return out

  def extend(self, other):
    self.entries += other.entries
    return self


class StateBase:
  __slots__ = ('registers', '_ebit', 'timestamp', 'undefined', 'mem_hints',
               'memory')

  def __init__(self):
    self.registers = {}
    self._ebit = None
    self.timestamp = None
    self.undefined = set()
    self.mem_hints = []

    self.memory = None

  def set_reg(self, reg, val):
    self.registers[reg] = val
    if reg in self.undefined:
      self.undefined.remove(reg)
    return self

  def unset_reg(self, reg):
    if reg in self.registers:
      self.registers.pop(reg)
    self.undefined.add(reg)
    return self

  def ebit(self, val) :
    self._ebit = val
    return self

  def ts(self, val) :
    self.timestamp = val
    return self

  def aflags(self, val):
    for flag in _aflags:
      self.registers[flag] = val
    return self

  def mem_hint(self, val):
    self.mem_hints.append(val)
    return self

  def mig(self, gen):
    if self.memory is None:
      self.memory = MemInput()
    self.memory.extend(gen.generated)
    return self

  def mem(self, addr, data, **kwargs):
    if self.memory is None:
      self.memory = MemInput()
    self.memory.add(addr, data, **kwargs)
    return self

  def rmem(self, addr, data): return self.mem(addr, data, r=True)
  def rwmem(self, addr, data): return self.mem(addr, data, r=True, w=True)
  def emem(self, addr, data): return self.mem(addr, data, e=True)


class State(StateBase):

  __slots__ = ('bytes', 'result')

  def __init__(self, default_val = None, default_rip = 0x1000):
    super().__init__()
    for x in _regs + _aflags:
      self.set_reg(x, default_val)
    self.set_reg("RIP", default_rip)
    self.ebit(False)
    self.ts(0)

    self.bytes = None
    self.result = None

  def disarm(self):
    self.registers = None
    self._ebit = None
    self.timestamp = None
    self.mem_hints = None

  def __bool__(self):
    return self.registers is not None \
           or self.ebit is not None \
           or self.timestamp is not None

  def mutate(self, other):
    mutated = copy.deepcopy(self)

    for reg, val in other.registers.items():
      mutated.set_reg(reg, val)

    for reg in other.undefined:
      mutated.registers.pop(reg)
      mutated.undefined.add(reg)

    if other._ebit is not None:
      mutated._ebit = other._ebit

    if other.timestamp is not None:
      mutated.timestamp = other.timestamp

    if other.memory is not None:
      mutated.memory = other.memory
    return mutated

  def set_bytes(self, bytes_):
    self.bytes = bytes_
    return self

  def get(self):
    out = {"inst_bits" : self.bytes if self.bytes is not None else "0",
           "ebit" : self._ebit,
           "timestamp" : self.timestamp,
           "regs" : {},
           "mem_hints": {},
           "memory": {}}
    for reg, val in self.registers.items():
      if val is not None:
        out["regs"][reg] = str(val)
    #for id, hint in self.mem_hints.items():
    #  out["mem_hints"][id] = hint.get()
    if self.memory is not None:
      out["memory"] = self.memory.get()
    return out

  def as_json_file(self, prefix="def.", dir=None):
    fname = prefix + "input.json"
    path = fname if dir is None else os.path.join(dir, fname)
    with open(path, 'w') as out:
      json.dump(self.get(), out)
    return path

class Mutator(StateBase):
  pass

def MS():
  return Mutator()

def S(x = None):
  return State(x)

for reg in _regs + _aflags:
  def s_reg_mem(self, reg, value, size, **kwargs):
    self.set_reg(reg, value)
    self.mig(MIG().mem_(value, size, **kwargs))
    return self

  def s_reg_rmem(reg):
    def tmp(self, value, size=0x100):
      return s_reg_mem(self, reg, value, size, r=True)
    return tmp

  def s_reg_wrmem(reg):
    def tmp(self, value, size=0x100):
      return s_reg_mem(self, reg, value, size, r=True, w=True)
    return tmp

  setattr(StateBase, "rm_" + reg, s_reg_rmem(reg))
  setattr(StateBase, "wrm_" + reg, s_reg_wrmem(reg))

  setattr(State, reg, lambda s,v,r=reg : s.set_reg(r, v))
  setattr(Mutator, reg, lambda s,v,r=reg : s.set_reg(r, v))
  setattr(Mutator, "u" + reg, lambda s,r=reg : s.unset_reg(r))


class TestCase:
  __slots__ = ('name', 'bytes', 'input', 'expected', 'simulated', '_result_generator',
               'run_mode')

  def __init__(self, name_=None, bytes_=None, input_=None, expected_=None):
    self.name = name_
    self.bytes = bytes_
    self.input = input_
    self.expected = expected_
    self.simulated = State()
    self._result_generator = None
    self.run_mode = '--derive'

# Base class to define tests with -- unfortunately it already does a lot of custom options
# feel free to come with better decomposition
# Core method is the `case` method which adds the tests to be run. It has a lot of options,
# some of which can be "inherited" from the `Test` itself (to avoid code repetition).
class Test:
  __slots__ = ('name', 'cases', 'dir', 'metafiles', '_bytes',
               '_tags', '_names', '_lift_opts', '_inst_arr',
               'default_istate', 'e_mutators', '_mode')

  def __init__(self, name_=""):
    # TODO(lukas): Split this into
    #                * inheritable attributes
    #                * helpers
    self.name = name_
    self.cases = []
    self.dir = None
    self.metafiles = {}

    self._tags = set()
    self._names = 0
    self._lift_opts = []

    self.default_istate = State()
    self.e_mutators = []

    # To allow a delayed byte generation, we allow two types of bytes
    # TODO(lukas): What if I want to combine them?

    # _bytes must always be a `Union[str, LazyBytes]`
    self._bytes = None
    # _inst_arr must always be a `Union[list[str], LazyBytes]`
    self._inst_arr = []

    self._mode = None

  def mode(self, what) :
    self._mode = what
    return self

  def bytes(self, bytes_):
    assert bytes_ is not None, "Cannot build the bytes yet (TODO)."
    if isinstance(bytes_, list):
      self._inst_arr = bytes_
      self._bytes = "".join(bytes_)
    elif isinstance(bytes_, str):
      self._inst_arr = [bytes_]
      self._bytes = bytes_
    else:
      self._inst_arr = bytes_
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
    case.run_mode = self._mode if self._mode is not None else case.run_mode
    self._names += 1
    case.name = name_
    case.name = kwargs.get('name',
                           case.name if case.name is not None else str(self._names))
    case.bytes = kwargs.get('lift_bytes', self._bytes)

    case.expected = self._expected_state(**kwargs)
    case.input = self._input_state(**kwargs)

    inst_chooser = kwargs.get('run_bytes', None)

    # TODO(lukas): A "bit" bleh.
    if inst_chooser is None:
      assert len(self._inst_arr) == 1, "Cannot default to multiple insts."
      case.input.bytes = self._bytes
    elif isinstance(inst_chooser, int):
      case.input.bytes = self._inst_arr[inst_chooser]
    elif isinstance(inst_chooser, list):
      assert len(inst_chooser) == 1, "Cannot run more than one inst."
      case.input.bytes = "".join(inst_chooser[0])
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
    def check_bytes(which):
      if not isinstance(which, str):
        return "".join(which.compile())
      return which

    print("[ > ] Generating test case", self.name)
    self._bytes = check_bytes(self._bytes)
    for case in self.cases:
      case.bytes = check_bytes(case.bytes)
      case.input.bytes = check_bytes(case.input.bytes)
      if case._result_generator is not None:
        should, val = case._result_generator.generate(self, **kwargs)
        if should:
          case.expected.result = val
