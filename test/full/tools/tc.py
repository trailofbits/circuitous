# Copyright (c) 2021 Trail of Bits, Inc.

import copy
import json
import os
import random

from itertools import permutations

from tools.circuit import RawCircuit, CookedCircuit

# Add methods same as register names to the State to make configuration easier
_regs = [ "RIP", "RSP", "RBP",
          "RAX", "RBX", "RCX", "RDX", "RSI", "RDI",
          "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"]
_e_regs = ["EAX", "EBX", "ECX", "EDX", "ESI", "EDI", "EIP", "ESP", "EBP"]
_b_regs = ["AX", "BX", "CX", "DX"]
_a_regs = ["AL", "AH"]
_aflags = ["AF", "CF", "OF", "ZF", "PF", "SF", "DF"]
_segments = ["SSBASE", "DSBASE", "FSBASE", "CSBASE", "GSBASE", "ESBASE"]
_all_regs = _regs + _aflags + _e_regs + _b_regs + _a_regs + _segments

class MemHint:
    READ = 0
    WRITE = 1
    __slots__ = ('used','addr', 'value', 'type', 'size', 'ts')

    def __init__(self, addr_, val_, type_, size_, ts_=0):
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
        self.ts = ts_

    def read(addr_, val_, size_):
        return MemHint(addr_, val_, MemHint.READ, size_)
    def write(addr_, val_, size_):
        return MemHint(addr_, val_, MemHint.WRITE, size_)

    def as_tuple(self):
        return (self.used, self.addr, self.value, self.type, self.size, self.ts)

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
        out += (self.size << 4) + (self.type << 1) + 1
        return hex(out)[2:]

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
                 'memory', '_seed', 'instruction_bits')

    def __init__(self):
        self.registers = {}
        self._ebit = None
        self.timestamp = None
        self.undefined = set()
        self.mem_hints = []

        self.instruction_bits = None

        self.memory = None
        self._seed = 42

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

    def segments(self, val):
        for segment_base in _segments:
            self.registers[segment_base] = val
        return self

    def mem_hint(self, val):
        if self.mem_hints is None:
            self.mem_hints = []
        if isinstance(val, list):
            for x in val:
                self.mem_hint(x)
        else:
            self.mem_hints.append(copy.deepcopy(val))
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


class StateImpl(StateBase):
    __slots__ = ('bytes', 'result')

    def __init__(self):
        super().__init__()

    def disarm(self):
        self.registers = None
        self.timestamp = None

    def __bool__(self):
        return self.registers is not None \
               or self.ebit is not None \
               or self.timestamp is not None

    def mutate(self, other):
        mutated = copy.deepcopy(self)

        for reg, val in other.registers.items():
            mutated.set_reg(reg, val)

        for reg in other.undefined:
            if reg in mutated.registers:
                mutated.registers.pop(reg)
            mutated.undefined.add(reg)

        if other._ebit is not None:
            mutated._ebit = other._ebit

        if other.timestamp is not None:
            mutated.timestamp = other.timestamp

        if other.memory is not None:
            mutated.memory = copy.deepcopy(other.memory)

        if other.mem_hints is not None:
            mutated.mem_hint(other.mem_hints)
        return mutated

    def set_bytes(self, bytes_):
        self.bytes = bytes_
        return self

    def get(self):
        out = {"instruction_bits" : self.bytes if self.bytes is not None else "00",
               "error_flag" : hex(self._ebit)[2:],
               "timestamp" : hex(self.timestamp)[2:],
               "regs" : {}}
        for reg, val in self.registers.items():
            if val is not None:
                out["regs"][reg] = hex(val)[2:]
        return out

class State32(StateImpl):
    def __init__(self, default_val = None, default_rip = 0x87000):
        super().__init__()
        for x in _e_regs:
            self.set_reg(x, default_val)
        for x in _aflags:
            if default_val is not None:
                self.set_reg(x, default_val % 2)
            else:
                self.set_reg(x, default_val)
        self.set_reg("EIP", default_rip)
        self.ebit(False)
        self.ts(0)

        self.bytes = None
        self.result = None

class State(StateImpl):
    def __init__(self, default_val = None, default_rip = 0x87000):
        super().__init__()
        for x in _regs:
            self.set_reg(x, default_val)
        for x in _aflags:
            if default_val is not None:
                self.set_reg(x, default_val % 2)
            else:
                self.set_reg(x, default_val)
        self.set_reg("RIP", default_rip)
        self.ebit(False)
        self.ts(0)

        self.bytes = None
        self.result = None


class Mutator(StateBase):
    pass

class Mutator32(StateBase):
    pass

def MS():
  return Mutator()

def MS32():
  return Mutator32()


def S(x = None):
  return State(x)

def S32(x = None):
  return State32(x)

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

    setattr(State, "rm_" + reg, s_reg_rmem(reg))
    setattr(State, "wrm_" + reg, s_reg_wrmem(reg))

    setattr(State, reg, lambda s,v,r=reg : s.set_reg(r, v))
    setattr(Mutator, reg, lambda s,v,r=reg : s.set_reg(r, v))
    setattr(Mutator, "u" + reg, lambda s,r=reg : s.unset_reg(r))


for reg in _e_regs + _aflags + _segments:
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

    setattr(State32, "rm_" + reg, s_reg_rmem(reg))
    setattr(State32, "wrm_" + reg, s_reg_wrmem(reg))

    print("Injecting", reg)
    setattr(State32, reg, lambda s,v,r=reg : s.set_reg(r, v))
    setattr(Mutator32, reg, lambda s,v,r=reg : s.set_reg(r, v))
    setattr(Mutator32, "u" + reg, lambda s,r=reg : s.unset_reg(r))



def random_state(seed):
    random.seed(seed)
    out = S(0x1031)
    for reg in _regs:
        getattr(out, reg)(random.randint(0, (1 << 64) - 1))
    for reg in _aflags:
        getattr(out, reg)(random.randint(0, 1))
    return out

def populate_state(gen):
    out = S(0x1031)
    for reg in _regs:
        getattr(out, reg)(gen(0, (1 << 25) - 1))
    for reg in _aflags:
        getattr(out, reg)(gen(0, 1))
    return out


class Trace:
    __slots__ = ('_states')

    def __init__(self, states = []):
        self._states = []
        self._states += states

    def __getitem__(self, idx):        return self._states[idx]
    def __setitem__(self, idx, value): self._states[idx] = value
    def __len__(self):                 return len(self._states)
    def append(self, state):           return self._state.append(state)

    def as_json(self):
        obj = {}
        obj["id"] = 1
        obj["entries"] = [ x.get() for x in self._states ]

        if self._states[0].memory is not None:
            obj["initial_memory"] = self._states[0].memory.get()
        return obj

# `State -> State`
class SingleStepTrace(Trace):
    def __init__(self):
        return super().__init__()

    def initial_state(self):
        assert len(self._states) == 2
        return self[0]

    def end_state(self):
        assert len(self._states) == 2
        return self[1]

class TestCase:
    __slots__ = ('name', 'bytes', 'input', 'expected', '_result_generator',
                 'seed')

    def __init__(self, name_=None, bytes_=None, input_=None, expected_=None):
        self.name = name_
        self.bytes = bytes_
        self.input = input_
        self.expected = expected_
        self._result_generator = None
        self.seed = 42

    def as_json(self):
        obj = {}
        obj["id"] = 1
        obj["entries"] = [ self.input.get(), self.expected.get() ]

        if self.input.memory is not None:
            obj["initial_memory"] = self.input.memory.get()
        return obj


# Base class to define tests with -- unfortunately it already does a lot of custom options
# feel free to come with better decomposition
# Core method is the `case` method which adds the tests to be run. It has a lot of options,
# some of which can be "inherited" from the `Test` itself (to avoid code repetition).
class Test:
    __slots__ = ('name', 'cases', 'metafiles', '_bytes',
                 '_tags', '_names', '_lift_opts', '_inst_arr',
                 'default_istate', 'e_mutators', '_mode', 'rand_gen',
                 '_arch')

    def __init__(self, name_=""):
        # TODO(lukas): Split this into
        #                * inheritable attributes
        #                * helpers
        self.name = name_
        self.cases = []
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
        self._arch = "amd64"
        self.rand_gen = None



    def arch(self, trg):
        self._arch = trg
        return self

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

    def extra_lift_opts(self):
        return self._lift_opts + ['--arch', self._arch]

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
        destate = kwargs.get('DE', None)
        if destate is not None:
            self.e_mutators.append(destate)
            return self.default_istate.mutate(destate)
        else:
            self.e_mutators.append(MS())
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
        assert name_ not in [x.name for x in self.cases]
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

    def all_defined(self, **kwargs):
        for idx, bytes in enumerate(self._inst_arr):
            if kwargs.get('random', False):
                self.case(run_bytes = idx, I = populate_state(self.get_val_gen()),
                          R = True, **kwargs)
            else:
                self.case(run_bytes = idx, R = True, **kwargs)
        return self

    def seed(self, x):
        assert self.rand_gen is None
        self.rand_gen = random.Random(x)
        return self

    def get_val_gen(self):
        def val_gen(a, b):
            return self.rand_gen.randint(a, b)
        return val_gen

    def random(self, number, **kwargs):
        for _ in range(number):
            self.case(I = populate_state(self.get_val_gen()), **kwargs)
        return self

    def scases(self, compiler, insts, **kwargs):
        for x in insts:
            self.case(run_bytes = compiler([x]), **kwargs)
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
        for idx, case in enumerate(self.cases):
            case.bytes = check_bytes(case.bytes)
            case.input.bytes = check_bytes(case.input.bytes)
            if case._result_generator is not None:
                should, val = case._result_generator.generate(self, **kwargs)
                if should:
                     case.expected.result = val

    def resolve_undefs(self):
        for case in self.cases:
            for reg in case.expected.undefined:
                assert reg in case.input.registers
                case.expected.registers[reg] = case.input.registers[reg]


class TraceTest():
    class TestCase:
        __slots__ = ('_trace', '_result',
                     '_steps', '_maker', '_mutators',
                     '_tags',
                     '_initial_state', 'bytes',
                     'name')

        # traces, R
        def __init__(self, name_, **kwargs):
            self.name = name_
            self._result = kwargs.get('R', True)

            self._initial_state = kwargs.get('state', kwargs.get('inherited_state', None))

            self._trace = None
            self._maker = None
            self._steps = []
            self._mutators = {}

            self.bytes = ''

        def generate(self, **kwargs):
            self._initial_state.set_reg('RIP', 0x80045)
            mem, state = self._initial_state.memory, self._initial_state
            insts = self.fmt_insts()
            states = self._maker.execute(insts, state, mem)

            for x in states:
                x.bytes = insts.get(x.registers['RIP'], '00')

            for idx, mutator in self._mutators.items():
                states[idx].mutate(mutator)

            self._trace = Trace(states)

        def as_json(self):
            return self._trace.as_json()

        def fmt_insts(self):
            out = {}
            init = self._initial_state.registers["RIP"]

            for inst, addr in self._steps:
                if addr is not None:
                    init = addr
                compiled = inst.compile()[0]
                out[init] = compiled

                assert (len(compiled) % 2 == 0)
                init += len(compiled) // 2
            return out

        def add_maker(self, maker_):
            self._maker = maker_
            return self

        def add_mutators(self, mutators):
            for idx, mutator in mutators:
                self._mutators[idx] = mutator
            return self

        def add_steps(self, insts):
            for x in insts:
                if isinstance(x, tuple):
                    self._steps.append(x)
                else:
                    self._steps.append((x, None))
            return self


    __slots__ = ('cases', 'name', '_name_counter',
                 '_initial_state', '_name_counter', '_tags',
                 # Remove
                 'metafiles', 'lift_opts', 'arch', 'circuit'
                 )

    def __init__(self, name_, **kwargs):
        self.cases = []
        self.name = name_
        self._name_counter = 0

        self._tags = set()

        self._initial_state = kwargs.get('state', None)
        self.metafiles = {}
        self.lift_opts = []
        self.arch = 'amd64'

        self.circuit = None

    def __getitem__(self, idx): return self.cases[idx]
    def append(self, state):
        self.cases.append(state)
        return self

    def __len__(self): return len(self.cases)

    def make_case(self, *args, **kwargs):
        self._name_counter += 1
        kwargs['inherited_state'] = self._initial_state
        self.append(self.TestCase(str(self._name_counter), **kwargs))
        return self.cases[-1]

    def extra_lift_opts(self):
        return self.lift_opts + ['--arch', self.arch]


    def resolve_undefs(self):
        pass

    def cooked_circuit(self, path):
        raise Exception("Not implemented")

    def raw_circuit(self, input_bytes):
        self.circuit = RawCircuit(input_bytes)
        return self


    def generate(self, **kwargs):
        for test_case in self.cases:
            test_case.generate(**kwargs)

    def tags(self, tags_):
        self._tags.update(tags_)
        return self
