# Copyright (c) 2021-2022 Trail of Bits, Inc.

import copy
import math

from tools.tc import State, Test, _regs, _aflags, MS, MemHint, MemInput, MIG

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
                case.expected = MicroxGen().get(case.input.bytes, case.input, case.input.memory).mutate(self.e_mutators[idx])
            except Exception as e:
                print("Microx fail in: " + self.name + " case " + case.name)
                print("Bytes: " + case.input.bytes)
                raise e
            case.expected.result = result
        return self

def mem_input_to_map(region, o, m):
    addr, bytes, (r, w, e) = region
    assert len(bytes) <= 0x1000
    aligned_addr = (addr >> 12) << 12
    if addr >> 12 not in m._memory_maps:
        amm = microx.ArrayMemoryMap(o, aligned_addr, aligned_addr + 0x1000,
                                    can_read=r, can_write=w, can_execute=e)
        m.add_map(amm)
    else:
        amm = m._memory_maps[addr >> 12]
    amm.store_bytes(addr, bytes)

class PermissiveMemory(microx.Memory):

    def __init__(self, ops, address_size, seed_, page_shift=12):
        super().__init__(ops, address_size, page_shift)
        self._memory_maps = {}
        self.seed = seed_
        self.additional_inputs = MIG(self.seed)

    def _find_map(self, byte_addr):
        offset = (byte_addr & self._address_mask) >> self._page_shift
        if offset not in self._memory_maps:
            aligned_addr = (byte_addr >> 12) << 12
            # Generate values for this unalocated memory
            self.additional_inputs.mem_(aligned_addr, 0x1000, r=True, w=True, e=True)
            # Store generated values as microx memory
            mem_input_to_map(self.additional_inputs.generated.entries[-1], self._ops, self)

        return self._memory_maps[offset]

    def add_map(self, nmap):
        if nmap.base() in self._memory_maps:
            return
        return super().add_map(nmap)


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

    def convert_input_bytes(self, inst):
        out = []
        for i in range(0, len(inst), 2):
            out.append(int(inst[i] + inst[i + 1], 16))
        return out

    def store_code(self, o, m, insts, size):
        for addr, inst in insts.items():
            self.store_inst(o, m, addr, inst, size)


    def store_inst(self, o, m, addr, inst, size):
        bytes = self.convert_input_bytes(inst)

        breakpoint = size - (addr % size)
        prefix = bytes[0 : breakpoint]
        postfix = bytes[breakpoint:]

        mem_input_to_map((addr, prefix, (True, True, True)), o, m)
        if len(postfix) != 0:
            mem_input_to_map((addr + len(prefix) , postfix, (True, True, True)), o, m)


    def error_transition(self, state):
        out = copy.deepcopy(state)
        out._ebit = True
        out.timestamp += 1
        return out

    def get(self, insts, state, memory):
        rip = state.registers.get('RIP', None)
        assert(rip is not None)
        return self.execute({ rip : insts }, state, memory)

    def process_step(self, old, thread, process):
        new = State()

        new.timestamp = old.timestamp + 1

        for mem_hint in process._mem_hints:
            mem_hint.ts = old.timestamp
            old.mem_hint(mem_hint)

        for reg in _regs + _aflags:
            if reg not in ["DF"]:
                new.set_reg(reg, thread.read_register(reg, thread.REG_HINT_NONE))
            else:
                new.set_reg(reg, old.registers[reg])

        new.ebit(False)
        return new


    def execute(self, insts, state, memory):
        # TODO(lukas): We need to handle RSP once we support memory ops.
        # assert "RSP" not in state.registers

        o = microx.Operations()
        m = PermissiveMemory(o, 64, 42)

        size = 0x1000
        self.store_code(o, m, insts, size)

        #TODO(lukas): Stack & other memory
        if memory is not None:
            for region in memory.entries:
                addr, bytes, (r, w, e) = region
                assert len(bytes) <= size
                aligned_addr = (addr >> 12) << 12
                amm = microx.ArrayMemoryMap(o, aligned_addr, aligned_addr + size,
                                            can_read=r, can_write=w, can_execute=e)
                m.add_map(amm)
                amm.store_bytes(addr, bytes)

        t = microx.EmptyThread(o)


        e = Process_(o, m)

        states = [state]

        for _ in range(len(insts)):
            e._mem_hints = []
            for reg, val in states[-1].registers.items():
                # Bugs in microx
                if reg in ["DF"]:
                    continue
                if val is not None:
                    t.write_register(reg, val)
            if states[-1]._ebit:
                states.append(self.error_transition(states[-1]))
                continue
            try:
                e.execute(t, 1)
                states.append(self.process_step(states[-1], t, e))
                state.mig(m.additional_inputs)
            except FloatingPointError as e:
                states.append(self.error_transition(states[-1]))

        return states

def microx_gen():
    return MicroxGen()
