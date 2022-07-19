# Copyright (c) 2021 Trail of Bits, Inc.

from tools.model_test import ModelTest, MicroxGen
from tools.tc import Test, State, MS

class VerifyTest(Test):

    __slots__ = ('manual_gen')

    def case(self, name=None, **kwargs):
        super().case(name, **kwargs)

        self.manual_gen = kwargs.get('M', None) is not None
        return self

    def generate(self, **kwargs):
        super().generate(**kwargs)

        for idx, case in enumerate(self.cases):
            result = True if case.expected.result is None else case.expected.result
            if not self.manual_gen:
                try:
                    trace = MicroxGen().get(case.input.bytes, case.input, case.input.memory)
                    assert(len(trace) >= 2)
                    case.input = trace[0]
                    case.expected = trace[1]
                except Exception as e:
                    print("Microx fail in: " + self.name + " case " + case.name)
                    print("Bytes: " + case.input.bytes)
                    raise e
            case.expected.mem_hints = case.input.mem_hints
            case.expected = case.expected.mutate(self.e_mutators[idx])
            case.expected.result = result
        return self
