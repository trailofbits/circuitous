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
                    case.expected = MicroxGen().get(case.input)
                except Exception as e:
                    print("Microx fail in: " + self.name + " case " + case.name)
                    print("Bytes: " + case.input.bytes)
                    raise e
            case.input.mem_hints = case.expected.mem_hints
            case.expected = case.expected.mutate(self.e_mutators[idx])
            case.expected.result = result
            case.run_mode = '--verify'
        return self
