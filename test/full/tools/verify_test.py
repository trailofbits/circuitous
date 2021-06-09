# Copyright (c) 2021 Trail of Bits, Inc.

from tools.model_test import ModelTest, MicroxGen
from tools.tc import Test, State, MS

class VerifyTest(Test):

  __slots__ = ('manual_gen')

  def case(self, name=None, **kwargs):
    super().case(name, **kwargs)
    self.e_mutators.append(kwargs.get('DE', MS()))

    self.manual_gen = kwargs.get('M', None) is not None

    return self

  def _expected_state(self, **kwargs):
    return State()

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
      case.expected = case.expected.mutate(self.e_mutators[idx])
      case.expected.result = result
      case.run_mode = '--verify'
      case.simulated.disarm()
    return self
