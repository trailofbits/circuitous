# Copyright (c) 2021 Trail of Bits, Inc.

import argparse
import json
import microx
import os
import subprocess
import tempfile

from tc import State, Test
import tc as TC

circuitous_prefix="../../build"
circuitous_run=os.path.join(circuitous_prefix, "circuitous-run")
circuitous_lift=os.path.join(circuitous_prefix, "circuitous-lift")
top_level_dir=None

def update_relative_paths():
  pass

def log_error(what):
  print("[ ! ]", what)

def log_info(what):
  print("[ > ]", what)

def check_retcode(pipes, component : str):
    out, err = pipes.communicate()
    ret_code = pipes.returncode
    if ret_code != 0:
      err_message = "Unexpected return value of " + component + ": " + str(ret_code)
      log_error(err_message)
      log_error("Dump of stdout:")
      print(out)
      log_error("Dump of stderr:")
      print(err)
      raise PipelineFail(err_message)

class PipelineFail(Exception):
  pass

class Lifter:
  component = "circuitous lifter"

  def __init__(self, binary=circuitous_lift):
    self.binary = binary

  def lift_test(self, tc):
    for case in tc.cases:
      bytes = case.input.bits
      print(type(bytes))
      circuit = self.circuit_name(bytes)
      if not circuit in tc.metafiles:
        circuit = self.lift(case.input.bits)
      tc.metafiles[case.name + ".circuit"] = circuit

  def circuit_name(self, bytes):
    print(type(bytes))
    return bytes + ".circuit.ir"

  def lift(self, bytes):
    args = [self.binary,
            "--bytes_in", bytes,
            "--json_out", "out.json", "--ir_out", self.circuit_name(bytes)]
    pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    check_retcode(pipes, self.component)

    return self.circuit_name(bytes)

class Interpret:
  component = "circuitous interpret"

  def __init__(self, binary=circuitous_run):
    self.binary = binary

  def run(self, tc):
    for case in tc.cases:
      self.run_case(case, tc.metafiles[case.name + ".circuit"])

  def run_case(self, case, ir):
    args = [self.binary,
            "--json_in", case.input.as_json_file(),
            "--json_out", case.name + ".result.json",
            "--ir_in", ir]
    pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    check_retcode(pipes, self.component)

    self.update_state(case, case.name + ".result.json")

  def update_state(self, case, result):
    with open(result) as f:
      result = json.load(f)
      for reg, val in result["output_regs"].items():
        case.simulated.set_reg(reg, val)
      case.simulated.result = result["result"]

class Model:
  pass

class Comparator:
  def __init__(self):
    pass

  def compare(self, tc):
    out = {}
    for case in tc.cases:
      out[case.name] = self.compare_case(case.simulated, case.expected)
    return out

  def compare_case(self, after, expected):
    if expected.result != after.result:
      return False, \
             "Verdict is incorrect, expected: " + str(expected.result) + \
              " got " + str(after.result)

    accept = True
    message = ""
    for reg, val in after.registers.items():
      if expected.registers[reg] != int(val):
        accept = False
        message += "Register " + reg + " (expected != actual): " + \
                   str(expected.registers[reg]) + " != " + str(val) + "\n"
    return accept, message

class Results:
  def __init__(self):
    self.results = {"pass" : 0, "fail" : 0, "error" : 0}

  def process(self, result):
    for name, (verdict, message) in result.items():
      log_info("TC: " + name)
      if not verdict:
        self.failed(message)
        self.results["fail"] += 1
      else:
        self.ok()
        self.results["pass"] += 1

  def failed(self, message):
    print("\t[ - ]", message)

  def ok(self):
    pass

  def report(self):
    log_info("Results:")
    for x in ["pass", "fail", "error"]:
      print("\t", x, " : ", self.results[x])

def main():
  arg_parser = argparse.ArgumentParser(
    formatter_class = argparse.RawDescriptionHelpFormatter)
  arg_parser.add_argument("--bytes", help="Something Something", required=True)
  args, command_args = arg_parser.parse_known_args()

  print("Supplied bytes:", args.bytes)
  test_dir = tempfile.mkdtemp(dir=os.getcwd())
  log_info("Test dir is: " + test_dir)
  os.chdir(test_dir)
  log_info("Entering: " + test_dir)
  global top_level_dir
  top_level_dir = test_dir

  Lifter().lift_test(TC.x)
  Interpret().run(TC.x)

  rs = Results()
  rs.process(Comparator().compare(TC.x))
  rs.report()

if __name__ == "__main__":
  main()