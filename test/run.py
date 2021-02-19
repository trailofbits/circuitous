# Copyright (c) 2021 Trail of Bits, Inc.

import argparse
import json
import os
import subprocess
import tempfile

from tc import State, Test
import tc as TC

import simple
import model_test as mp

circuitous_prefix="../build"
circuitous_run=os.path.abspath(os.path.join(circuitous_prefix, "circuitous-run"))
circuitous_lift=os.path.abspath(os.path.join(circuitous_prefix, "circuitous-lift"))
top_level_dir=None

class Colors:
  GREEN = '\033[92m'
  RED = '\033[91m'
  YELLOW = '\033[93m'
  RESET = '\033[0m'

def end_color():
    return Colors.RESET

def colorize(color, what):
    return color + what + end_color()

def green(what):
    return colorize(Colors.GREEN, what)

def red(what):
    return colorize(Colors.RED, what)

def yellow(what):
    return colorize(Colors.YELLOW, what)


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
      bytes = case.bytes
      circuit = self.circuit_name(bytes)
      log_info("Lifting: " + '.' * 16 + " " + tc.name + " -> " + case.name)
      if not circuit in tc.metafiles:
        tc.metafiles[circuit] = self.lift(case.bytes, tc._lift_opts)
      else:
        log_info("\tSkipped")
      tc.metafiles[case.name + ".circuit"] = circuit

  def circuit_name(self, bytes):
    return bytes + ".circuit.ir"

  def lift(self, bytes, extra_args):
    args = [self.binary,
            "--bytes_in", bytes,
            "--json_out", "out.json", "--ir_out", self.circuit_name(bytes)]
    args += extra_args
    pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    check_retcode(pipes, self.component)

    return self.circuit_name(bytes)

class Interpret:
  component = "circuitous interpret"

  def __init__(self, binary=circuitous_run):
    self.binary = binary

  def run(self, tc):
    for case in tc.cases:
      self.run_case(case, tc.metafiles[case.name + ".circuit"], tc)

  def run_case(self, case, ir, parent):
    args = [self.binary,
            "--json_in", case.input.as_json_file(),
            "--json_out", case.name + ".result.json",
            "--ir_in", ir]
    log_info("Running: " + '.' * 16 + " " + parent.name + " -> " + case.name)
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
      out[case.name] = self.compare_case(case.input, case.simulated, case.expected)
    return out

  def compare_case(self, input, after, expected):
    accept = True
    message = ""
    for reg, val in after.registers.items():
      e_val = expected.registers.get(reg, input.registers.get(reg))
      if e_val != int(val):
        accept = False
        message += "Register " + reg + " (expected != actual): " + \
                   str(expected.registers[reg]) + " != " + str(val) + "\n"

    if expected.result:
      if after.result == False or accept == False:
        if after.result == False:
          message += "Should accept, did not.\n"
        return False, message
    if not expected.result:
      if accept and after.result:
        return False, "Should reject, did not.\n"
    return True, ""

class Results:
  def __init__(self):
    self.results = {"pass" : 0, "fail" : 0, "error" : 0}

  def process(self, result, name):
    log_info("Test: " + name)
    for name, (verdict, message) in result.items():
      if not verdict:
        self.failed(name, message)
        self.results["fail"] += 1
      else:
        self.ok()
        self.results["pass"] += 1

  def failed(self, name, message):
    print("\t[", name, "]", message)

  def ok(self):
    pass

  def report(self):
    log_info("Results:")
    for x in ["pass", "fail", "error"]:
      print("\t", x, " : ", self.results[x])


def execute_tests(tests, top_dir):
  log_info("Test dir is: " + top_dir)
  os.chdir(top_dir)
  global top_level_dir
  top_level_dir = os.path.abspath(top_dir)

  for x in tests:
    test_dir = tempfile.mkdtemp(dir=os.getcwd(),
                                prefix=x.name.replace(' ', '.').replace('/', '.') + '_')
    os.chdir(test_dir)
    x.generate()

    Lifter().lift_test(x)
    Interpret().run(x)
    rs = Results()
    rs.process(Comparator().compare(x), x.name)
    rs.report()
    os.chdir(top_level_dir)


# TODO(lukas): Mocking this one for now
def fetch_test(sets):
  result = set()
  x = mp.ModelTest("mov imm rdx").bytes("ba12000000").case(I = State(), R = True )
  result.add(x)
  result.update(set(simple.mov))
  return result


def filter_by_tag(sets, tags):
  log_info("Filtering " + str(len(sets)) + " tests by " + str(tags))
  if 'all' in tags:
    return sets

  result = set()
  for test in tests:
    if test._tags.intersect(tags):
      result.add(test)
  return result

def main():
  arg_parser = argparse.ArgumentParser(
    formatter_class = argparse.RawDescriptionHelpFormatter)
  arg_parser.add_argument("--persist",
                          help="Runtime helper directories will not be cleaned.",
                          action='store_true',
                          default=False
                          )
  arg_parser.add_argument("--tags",
                          help="Specify which tags you want to run.",
                          action='extend',
                          nargs='+'
                          )
  arg_parser.add_argument("--sets",
                           help="TODO: Choose which test sets to consider.",
                           action='extend',
                           nargs='+')
  arg_parser.add_argument("--dbg",
                           help="TODO: Run in dbg mode, which means be verbose.",
                           action='store_true',
                           default=True)

  args, command_args = arg_parser.parse_known_args()
  if args.tags is None:
    args.tags = ['all']

  tests = filter_by_tag(fetch_test(args.sets), args.tags)
  if args.persist:
    log_info("Creating persistent directory")
    test_dir = tempfile.mkdtemp(dir=os.getcwd())
    execute_tests(tests, test_dir)
  else:
    log_info("Creating temporary directory")
    with tempfile.TemporaryDirectory(dir=os.getcwd()) as tmpdir:
      execute_tests(tests, tmpdir)

if __name__ == "__main__":
  main()