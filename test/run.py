# Copyright (c) 2021 Trail of Bits, Inc.

import argparse
import json
import os
import subprocess
import tempfile
import threading
import queue

from tc import State, Test
import tc as TC

import simple
import basic
import model_test as mp

circuitous_prefix="../build"
circuitous_run=os.path.abspath(os.path.join(circuitous_prefix, "circuitous-run"))
circuitous_lift=os.path.abspath(os.path.join(circuitous_prefix, "circuitous-lift"))
top_level_dir=None

dbg_verbose = False

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
    try:
      out, err = pipes.communicate(timeout = 45)
    except subprocess.TimeoutExpired:
      pipes.kill()
      out, err = pipes.communicate()
      raise PipelineFail(component, out, err, "Timeout")
    ret_code = pipes.returncode
    if ret_code != 0:
      raise PipelineFail(component, out, err, "Ret code != 0")

def strip(what):
  out = []
  for x in what:
    if x[0:2] == '--':
      out.append(x[2:])
    elif x[0:1] == '-':
      out.append(x[1:])
  return out


class PipelineFail(Exception):
  def __init__(self, component, out="", err="", cause=None):
    self._out = out
    self._err = err
    self._component = component
    self._cause = cause

  def __str__(self):
    msg = "Pipeline fail in: " + self._component + '\n'
    msg += "Cause: " + "Unknown" if self._cause is None else self._cause + '\n'
    msg += "Stdout:\n" + self._out + '\n'
    msg += '\n-----\n'
    msg += "Stderr:\n" + self._err + '\n'
    return msg


class SimulateCWDMixin():
  def locate(self, target):
    return os.path.join(self.test_dir, target)

class Lifter(SimulateCWDMixin):
  __slots__ = ('binary', 'test_dir')

  component = "circuitous lifter"

  def __init__(self, test_dir_, binary=circuitous_lift):
    self.binary = binary
    self.test_dir = test_dir_

  def lift_test(self, tc, extra_args):
    for case in tc.cases:
      bytes = case.bytes
      circuit = self.locate(self.circuit_name(bytes))
      log_info("Lifting: " + '.' * 16 + " " + tc.name + " -> " + case.name)
      if not circuit in tc.metafiles:
        tc.metafiles[circuit] = self.lift(case.bytes, tc._lift_opts + extra_args)
      else:
        log_info("\tSkipped")
      tc.metafiles[case.name + ".circuit"] = circuit

  def circuit_name(self, bytes):
    return bytes + ".circuit.ir"

  def lift(self, bytes, extra_args):
    args = [self.binary,
            "--bytes_in", bytes,
            "--json_out", self.locate("out.json"),
            "--ir_out", self.locate(self.circuit_name(bytes))]
    if dbg_verbose:
      args += ["--dot_out", self.locate(self.circuit_name(bytes) + ".dot")]
    args += extra_args
    pipes = subprocess.Popen(args,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True)
    check_retcode(pipes, self.component)

    return self.locate(self.circuit_name(bytes))

class Interpret(SimulateCWDMixin):
  __slots__ = ('binary', 'test_dir')

  component = "circuitous interpret"

  def __init__(self, test_dir_, binary=circuitous_run):
    self.binary = binary
    self.test_dir = test_dir_

  def run(self, tc):
    for case in tc.cases:
      self.run_case(case, tc.metafiles[case.name + ".circuit"], tc)

  def run_case(self, case, ir, parent):
    args = [self.binary,
            "--json_in", case.input.as_json_file(case.name, self.test_dir),
            "--json_out", self.locate(case.name + ".result.json"),
            "--ir_in", ir]
    if dbg_verbose:
      args += ["--dot_out", self.locate(case.name + ".result.dot")]
      parent.metafiles[case.name + ".result.dot"] = self.locate(case.name + ".result.dot")
    log_info("Running: " + '.' * 16 + " " + parent.name + " -> " + case.name)
    pipes = subprocess.Popen(args,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True)
    check_retcode(pipes, self.component)
    parent.metafiles[case.name + ".result.json"] = self.locate(case.name + ".result.json")

    self.update_state(case, self.locate(case.name + ".result.json"))

  def update_state(self, case, result):
    with open(result) as f:
      result = json.load(f)
      for reg, val in result["output_regs"].items():
        case.simulated.set_reg(reg, int(val, 10))
      case.simulated.result = result["result"]


class TestResult:
  ok = 0
  fail = 1
  error = 2

  __slots__ = ('status', 'msg', 'error_exception')

  def __init__(self, status_, msg_="", error_exception_=None):
    self.status = status_
    self.msg = msg_
    self.error_exception = error_exception_

  def __bool__(self):
    return self.status == self.ok

  # TODO(lukas): Want to get unpack, did not wanna bother with __iter__
  def as_tuple(self):
    return (self.status, self.msg, self.error_exception)

class Comparator(SimulateCWDMixin):
  def __init__(self, test_dir_):
    self.test_dir = test_dir_

  def compare(self, tc):
    out = {}
    for case in tc.cases:
      accept, msg = self.compare_case(case.input, case.simulated, case.expected)
      out[case.name] = TestResult(TestResult.ok if accept else TestResult.fail, msg)
      if not out[case.name]:
        msg = out[case.name].msg
        msg += "\n\tLift bytes: " + tc._bytes
        msg += "\n\tRun bytes: " + case.input.bytes
        # TODO(lukas): Not sure how to fix cleanly (threads broke it)
        if dbg_verbose:
          msg += "\n\t" + tc.metafiles[case.name + ".circuit"]
          msg += "\n\t" + tc.metafiles[case.name + ".result.json"]
          msg += "\n\t" + tc.metafiles[case.name + ".result.dot"]
        out[case.name].msg = msg
    return out

  def compare_case(self, input, after, expected):
    accept = True
    message = ""
    for reg, val in after.registers.items():
      if reg in expected.undefined or val is None:
        continue
      e_val = expected.registers.get(reg, input.registers.get(reg))
      if e_val != int(val):
        accept = False
        message += "Register " + reg + " (expected != actual): " + \
                   str(e_val) + " != " + str(val) + "\n"

    skipped = []
    for reg in expected.registers.keys():
      if reg not in after.registers  and reg not in input.registers.keys():
        accept = False
        skipped.append(reg)

    if skipped:
      message += "Registers that were expected but not present in result:\n["
    for reg in skipped:
        message += " " + reg
    if skipped:
      message += " ]\n"

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
  __slots__ = ('results', 'fails', 'fragile')

  def __init__(self, fragile_=False):
    self.results = {"pass" : 0, "fail" : 0, "error" : 0}
    self.fails = []
    self.fragile = fragile_

  def process(self, results, test_name):
    for name, result in results.items():
      verdict, message, e = result.as_tuple()
      full_name = test_name + " -> " + name
      if verdict == TestResult.fail:
        self.failed(full_name, message)
        self.results["fail"] += 1
      elif verdict == TestResult.ok:
        self.ok()
        self.results["pass"] += 1
      elif verdict == TestResult.error:
        self.results["error"] += 1
        print(yellow(test_name + ": Pipeline fail in: " + e._component))
        if dbg_verbose:
          print(e)


  def failed(self, name, message):
    print(red("[" + name + "]"), "\n", message)
    self.fails.append(name)

  def ok(self):
    pass

  def error(self, tc, e):
    if self.fragile:
      raise e
    self.results["error"] += len(tc.cases)
    print(yellow(tc.name + ": Pipeline fail in: " + e._component))


  def report(self):
    log_info("Results:")
    for x in self.fails:
      log_info("F: " + x)
    # TODO(lukas): Bleh, rework when we are certain the
    #              categories won't change much.
    for x in ["pass", "fail", "error"]:
      message = "\t" + x + " : " + str(self.results[x])
      if x == "fail" and self.results[x] != 0:
        message = red(message)
      if x == "pass" and self.results[x] == sum(self.results.values()):
        message = green(message)
      if x == "error" and self.results[x] != 0:
        message = yellow(message)
      print(message)


def execute_tests(tests, top_dir, extra_args, fragile, jobs):
  log_info("Test dir is: " + top_dir)
  os.chdir(top_dir)
  global top_level_dir
  top_level_dir = os.path.abspath(top_dir)

  results = queue.Queue()
  todo = queue.Queue()

  for x in tests:
    x.generate(lift=strip(extra_args))
    todo.put(x)

  def parallel_exec():
    while not todo.empty():
      x = None
      try:
        x = todo.get()
      except queue.Empty:
        return

      test_dir = tempfile.mkdtemp(dir=os.getcwd(),
                                  prefix=x.name.replace(' ', '.').replace('/', '.') + '_')
      test_dir = os.path.abspath(test_dir)
      #os.chdir(test_dir)
      try:
        Lifter(test_dir).lift_test(x, extra_args)
        Interpret(test_dir).run(x)
        r = Comparator(test_dir).compare(x)
        results.put((x.name, r))
      except PipelineFail as e:
        results.put((x.name, {"pipeline" : TestResult(TestResult.error, "", e)}))
        if fragile and jobs == 1:
          raise e

  threads = []
  for i in range(jobs):
    t = threading.Thread(target=parallel_exec)
    t.start()
    threads.append(t)
  for t in threads:
    t.join()



  rs = Results(fragile)
  while not results.empty():
    try:
      name, result = results.get()
    except queue.Empty:
      break
    rs.process(result, name)
  rs.report()


# TODO(lukas): Mocking this one for now
def fetch_test(sets):
  result = set()
  x = mp.ModelTest("mov imm rdx").bytes("ba12000000").case(I = State(), R = True )
  result.add(x)
  for x in simple.circuitous_tests:
    result.update(x)
  for x in basic.circuitous_tests:
    result.update(x)
  return result


def filter_by_tag(sets, tags):
  log_info("Filtering " + str(len(sets)) + " tests by " + str(tags))
  if 'all' in tags:
    return sets

  result = set()
  for test in sets:
    if test._tags.intersection(tags):
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
                           default=False)
  arg_parser.add_argument("--jobs",
                          help="Number of python threads. Should improve execution time.",
                          default=1)
  arg_parser.add_argument("--fragile",
                          help="If pipeline fails, kill & report",
                          action='store_true',
                          default=False)

  args, command_args = arg_parser.parse_known_args()

  global dbg_verbose
  dbg_verbose = args.dbg

  if args.tags is None:
    args.tags = ['all']
  args.jobs = int(args.jobs)

  tests = filter_by_tag(fetch_test(args.sets), args.tags)
  log_info("Filtered " + str(len(tests)) + " tests.")
  if args.persist:
    log_info("Creating persistent directory")
    test_dir = tempfile.mkdtemp(dir=os.getcwd())
    execute_tests(tests, test_dir, command_args, args.fragile, args.jobs)
  else:
    log_info("Creating temporary directory")
    with tempfile.TemporaryDirectory(dir=os.getcwd()) as tmpdir:
      execute_tests(tests, tmpdir, command_args, args.fragile, args.jobs)


if __name__ == "__main__":
  main()