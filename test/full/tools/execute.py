# Copyright (c) 2022 Trail of Bits, Inc.

import argparse
import json
import os
import subprocess
import tempfile
import threading
import queue
import sys
import re

from tools.utils import RunsPopen, CircuitousError
from tools.result import ResultValue

class BaseRunner(RunsPopen):
    __slots__ = ('test_dir', 'test_case', 'should_die', 'verbose')

    def __init__(self, test_dir_, test_case_, runner_, **kwargs):
        self.test_dir = test_dir_
        self.test_case = test_case_

        self.runner = runner_

        assert self.runner is not None
        assert os.path.exists(self.runner)

        self.should_die = kwargs.get('die', False)
        self.verbose = kwargs.get('verbose', False)

    def prefix(self):
        return "circ_test_case" + self.test_case.name

    def store_trace(self, data):
        fname = os.path.abspath(os.path.join(self.test_dir, self.prefix() + '.trace.json'))
        with open(fname, 'w') as out:
            json.dump(data, out, indent=4)
        return fname

    def run(self, circuit_path):
        result = os.path.join(self.test_dir, self.prefix() + '.result.json')

        args = [self.runner,
                '--verify',
                '--log-dir', self.test_dir,
                '--traces', self.store_trace(self.test_case.as_json()),
                '--export-derived', result,
                '--ir-in', circuit_path,
                '--logtostderr']

        if self.verbose:
            args += ['--dbg']

        if self.should_die:
            args += ['--die']

        pipes = subprocess.Popen(args,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 text=True)
        self.check_popen(pipes)
        return result

class BaseEvaluator():
    __slots__ = ('test_case')

    def __init__(self, test_case_):
        self.test_case = test_case_

    def load_json(self, path):
        with open(path) as f:
            return json.load(f)

    def parse_json(self, obj):
        result = obj['result']
        traces = obj['traces']
        return (result, traces)

    def do_memory_compare(self, state, trace):
        used = set()
        def as_tuple(what):
            out = []
            for x in ['used', 'addr', 'value', 'mode', 'size', 'ts']:
                out.append(int(what[x], 16))
            return tuple(out)

        for _, item in trace["memory_hints"].items():
            if item['used'] != '0':
                used.add(as_tuple(item))

        e = set()
        for item in state.mem_hints:
            e.add(item.as_tuple())

        msg =  'Expected ' + str(e) + '\n'
        msg += 'Got: ' + str(used) + '\n'

        return e == used, msg

    def compare_memory(self, traces):
        if len(self.test_case._trace) != len(traces) + 1:
            detail = str(len(self.test_case._trace)) + ' ' + str(len(traces))
            return ResultValue.make_error('Trace size missmatch ' + detail, None)

        acc = True
        poi = ""
        for i in range(len(traces)):
            v, msg = self.do_memory_compare(self.test_case._trace[i], traces[str(i)])
            if v:
                continue

            poi = msg
            acc = False


        if acc:
            return ResultValue.make_pass()

        return ResultValue.make_error("Memory hint missmatch", poi)

    def run(self, result_json):
        result, traces = self.parse_json(self.load_json(result_json))

        if result == 'error':
            return ResultValue.make_error('Result: Error', None)

        if result == 'reject':
            if self.test_case._result == False:
                return ResultValue.make_pass()
            else:
                return ResultValue.make_fail('Expected Accept, got Reject', {})

        # result must be 'pass' or some invariant is broken
        if result != 'accept':
            return ResultValue.make_error('Got unexpected result: ' + result, None)

        # Now only thing left is to compare mem_hints to what trace has in itself
        return self.compare_memory(traces)


class TestExecutor():
    runner = BaseRunner
    evaluator = BaseEvaluator

    __slots__ = ('test_dir')

    def __init__(self, test_dir_):
        self.test_dir = test_dir_

    def mk_name(self, test_def):
        # Since there is only one circuit per test, and it will always end with `.circuit`
        # there is no need for reasonable human-readable name.
        # Name itself is not used due to possible unusual symbols in test names.
        def normalize(what):
            return str(hash(what));

        return "circ_test." + normalize(test_def.name) + ".circuit"

    def get_runner(self, test_case, runner, **kwargs):
        # TODO(lukas): Parametrize by test case?
        return self.runner(self.test_dir, test_case, runner, **kwargs)

    def get_evaluator(self, test_case):
        return self.evaluator(test_case)

    def mk_pipeline_fail(self, test_def, e):
        results = {}
        for test_case in test_def.cases:
            results[test_case] = ResultValue.ERROR
        return (test_def, ResultValue.make_error('Fail', str(e)), {})


    # -> (dict of results, Optional message to be printed)
    def run(self, test_def, runner_binary, **kwargs):

        lift_dir = os.path.abspath(tempfile.mkdtemp(dir=self.test_dir, prefix='circuit'))
        try:
            circuit = test_def.circuit.compile(self.mk_name(test_def), lift_dir)
        except CircuitousError as e:
            return self.mk_pipeline_fail(test_def, e)

        results = {}
        for test_case in test_def.cases:
            try:
                result = self.get_runner(test_case, runner_binary, **kwargs).run(circuit)
            except CircuitousError as e:
                results[test_case] = ResultValue.make_error('circuitous-run error', str(e))
                continue
            results[test_case] = self.get_evaluator(test_case).run(result)

        return (test_def, None, results)

def run_multitrace(test_def, test_dir, runner_binary, **kwargs):
    return TestExecutor(test_dir).run(test_def, runner_binary, **kwargs)
