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

from tools.utils import RunsPopen

class RunsPopen:

    def check_popen(self, pipes):
        try:
            out, err = pipes.communicate(timeout = 45)
        except subprocess.TimeoutExpired:
            pipes.kill()
            out, err = pipes.communicate()

            raise CircuitousError(self.__class__.__name__, out, err, 'Ttimeout')

        ret_code = pipes.returncode
        if ret_code != 0:
            raise CircuitousError(self.__class__.__name__, out, err, 'Ret code != 0')

# Circuit is already a valid file
class CookedCircuit:

    def __init__(self):
        pass

    def compile(self, name, where):
        pass

# Needs to be lifted
class RawCircuit(RunsPopen):
    __slots__ = ('insts')

    def __init__(self, instructions):
        self.insts = instructions

    def flattened(self):
        return "".join(self.insts.compile())

    def compile(self, name, where):
        circuit_path = os.path.abspath(os.path.join(where, name))

        # TODO(lukas): Normalize
        args = ['../../../build/circuitous-lift',
                '--log-dir', where,
                '--bytes-in', self.flattened(),
                '--ir-out', circuit_path,
                '--logtostderr']
        # TODO(lukas): Configurable
        args += ['--arch', 'amd64', '--os', 'macos']

        # TODO(lukas): Remove
        if True:
            args += ['--dbg']
        else:
            args += ['--quiet']

        pipes = subprocess.Popen(args,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 text=True)
        self.check_popen(pipes)
        return circuit_path
