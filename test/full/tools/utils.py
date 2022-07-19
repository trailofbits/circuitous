# Copyright (c) 2022 Trail of Bits, Inc.

import json
import os
import subprocess

class CircuitousError(Exception):
    def __init__(self, component_, out_="", err_="", cause_=None):
        self.out = out_
        self.err = err_
        self.component = component_
        self.cause = cause_

    def fmt_cause(self):
        if self.cause is None:
            return 'Unknown'
        return self.cause

    def __str__(self):
        msg = 'Circuitous failed in: ' + self.component + '\n'
        msg += 'Cause: ' + self.fmt_cause() + '\n'
        msg += 'Stdout:\n' + self.out + '\n'
        msg += 'Stderr:\n' + self.err + '\n'
        return msg

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
