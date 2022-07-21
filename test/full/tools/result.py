# Copyright (c) 2022 Trail of Bits, Inc.

import os
import sys

class ResultValue:
    PASS = 0
    FAIL = 1
    ERROR = 2

    ALL = [PASS, FAIL, ERROR]

    __slots__ = ('value', 'msg', 'dbg_msg')

    def __init__(self, v_, m_, dm_):
        assert (v_ in self.ALL)
        self.value = v_

        assert (v_ != self.PASS or (m_ is None and dm_ is None))
        self.msg = m_
        self.dbg_msg = dm_

    def make_error(msg_, dbg_=None):
        return ResultValue(ResultValue.ERROR, msg_, dbg_)

    def make_fail(msg_, dbg_=None):
        return ResultValue(ResultValue.FAIL, msg_, dbg_)

    def make_pass():
        return ResultValue(ResultValue.PASS, None, None)

    def value_to_string(val):
        if val == ResultValue.PASS:
            return 'Pass'
        if val == ResultValue.FAIL:
            return 'Fail'
        if val == ResultValue.ERROR:
            return 'Error'
        raise Exception()


    def __str__(self):
        out = ResultValue.value_to_string(self.value) + '\n'
        if self.msg is not None:
            out += 'Msg:\n' + self.msg + '\n'
        if self.dbg_msg is not None:
            out += 'Details:\n' + str(self.dbg_msg) + '\n'
        return out

class ResultAccumulator:
    __slots__ = ('total', 'def_count', 'case_count', 'counts')

    def __init__(self):
        self.total = {}
        self.counts = {}
        for x in ResultValue.ALL:
            self.counts[x] = 0

        self.def_count = 0
        self.case_count = 0

    def add(self, result):
        test_def, total_result, case_results = result

        self.def_count += 1
        self.case_count += len(test_def.cases)

        if total_result is not None:
            assert (total_result.value != ResultValue.PASS)
            self.total[test_def] = (total_result, {})
            self.counts[total_result.value] += len(test_def.cases)
            return

        self.total[test_def] = (None, {})
        for c, r in case_results.items():
            self.total[test_def][1][c] = r
            self.counts[r.value] += 1

        return self

    def overall(self, test_def, v):
        return test_def.name + ' has overall result: ' + str(v) + '\n'

    def results(self, verbose_level=1):
        msg = ''
        if verbose_level >= 1:
            for test_def, (v, case_value) in self.total.items():
                if v is not None:
                    msg += self.overall(test_def, v)
                    continue

                inner = ''
                for test_case, v in case_value.items():
                    if v.value == ResultValue.PASS:
                        continue

                    inner += test_case.name + ": " + str(v) + '\n'

                if len(inner) != 0:
                    msg += test_def.name + ':\n' + inner


        if verbose_level >= 0:
            msg += 'Results:\n | Cases ran: ' + str(self.case_count) + '\n'
            for v, c in self.counts.items():
                msg += '\t' + ResultValue.value_to_string(v) + ' : ' + str(c) + '\n'
        return msg

    def succeeded(self):
        if self.total == self.counts[ResultValue.PASS]:
            return 0
        return 1
