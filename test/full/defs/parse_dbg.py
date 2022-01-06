# Copyright (c) 2021 Trail of Bits, Inc.

import os

def load_dbg(trg):
    filename = os.path.join(os.path.join('defs', 'input_files'), trg)

    out = []
    with open(filename, 'r') as f:
        for line in f:
            prefix, _ = line.split(' ')
            out.append(prefix)
    return out
