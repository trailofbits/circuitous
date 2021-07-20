# Copyright (c) 2021 Trail of Bits, Inc.

import itertools

class TGen:
  __slots__ = ('instructions')

  def __init__(self):
    self.instructions = []

  def __get__(self, idx): return self.instructions[idx]
  def __iter__(self, idx): return self.instructions[idx]

  def add(self, what):
    if isinstance(what, list):
      self.instructions += what
    else:
      self.instructions.append(what)

  def get(self):
    out = []
    for x in self.instructions:
      for y in x.get():
        out.append(y)
    return out

  def __str__(self):
    return '\n'.join(self.get())


class Inst:
  __slots__ = ('operands', 'opcode')

  def __init__(self, opcode_):
    self.opcode = opcode_
    self.operands = []

  def __get__(self, idx):
    return self.operands[idx]

  def __iter__(self):
    return self.operands.__iter__()

  def add(self, what):
    if isinstance(what, list):
      self.operands += what
    else:
      self.operands.append(what)
    return self

  def build(self, prod):
    out = self.opcode
    for idx, op in enumerate(prod):
      out += " " + op
      if idx != len(prod) - 1:
        out += ","
    return out

  def get(self):
    opts = [x.get() for x in self.operands]
    out = []
    for prod in itertools.product(*opts):
      out.append(self.build(prod))
    return out

  def __str__(self):
    return "\n".join(self.get())


class RegBase:
  __slots__ = ('size', 'reprs')

  def __init__(self, size_, reprs_):
    assert size_ in [64, 32, 16, 8, -8]
    self.size = size_
    assert reprs_
    self.reprs = reprs_

  def get(self):
    return [self._get(x) for x in self.reprs]

class OGPR(RegBase):
  BASE = ['AX', 'BX', 'CX', 'DX', 'SI', 'DI', 'BP', 'SP']
  MOD = { 64 : 'R', 32 : 'E', 16 : '', 8 : 'L', -8 : 'H' }

  def construct(self, what):
    return self.MOD[self.size] + what

  def _get(self, chosen):
    if self.size in [8, -8]:
      return chosen[0] + self.MOD[self.size]
    return self.construct(chosen)

  def any(size_):
    if size_ in [8, -8]:
      return OGPR(size_, [OGPR.BASE[0]])
    return OGPR(size_, [OGPR.BASE[0], OGPR.BASE[-1]])

class EGPR(RegBase):
  BASE = ['R8', 'R9', 'R10', 'R11', 'R12', 'R13', 'R14', 'R15']
  MOD = { 64 : '', 32 : 'd', 16 : 'w', 8 : 'b' }

  def construct(self, what):
    return what + self.MOD[self.size]

  def _get(self, chosen):
    return self.construct(chosen)

  def __str__(self):
    return "\n".join(self.get())

  def any(size_):
    return EGPR(size_, [EGPR.BASE[0]])

class NoRSP(OGPR):
  def any(size_):
    return NoRSP(size_, [OGPR.BASE[0]])


class GPR:
  __slots__ = ('egpr', 'ogpr')

  def __init__(self, size):
    self.ogpr = None
    self.egpr = None

    if size != -8:
      self.egpr = EGPR.any(size)
    self.ogpr = OGPR.any(size)

  def get(self):
    out = self.ogpr.get()
    if self.egpr is not None:
      out += self.egpr.get()
    return out

  def __str__(self):
    return "\n".join(self.get())

class Multiply:
  __slots__ = ('data')
  def __init__(self, data_):
    assert isinstance(data_, list)
    self.data = data_

  def get(self):
    out = list()
    for x in self.data:
      out += x.get()
    return out

class Imm:
  __slots__ = ('sizes', 'base_val')

  def __init__(self, size_):
    if isinstance(size_, int):
      self.sizes = [abs(size_)]
    else:
      self.sizes = list(size_)
    for x in self.sizes:
      assert x % 8 == 0 and x != 0

    self.base_val = "12"

  def get(self):
    for x in self.sizes:
      assert (x // 8) > 0
    return ["0x" + (x // 8) * self.base_val for x in self.sizes]

  def __str__(self):
    return "\n".join(self.get())

  def any():
    return Imm([8, 16, 32, 64])

  def max(size_):
    args = []
    for x in [8, 16, 32, 64]:
      if x <= size_:
        args.append(x)
    return Imm(x)

class Scalar:
  __slots__ = ('values')

  def __init__(self, v):
    if isinstance(v, int):
      self.values = [v]
    else:
      self.values = list(v)

  def get(self):
    return [str(x) for x in self.values]

  def __str__(self):
    return "\n".join(self.get())

  def any():
    return Scalar([0, 1, 2, 4, 8])



class Mem:
  __slots__ = ('size', 'base', 'index', 'displacement', 'scale')

  def __init__(self, size_, b, i, s, d):
    self.base = b
    self.index = i
    self.scale = s
    self.displacement = d
    self.size = size_

  def make(size_):
    return Mem(size_, GPR(size_), NoRSP.any(size_), Scalar.any(), Imm.max(size_))

  def simplified(size_):
    return Mem(size_, GPR(size_), NoRSP.any(size_), Scalar(4), Imm(size_))

  def any():
    return Multiply([Mem.simplified(32), Mem.simplified(64)])

  def construct(self, b, i, s, d):
    assert isinstance(s, str)
    assert isinstance(d, str)
    assert isinstance(i, str)
    assert isinstance(b, str)

    out = '['
    if b is not None and b:
      out += ' ' + b

    # At this point everything is string, since `get()` was called before
    if s != "0":
      if i is not None and i:
        out += ' + ' + i
      out += ' * ' + s
    out += " + " + d
    out += ' ]'
    return out

  def get(self):
    bs = self.base.get()
    idxs = self.index.get()
    scs = self.scale.get()
    disps = self.displacement.get()

    out = []
    for prod in itertools.product(bs, idxs, scs, disps):
      out.append(self.construct(*prod))
    return out

  def __str__(self):
    return "\n".join(self.get())

class MemoryExpr:

  SIZES = {
    8 : "BYTE PTR",
    16: "WORD PTR",
    32: "DWORD PTR",
    64: "QWORD PTR",
  }

  __slot__ = ('sizes', 'op')

  def __init__(self, sizes_, op_):
    self.sizes = sizes_
    self.op = op_

  def get(self):
    out = []
    for size in self.sizes:
      for gen_op in self.op.get():
        out.append(self.SIZES[size] + " " + gen_op)
    return out

class IDef:
  __slots__ = ('opcode', 'insts')

  def __init__(self, o_):
    self.opcode = o_
    self.insts = TGen()

  def add(self, inst):
    self.insts.add(inst)
    return self

  def all(self):
    for x in [64, 32, 16, 8, -8]:
      self.add(Inst(self.opcode).add(GPR(x)).add(GPR(x)))
      self.add(Inst(self.opcode).add(GPR(x)).add(Imm(x)))
      if x in [32, 64]:
        self.add(Inst(self.opcode).add(Mem.make(x)).add(GPR(x)))
    return self

  def iadd(self):
    for x in [64, 32, 16, 8, -8]:
      self.add(Inst(self.opcode).add(GPR(x)).add(GPR(x)))
      self.add(Inst(self.opcode).add(GPR(x)).add(Imm(min(x, 32))))
      if x in [32, 64]:
        self.add(Inst(self.opcode).add(Mem.make(x)).add(GPR(x)))
    return self

  def iform(self, ops):
    assert isinstance(ops, list)
    for x in itertools.product(*ops):
      self.insts.add(Inst(self.opcode).add(list(x)))
    return self

  def _apply(self, size, *what):
    def impl(fn):
      return fn(size)
    return tuple(map(impl, *what))


  def apply_uniform_iform(self, sizes, *ops):
    for size in sizes:
      self.iform(list(self._apply(size, *ops)))
    return self

  def get(self): return self.insts.get()
  def __str__(self): return self.insts.__str__()


def reg(size): return [GPR(size)]
def mem(size): return [MemoryExpr([size], Mem.any())]

def reg_mem(size): return [GPR(size), MemoryExpr([size], Mem.any())]

def imm(size, base_val = None):
  out = Imm(size)
  if base_val is not None:
    out.base_val = base_val
  return [out]

def compile(compiler, defs):
  unfolded = []
  for d in defs:
    unfolded += d.get()
  return compiler(unfolded)

def bs_(inst):
  return IDef(inst).iform([reg(16), reg(16)]) \
                   .iform([reg(32), reg_mem(32)]) \
                   .iform([reg(64), reg_mem(64)])

def bt():
  return IDef("bt").iform([reg_mem(16), reg(16)]) \
                   .iform([reg_mem(32), reg(32)]) \
                   .iform([reg_mem(64), reg(64)]) \
                   .iform([reg_mem(16), imm(8)]) \
                   .iform([reg_mem(32), imm(8)]) \
                   .iform([reg_mem(64), imm(8)])

def mov_extend(inst):
  return IDef(inst).iform([reg(16), reg_mem(8)]) \
                   .iform([reg(32), reg_mem(8)]) \
                   .iform([reg(64), reg_mem(8)]) \
                   .iform([reg(32), reg_mem(16)]) \
                   .iform([reg(64), reg_mem(16)])


def bt_(inst):
  return IDef(inst).iform([reg_mem(16), reg(16)]) \
                   .iform([reg_mem(32), reg(32)]) \
                   .iform([reg_mem(64), reg(64)]) \
                   .iform([reg_mem(16), imm(8, "09")]) \
                   .iform([reg_mem(32), imm(8, "09")]) \
                   .iform([reg_mem(64), imm(8, "09")])



def movzx(): return mov_extend("movzx")
def movsx(): return mov_extend("movsx")

def bsr(): return bs_("bsr")
def bsf(): return bs_("bsf")

def btc(): return bt_("btc")
def btr(): return bt_("btr")
def bts(): return bt_("bts")

def mov():
  out = TGen()
  for x in [64, 32, 16, 8, -8]:
    out.add(Inst('mov').add(GPR(x)).add(GPR(x)))
    out.add(Inst('mov').add(GPR(x)).add(Imm(x)))
    if x in [32, 64]:
      out.add(Inst('mov').add(Mem.make(x)).add(GPR(x)))
  return out.get()