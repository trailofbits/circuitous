# Copyright (c) 2021 Trail of Bits, Inc.

import json
import os
import subprocess
import tempfile

class LazyBytes:
  __slots__ = ('instructions', 'generator')

  def __init__(self, instructions_, generator_):
    assert isinstance(instructions_, list)
    self.instructions = instructions_
    self.generator = generator_
    self.generator.queue(instructions_)

  def compile(self):
    return self.generator.compile(self.instructions)

  def extract(self, x):
    return LazyBytes([self.instructions[x]], self.generator)

  def __getitem__(self, x): return self.extract(x)
  def __len__(self): return len(self.instructions)

class Cache:
  def __init__(self):
    self.cache = {}
    self.verbose = False
    self._queue = []

  def compile(self, instructions):
    if not all(i in self.cache for i in instructions):
      self.update_cache(instructions)
    elif self.verbose:
      print("[ > ] No need to compile, already cached")
    return self.from_cache(instructions)

  def from_cache(self, instructions):
    result = []
    for i in instructions:
      result.append(self.cache[i])
    return result

  def update_cache(self, instructions):
    raw_insts = []
    for i in instructions + self._queue:
      if i not in self.cache:
        raw_insts.append(i)
    self._queue = []
    as_bytes = self._compile(raw_insts)
    for i in range(len(raw_insts)):
      self.cache[raw_insts[i]] = as_bytes[i]

  def queue(self, possible_insts):
    assert isinstance(possible_insts, list)
    self._queue += possible_insts



class FrozenCache():
  def glacier_path(self, path):
    return path

  def thaw_cache(self, path):
    if not os.path.isfile(self.glacier_path(path)):
      print("[ > ] Not thawing, glacier does not exist.")
      return
    with open(self.glacier_path(path)) as f:
      offline_cache = json.load(f)
      for inst, bytes in offline_cache["entries"].items():
        self.cache[inst] = bytes

  def freeze_cache(self, path):
    entries = {}
    for inst, bytes in self.cache.items():
      entries[inst] = bytes
    out = { "entries" : entries }

    with open(self.glacier_path(path), 'w') as f:
      json.dump(out, f)



class PersistentCache(FrozenCache, Cache):
  def _compile(self, instructions):
    return get_instructions(instructions, self.isa)

  def glacier_path(self, path):
    # TODO(lukas): Fix me
    assert path[-5:] == ".json"
    return path[:-5] + "." + self.isa + ".json"

class IntelCache(PersistentCache):
  isa = "intel"

class AttCache(PersistentCache):
  isa = "att"

_intel_cache = IntelCache()
_att_cache = AttCache()

all_gens = [_intel_cache, _att_cache]

# TODO(lukas): Cache this, and possibly rework using some faster way
#               -- maybe some external library can do this for us.
def intel(instructions):
  return LazyBytes(instructions, _intel_cache)

def att(instructions):
  return LazyBytes(instructions, _att_cache)

# What happens here:
# * Create tmp dir
# * Move there
# * Dump instruction into file
# * Use clang to get `.o`
# * Objdump the `.o`
# * Parse out the bytes
def get_instructions(instructions, syntax="intel"):
  print("[ > ] Generating instructions")
  with tempfile.TemporaryDirectory() as tmpdir:
    saved = os.path.abspath(os.getcwd())
    os.chdir(tmpdir)
    with open("insts.S", 'w') as out:
      out.write(".dummy_section:\n")
      for inst in instructions:
        out.write(inst + '\n')
    if syntax == "intel":
      asm_syntax = "-masm=intel"
    elif syntax == "att":
      asm_syntax = "-masm=att"
    else:
      raise Exception("Unrecognized assm syntax")
    args = ["clang", "-c", "-o", "insts.o", asm_syntax, "insts.S"]
    pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True)
    out, err = pipes.communicate()
    if pipes.returncode != 0:
      print(out)
      print(err)
      raise Exception("Something happened during compilation")
    as_bytes = parse("insts.o")
    os.chdir(saved)
    return as_bytes

# TODO(lukas): This is hacky at best. The way parsing is done is probably
#              not very portable.
def parse(filename, verbose=False):
  bytes = []
  args = ["objdump", "--disassemble", filename]
  pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                           text=True)
  out, err = pipes.communicate()
  if pipes.returncode != 0:
    print(out)
    print(err)
    raise Exception("Something happened during objdump")
  section_data = out.split("<.dummy_section>:")[1]
  for line in section_data.split('\n'):
    # remove whitespaces first
    line = line.lstrip()
    if not line:
      continue
    if verbose:
      print(line)
    # remove address in form `xxx:``
    line = line.split(': ')[1]
    # remove trailing assembly text
    line = line.split('\t')[0]
    # remove ' ' between bytes
    line = line.replace(' ', '')
    bytes.append(line)
  return bytes
