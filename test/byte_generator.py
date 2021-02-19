# Copyright (c) 2021 Trail of Bits, Inc.

import os
import subprocess
import tempfile

# TODO(lukas): Cache this, and possibly rework using some faster way
#               -- maybe some external library can do this for us.
def intel(instructions):
  return get_instructions(instructions, "intel")

def att(instructions):
  return get_instructions(instructions, "att")

# What happens here:
# * Create tmp dir
# * Move there
# * Dump instruction into file
# * Use clang to get `.o`
# * Objdump the `.o`
# * Parse out the bytes
def get_instructions(instructions, syntax="intel"):
  with tempfile.TemporaryDirectory(os.getcwd()) as tmpdir:
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
def parse(filename):
  bytes = []
  args = ["objdump", "--disassemble", filename]
  pipes = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                           text=True)
  out, err = pipes.communicate()
  if pipes.returncode != 0:
    print(out)
    print(err)
    raise Exception("Something happened during objdump")
  section_data = out.split(".dummy_section:")[1]
  for line in section_data.split('\n'):
    # remove whitespaces first
    line = line.lstrip()
    if not line:
      continue
    print(line)
    # remove address in form `xxx:``
    line = line.split(': ')[1]
    # remove trailing assembly text
    line = line.split('  ')[0]
    # remove ' ' between bytes
    line = line.replace(' ', '')
    bytes.append(line)
  return bytes
