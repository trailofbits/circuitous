import operator
import os.path
import subprocess
from functools import reduce
import math
from pathlib import Path
import shutil

circuitous_prefix = "@PROJECT_BINARY_DIR@/bin"
circuitous_decoder = os.path.abspath(
    os.path.join(circuitous_prefix, "decoder/circuitous-decoder"))
decoder_test_runner = "decoder/decoder_tester.cpp"

output = "tmp/"
output_dir = Path(output)
if output_dir.exists():
    if output_dir.is_dir():
        shutil.rmtree(output)
    else:
        shutil.rm(output)
Path(output).mkdir(parents=True, exist_ok=True)


# Decoders for circuitous are in essence a set of checks between instr_bits and constant values in the DecoderResult subtree.
# We assume that any value bit that is not explicitly checked for in a DecodeCondition can be freely chosen.
#
# The way testing works is that we take an encoding of an input instruction, and test if the decoder:
# 1. returns true only if all checked bits are equal to the constants
# 2. accepts all encodings where a bit is flipped which can be freely chosen
#
# As the amount of test cases can get pretty big for a single instruction, we opt to only check if flipping a single bit from the original encoding causes a change in output.
# We do this for every possible bit in the original encoding.
#
#
# Generated test files are organized in the following manner:
# ```
# <list of encodings which should be accepted>
# \---
# <list of encodings which should be rejected>
# ```
# The list of encodings should have a new line per encoding. Encodings are hex values in plain text.
# Decoders for circuitous are in essence a set of checks between instr_bits and constant values in the DecoderResult subtree.
# We assume that any value bit that is not explicitly checked for in a DecodeCondition can be freely chosen.
#
# The way testing works is that we take an encoding of an input instruction, and test if the decoder:
#     1. returns true only if all checked bits are equal to the constants
#     2. accepts all encodings where a bit is flipped which can be freely chosen
#
# As the amount of test cases can get pretty big for a single instruction, we opt to only check if flipping a single bit from the original encoding causes a change in output.
# We do this for every possible bit in the original encoding.
#
#
# Generated test files are organized in the following manner:
# ```
# <list of encodings which should be accepted>
# \---
# <list of encodings which should be rejected>
# ```
# The list of encodings should have a new line per encoding. Encodings are hex values in plain text.

def toggle_bit(value, bit_index):
    val_cpy = value.copy()
    val_cpy[math.floor(bit_index / 8)] = val_cpy[math.floor(bit_index / 8)] ^ (
                1 << (bit_index % 8))
    return val_cpy


def in_a_decode_condition(dcs, bit):
    for dc in dcs:
        if bit in range(dc[0], dc[1]):
            return True
    return False


def print_hex_bytes(byte_arr):
    h = ""
    for b in byte_arr:
        h = h + " " + hex(b)

    return str(h)


def create_test_for_enc(enc, dc):
    flip_accept = []
    flip_deny = []

    for i in range(0, len(enc) * 8):
        if in_a_decode_condition(dc, i):
            flip_deny.append(i)
        else:
            flip_accept.append(i);

    return flip_accept, flip_deny


def concat_list(byt):
    s = ""
    for b in byt:
        s = s + b
    return s


def bytes_to_str_without_leading_0x(byt):
    s = ""
    for b in byt:
        s = s + f'{b:x}'
    return s


def create_input_at_file(test_file, test_enc_conditions):
    accept, deny = [], []

    for enc, dec_conditions in test_enc_conditions:
        a, d = create_test_for_enc(enc, dec_conditions)
        accept.append(enc)
        for offset in a:
            accept.append(toggle_bit(enc, offset))
        for offset in d:
            deny.append(toggle_bit(enc, offset))

    with open(test_file, 'w') as tf:
        for enc in accept:
            tf.write(print_hex_bytes(enc) + "\n")
        tf.write("---\n")
        for enc in deny:
            tf.write(print_hex_bytes(enc) + "\n")


# Expects output file name, and a list of ([<instr_encoding_bytes>],
#                                     [ (<offsets from extract node of the decode_condition>)] )
def run_test(test_name, test_enc_conditions):
    output_file_name = output + test_name
    bytes_in = reduce(lambda str, enc_cond: str + bytes_to_str_without_leading_0x(enc_cond[0]),
                      test_enc_conditions, "")
    print(subprocess.run(
        [circuitous_decoder, "--arch", "amd64", "--os", "linux", "--bytes-in", bytes_in,
         "--dec-out", output_file_name + ".cpp"]))
    print(subprocess.run(
        ["clang++", output_file_name + ".cpp", "-c", "-o", output_file_name + ".o"],
        capture_output=True))
    print(subprocess.run(
        ["clang++", decoder_test_runner, output_file_name + ".o", "-o", output_file_name],
        capture_output=True))

    test_file = output_file_name + ".input"
    create_input_at_file(test_file, test_enc_conditions)

    res = subprocess.run(["./" + output_file_name, test_file], capture_output=True)
    if res.returncode != 0:
        print(res.stdout)
        print(res.stderr)
    return res.returncode


def test_full_single_byte_compare():
    res = run_test("full_byte", [([0x90], [(0, 8)])])
    assert res == 1


def test_decoder_condtion_over_three_bytes():
    # Test covers:
    # single byte test with lsb and msb padding
    # multi-byte test without msb padding
    # multi-byte test with lsb padding
    # across byte boundary
    res = run_test("full_three_bytes", [([0x48, 0x8b, 0xc0], [(1, 2), (3, 16), (22, 24)])])
    assert res == 1


def test_two_different_instructions():
    res = run_test("test_two_different_instructions",
                   [([0x90], [(0, 8)]), ([0x48, 0x8b, 0xc0], [(1, 2), (3, 16), (22, 24)])])
    assert res == 1
