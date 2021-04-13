# Overview

The following document tries to make navigation of the codebase easier by introducing
basic components.

To lift instructions into circIR the following pipeline is employed:
`BaseLifter<ILifter> -> ( CircuitBuilder -> Circuit0 -> Circuit1 ) -> Lowering -> Optimizations`

# Lifting

All lifting components share common `Context`.

`BaseLifter` is parametrized by type of `InstructionLifter` to use - it is expected it inherits
from `remill::InstructionLifter` (at least for now).
Job of this component is three-fold
  * Decode the instruction
  * Use `InstructionFuzzer` to determine which regions of the input bytes are responsible
    for operadns encoding
  * Lift instruction into LLVM IR via remill semantic functions.
  * Flatten control flow

`InstructionFuzzer` currently takes an instruction and flip all bytes:
e.g. if we are given:
1000 0110
we try
0000 0110
1100 0110
...
1000 0111

For each such permutation we compare the decoded `remill::Instruction` (if there is some valid
decoding) with the original. Comparison takes into account the expected difference; if we
expect that some region encodes register operand, then
`REG_64 RAX` is equal to `REG_64 RBX` but not equal to `REG_32 EAX`.
There are some other troublesome cases (such as regions encoding multiple operands or implicit
operands) -- currently we can handle Immediate and Register operands including some corner
cases.
Result is stored as `shadowinst::Instruction` which kind of mirrors the
`remill::Instruction` and tells for each operand regions & translation table.
Overall, this component is under development right now and can change.

Control flow flattener takes advantage of the fact that instructions are DAG and simply
introduces a path condition. Really simplified example:
```
if ( x )
  y = a
else
  z = b
```
we will simply modify the code to
```
y = ( x ) ? a : y
z = ( !x ) ? b : y
```
Overall we expect there to be no other memory present other than stores/loads into
`State`.

Instruction lifter has 2 specifics (for the operands we can fuzz)
  * Immediate: Is always wrapped in intrinsic call to avoid extra optimizations.
    If we have a region we instead use something like
    `%op = __circuitous.extract.0.8()`
  * Register: This is a bit more complicated as normally remill expects the registers
    to be known before. Basically what we do for read args is following
    ```
    %read1 = select( extract.8.16 == 010101, RAX, undef )
    %read2 = select( extract.8.16 == 010111, RBX, %read1 )
    ... until we reach `%readN` ...
    ```
    to mak sure we decoded exactly one operand we also do a xor on all the `select`
    condition operands.
    If the register is write situation is a bit more complex due to llvm optimizations
    but the base idea is similar:
    ```
    %readN = ... the tower of selects ...
    %dst = __circuitous.alloca.N() <- we need to have it as intrinsic call to avoid mem2reg
                                      killing it
    %dst <- %readN
    ```
    We also slap some metadata onto `%dst` so later parts of the pipeline can retrieve it and
    replace it with alloca (to be optimized away by `mem2reg` at the right time).

`CircuitBuilder` basically takes the flattened semantics functions and wraps them together
into one in form of:
```
def void @inst_1(%RAX, %RAX_next, %RBX, %RBX_next, ... for each "toplevel" registers... ) {
  alloca State
  State.RAX <- RAX
  State.RBX <- RBX
  ... call semantic ...
  RAX_after <- *State.RAX
  %RAX_valid = __circuitous.icmp.eq( RAX_after, RAX_next )
  ...
  __circutious.verify_inst( some decoding checks, %RAX_valid, RBX_valid, ...)
  ...
  later we also xor all `__circuitous.verify_inst` to make sure exactly one was accepted.
}
```
Some bits are omitted above - decoding checks are the `select` conditions we talked about
before + all regions of the instruction that are not associated with any operand.
After this some extra optimizations like eliminations of unused registers is done.

# Lowering

Lowering is actually pretty simple - we just traverse the bitcode and emit the cirIR
nodes depending on the instructions. Our intrinsics can of course be lowered as multiple
nodes.

# CircIR

CircIR is a really basic DAG - there is no magic happening. It can be traversed up/down
(uses/users). For more details consult the `IR.h` file.

# Tests

Basic architecture of the tests is following
`Lift -> Interpret -> Compare`
where `Compare` compares output of `Interpret` with expected result.
Expected result can come from two sources:
  * Hardcoded in the test
  * Partially (or fully) generated using `microx` (uses the host cpu).

The interesting cmd options are:
  * `--jobs` number of threads
  * `--tags` select tests only with some tag. By default `todo` tag is ignored (as usually
    these tests do not pass yet) -- to include those tests you can specify `--tags todo`.
    Another special value is `all` which selects all tests (excluding `todo`)
  * `--persist` keep the temporary directories -- handy if someone wants to investigate
    the results
  * `--dbg` extra output, ideally combined with `--persist`
  * `--fragile` end with first failure - only supported if `--jobs` is `1`
  * Any other extra arguments are forwarded to the lifter

Ideally, you want the following command to report no failures:
`--tags all --reduce_imms`

As for the structure of tests themselves, the most of the magic is abstracted away.
Class `ModelTest` represents the `microx` test, why only `Test` requires manual definition
of expected output. For more on options available consult `tc.py` or look into some
files with test definitions.

Overall, other architectures are not supported yet (it will require some config, but it is
definitely doable).
