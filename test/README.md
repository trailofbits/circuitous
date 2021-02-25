# Overview

For instruction following happens:
 * Circuit is generated
 * It is run via `circuitous-run` with some inputs
 * Output state is compared to the same run via `microx`
Should provide some basic confidence that nothing broke and that
circuits probably work.

To do some testing of circuit transformations one promising way
may be to dump both pre-optimized and optimized to smtlib and then
let `z3` find out if they are the same.

# Prerequisites

Build the project. Build `microx`. If you are on macOS, `microx` may fail
to compile -- try to use patches in `test/patches` directory. (Via `git am`
for example).

Also, since `microx` does not provide any way to install itself somewhere
(or maybe correct me about how to do it), just symlink it here (do not forget the `.so`).

# How to run this

There is only one file that can be executed -- `run.py`
It has several options:
    * tags - you can specify which subset of tests to run (test has to have at least one of the tags)
    * fragile - stop on first lift error
    * persist - do not clean up the temporary dirs, for example if you want to examine something
    * anything else will be treated as extra arguments to be forwarded to `circuitous-lift`

Output tries to be self explanatory. At the moment there is no minimal set of tests that should pass, but it is being worked out and will be represented by `min` tag.
