/*
 * Copyright (c) 2021-present, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>
#include <spdlog/cfg/env.h>
#include <spdlog/cfg/argv.h>


int main(int argc, char** argv) {
    spdlog::cfg::load_env_levels();
    spdlog::cfg::load_argv_levels(argc, argv);

    doctest::Context context;
    context.applyCommandLine(argc, argv);

    int res = context.run();

    if(context.shouldExit()) // important - query flags (and --exit) rely on the user doing this
        return res;          // propagate the result of the tests

    int client_stuff_return_code = 0;
    // your program - if the testing framework is integrated in your production code

    return res + client_stuff_return_code; // the result from doctest is propagated here as well
}
