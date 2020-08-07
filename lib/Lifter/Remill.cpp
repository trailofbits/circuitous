/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Remill.h>
#include <glog/logging.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

#include "CircuitBuilder.h"

namespace circuitous {

std::unique_ptr<Circuit> LiftInstructionsInFile(const std::string &arch_name,
                                                const std::string &os_name,
                                                const std::string &file_name) {

  auto maybe_buff = llvm::MemoryBuffer::getFile(file_name, -1, false);
  if (remill::IsError(maybe_buff)) {
    LOG(ERROR) << remill::GetErrorString(maybe_buff) << std::endl;
    return nullptr;
  }

  const auto buff = remill::GetReference(maybe_buff)->getBuffer();

  circuitous::CircuitBuilder builder([&](llvm::LLVMContext &context) {
    return remill::Arch::Build(&context, remill::GetOSName(os_name),
                               remill::GetArchName(arch_name));
  });

  return builder.Build(buff);
}

}  // namespace circuitous
