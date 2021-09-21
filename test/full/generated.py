# Copyright (c) 2021 Trail of Bits, Inc.

from tools.tc import State, Test, S, MS, random_state, MemHint
from tools.byte_generator import intel
from tools.model_test import ModelTest
from tools.verify_test import VerifyTest
import tools.tgen as tgen

mov = {
  VerifyTest("gen all mov") \
  .bytes(intel(tgen.mov()))
  .tags({"mov", "generated"})
  .DI(S(0x1).RAX(0x0).aflags(0))
  .case(run_bytes = 0, R = True)
  .case(run_bytes = 1, R = True),
}

add = {
  VerifyTest("gen all add") \
  .bytes(intel(tgen.IDef("add").iadd().get()))
  .tags({"add", "generated", "gen_add"})
  .DI(S(0x41124))
  .all_defined(),

  VerifyTest("gen all add") \
  .bytes(["674401848012121212"])
  .tags({"add", "generated"})
  .DI(S(0x41124))
  .all_defined(),
}

leave = {
  VerifyTest("leave") \
  .bytes(intel(["leave"]))
  # NOTE(lukas): Not part of tiny86
  .tags({"leave", "todo"})
  .case(run_bytes = 0, I = random_state(42), R=True)
  .case(run_bytes = 0, I = random_state(4212), R=True)
  .case(run_bytes = 0, I = random_state(4213), R=True),
}

ret = {
  VerifyTest("ret") \
  .bytes(intel(["ret"]))
  .tags({"ret"})
  .case(run_bytes = 0, I = random_state(42), R=True)
  .case(run_bytes = 0, I = random_state(4212), R=True)
  .case(run_bytes = 0, I = random_state(4213), R=True),
}

nop = {
  VerifyTest("nop") \
  .bytes(intel(["nop"]))
  .tags({"nop"})
  .case(run_bytes = 0, I = random_state(42).ts(24), R=True)
  .case(run_bytes = 0, I = random_state(4212), R=True)
  .case(run_bytes = 0, I = random_state(4213).ts(4551), R=True),
}

neg = {
  VerifyTest("neg") \
  .bytes(intel(["neg rax"]))
  .tags({"neg"})
  .case(run_bytes = 0, I = random_state(42).ts(24), R=True)
  .case(run_bytes = intel(["neg rbx"]), I = random_state(4212), R=True)
  .case(run_bytes = intel(["neg rsp"]), I = random_state(4213).ts(4551), R=True),
}

big = {
  VerifyTest("gen all add") \
  .bytes(tgen.compile(intel, [tgen.IDef("add").iadd(), tgen.IDef("mov").all()]))
  .tags({"generated", "gen_big"})
  .DI(S(0x41124))
  .all_defined(),
}

lahf = {
  VerifyTest("lahf") \
  .bytes(intel(["lahf"]))
  .tags({"lahf"})
  .case(run_bytes = 0, I = random_state(42).ts(24), R=True)
  .case(run_bytes = 0, I = random_state(4212), R=True)
  .case(run_bytes = 0, I = random_state(4213).ts(4551), R=True),
}

test = {
  VerifyTest("gen all test") \
  .bytes(tgen.compile(intel, [tgen.IDef("test").iadd()]))
  .tags({"test", "generated"})
  .DI(S(0x1).RAX(0x0).aflags(0))
  .all_defined()
}

bsr = {
  VerifyTest("bsr") \
  .bytes(tgen.compile(intel, [tgen.bsr()]))
  .tags({"bsr", "generated"})
  .DI(S(0x1).RAX(0x0).aflags(0))
  .all_defined()
}

bt = {
  VerifyTest("bt") \
  .bytes(tgen.compile(intel, [tgen.bsr()]))
  .tags({"bt", "generated"})
  .DI(random_state(42))
  .all_defined()
}

flag_ops = {
  # NOTE(lukas): There is some microx related problem which causes sigsev
  Test("std").bytes(intel(["std"])).tags({"std", "min", "todo"})
  .DI(random_state(42))
  .case(run_bytes = 0, DE = MS().DF(1), DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DE = MS().DF(1), DI = MS().aflags(0), R=True),

  VerifyTest("stc").bytes(intel(["stc"])).tags({"stc", "min"})
  .DI(random_state(42))
  .case(run_bytes = 0, DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0), R=True),

  Test("cld").bytes(intel(["cld"]))
  .tags({"cld", "min"})
  .mode("--verify")
  .DI(random_state(42).RIP(0x98120))
  .case(run_bytes = 0, DI = MS().aflags(1),
        DE = MS().aflags(1).DF(0x0).RIP(0x98121).ts(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0),
        DE = MS().aflags(0).DF(0x0).RIP(0x98121).ts(1), R=True),

  VerifyTest("clc").bytes(intel(["clc"])).tags({"clc", "min"})
  .DI(random_state(42))
  .case(run_bytes = 0, DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0), R=True),

  VerifyTest("pushf").bytes(intel(["pushf"])).tags({"pushf", "min", "todo"}).seed(4212)
  .case(run_bytes = 0, I = S(0x2000).aflags(1), R=True)
  .random(5, run_bytes = 0, R = True),

  VerifyTest("popf").bytes(intel(["popf"])).tags({"popf", "min", "todo"}).seed(4212)
  .random(5, run_bytes = 0, R = True),

  # This is painful because microx
  VerifyTest("aflags").bytes(intel(["popf", "pushf", "clc", "cld", "stc"]))
  .tags({"aflags", "todo"})
  .all_defined(I = random_state(42))
  .all_defined(I = random_state(43).aflags(0))
  .all_defined(I = random_state(44).aflags(1))
}

movxzsx = {
  VerifyTest("movsx") \
  .bytes(tgen.compile(intel, [tgen.movsx()]))
  .tags({"movsx", "generated", "huge-movsx"}).seed(4123)
  .all_defined(random = True),

  VerifyTest("movsx") \
  .bytes(intel(["movsx rax, word ptr [rax + 2 * rcx + 0x2122]"]))
  .tags({"movsx", "generated", "small-movsx"}).seed(4123)
  .DI(S(0x4212).RCX(0x400).RIP(0xdcae6e9fb39cfd4d))
  .case(run_bytes = 0, R=True),

  VerifyTest("movzx") \
  .bytes(tgen.compile(intel, [tgen.movsx()]))
  .tags({"movzx", "generated", "huge-movzx"}).seed(4123)
  .DI(S(0x1).RAX(0x0).aflags(0))
  .all_defined(random=True),

  VerifyTest("movzx") \
  .bytes(intel(["movzx rax, word ptr [rax + 2 * rcx + 0x2122]"]))
  .tags({"movzx", "generated", "small-movzx"}).seed(4123)
  .DI(S(0x1).RAX(0x0).aflags(0))
  .all_defined(random=True),
}

cmc = {
  VerifyTest("cmc").bytes(intel(["cmc"])).tags({"cmc", "min"})
  .DI(random_state(42))
  .case(run_bytes = 0, DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0), R=True),
}

_ahf = {
  VerifyTest("sahf").bytes(intel(["sahf"])).tags({"sahf", "min"})
  .DI(random_state(42))
  .case(run_bytes = 0, DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0), R=True),

  VerifyTest("lahf").bytes(intel(["lahf"])).tags({"lahf", "min"})
  .DI(random_state(42))
  .case(run_bytes = 0, DI = MS().aflags(1), R=True)
  .case(run_bytes = 0, DI = MS().aflags(0), R=True),
}

bs = {
  VerifyTest("bsr") \
  .bytes(tgen.compile(intel, [tgen.bsr()]))
  .tags({"generated", "huge-bsr"}).seed(4123)
  .all_defined(random=True),

  VerifyTest("bsf") \
  .bytes(tgen.compile(intel, [tgen.bsf()]))
  .tags({"generated", "huge-bsf"}).seed(4123)
  .all_defined(random=True),
}

ret = {
  VerifyTest("ret").bytes(intel(["ret"])).tags({"ret", "min"})
  .seed(4124)
  .random(5, run_bytes = 0, R = True),

  VerifyTest("ret imm").bytes(intel(["ret 0x12"])).tags({"ret", "min"})
  .DI(random_state(4124))
  .scases(intel, ["ret 0x14", "ret 0xff", "ret 0x0"], R = True),
}

#TODO(lukas): Test some modulos
bt_src = {
  VerifyTest("bts") \
  .bytes(tgen.compile(intel, [tgen.bts()]))
  .tags({"generated", "huge-bts"}).seed(4123)
  .all_defined(random=True),

  VerifyTest("btr") \
  .bytes(tgen.compile(intel, [tgen.btr()]))
  .tags({"generated", "huge-btr"}).seed(4123)
  .all_defined(random=True),

  VerifyTest("btc") \
  .bytes(tgen.compile(intel, [tgen.btc()]))
  .tags({"generated", "huge-btc"}).seed(4123)
  .all_defined(random=True, DE=MS().uOF().uSF().uZF().uAF().uPF()),

  VerifyTest("btc-small").tags({"btc", "64b", "min"})
  .bytes(["67660fbb848012121212"]).seed(4123)
  .all_defined(random=True, DE=MS().uOF().uSF().uZF().uAF().uPF()),

  VerifyTest("bts-s_z_ext_lifter") \
  .bytes(intel(["bts QWORD PTR [rax + 2 *rsi + 0xff], 0xf2"]))
  .tags({"bts", "min"}).seed(4123)
  .DI(random_state(4123))
  .case(run_bytes = 0, DE=MS().uOF().uSF().uAF().uPF(), R=True),

  # NOTE(lukas): Microx has bug when immediate is grater than size
  Test("bts_corner_big_imm") \
  .bytes(intel(["bts WORD PTR [r8d + 2 * eax + 0x50], 0x20"]))
  .tags({"bts", "min"})
  .mode("--verify")
  .DI(S(0x3040).RIP(0x8012).R8(0x2100).RAX(0x380).aflags(0).ts(0)
               .rwmem(0x2800, "11" * 0x100)
               .rwmem(0x3800, "00" * 0x100))
  .case(run_bytes = 0,
        DE = MS().CF(1).ts(1).RIP(0x8012 + 9)
                 .mem_hint(MemHint.read(0x2854, 0x1111, 2))
                 .mem_hint(MemHint.write(0x2854, 0x1111, 2)),
        R=True)
  .case(run_bytes = intel(["bts WORD PTR [r8d + 2 * eax + 0x50], 0x21"]),
        DI = MS().RAX(0xb80),
        DE = MS().RAX(0xb80).CF(0).ts(1).RIP(0x8012 + 9)
                 .mem_hint(MemHint.read(0x3854, 0x0000, 2))
                 .mem_hint(MemHint.write(0x3854, 0b10, 2)),
        R=True),

  # NOTE(lukas): Microx has bug when immediate is grater than size
  Test("bts_corner_big_imm") \
  .bytes(intel(["bts WORD PTR [r8d + 2 * eax + 0x50], 0x20"]))
  .tags({"bts", "min"})
  .mode("--verify")
  .DI(S(0x3040).RIP(0x8012).R8(0x2100).RAX(0x380).aflags(0)
               .rwmem(0x2800, "11" * 0x100).ts(0))
  .case(run_bytes = 0,
        DE = MS().CF(1).ts(1).RIP(0x8012 + 9)
                 .mem_hint(MemHint.read(0x2854, 0x1111, 2))
                 .mem_hint(MemHint.write(0x2854, 0x1111, 2)),
        R=True),
}

# To verify that cirucit fails is memory hints are incorrect
memhint_fails = {
  Test("mov_incorrent_memhint") \
  .bytes(intel(["mov QWORD PTR [rax], 0x20"]))
  .tags({"mov", "min", "fail"})
  .mode("--verify")
  .DI(S(0x3040).RIP(0x8012).R8(0x2100).RAX(0x30080).aflags(0)
               .rwmem(0x300500, "11" * 0x100).ts(0))
  .case(run_bytes = 0,
        DE = MS().RIP(0x8012 + 7)
                 .mem_hint(MemHint.write(0x2854, 0x20, 8)),
        R=False)
  .case(run_bytes = 0,
        DE = MS().RIP(0x8012 + 7)
                 .mem_hint(MemHint.write(0x300080, 0x21, 8)),
        R=False),
}

tiny86 = {
  VerifyTest("tiny86")
  .tags({"giant", "tiny86", "todo"})
  .bytes(tgen.compile(intel, [tgen.btc(), tgen.btr(), tgen.bts(),
                              tgen.IDef("add").iadd(), tgen.IDef("mov").all(),
                              tgen.IDef("test").iadd(), tgen.bsr(), tgen.movsx(),
                              tgen.movzx()]))
  .seed(8312)
  .all_defined(random=True)
}

test_rip_split = {
  VerifyTest("test_rip_split").tags({"min", "selftest"})
  .bytes(intel(["mov [rax + 2 * r8 + 0xff], rdx"]))
  .DI(random_state(431).RIP(0x9ffe))
  .all_defined()
}

circuitous_tests = [mov, add, big, leave, nop, neg, ret, lahf, bsr, bt, flag_ops,
  movxzsx, cmc, bs, _ahf, ret, bt_src, tiny86, test_rip_split
]
