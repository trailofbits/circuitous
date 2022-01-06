# Copyright (c) 2021 Trail of Bits, Inc.

from tools.byte_generator import intel
from tools.model_test import ModelTest, Test
from tools.verify_test import VerifyTest
from tools.tc import State, S, MS, S32, MS32

def set_imul_undef(s):
    return s.uAF().uZF().uSF().uPF()

test_imul = {
    VerifyTest("imul-a_r32_r32").tags({"min", "imul"}).bytes(intel(["imul eax, ebx"]))
    .DI(S(0x100).RIP(0x10000).RAX(0xff4).RBX(0x5).RCX(0x21).RDX(0x121).aflags(0))
    .scases(intel, ["imul eax, ebx", "imul ecx, edx"],
            DE = MS().uAF().uZF().uSF().uPF(), R =  True)
    .scases(intel, ["imul r8d, ecx", "imul edx, r10d"],
            DE = MS().uAF().uZF().uSF().uPF(), R =  False),

    VerifyTest("imul-b").tags({"min", "imul"})
    .bytes(["f7eb"])
    .DI(S(0x5000).RAX(0x60).RBX(0xffffffffffffffff).RIP(0x6000))
    .case(run_bytes=["f7eb"], DE = MS().uAF().uZF().uSF().uPF(),  R=True)
}

test_x86_imul = {
    Test("x86_imul-a_all").tags({"tiny86", "imul"})
    .mode("--verify")
    .arch("x86")
    .bytes(["3ef6ee67666666f7ea666666f7ea6666f7eaf7ee0fafe9260fafff6bd5ff266bffff67f36bff8b6669f0ff8d66f269ff2d9c69d2ffffffff3e69ffff83f800f2f369ff8d05c3770faf450c0faf0f0fafb5ff8d05ca0fafabff89e0c70faf66ff0faf54ff8d0faf34ff0faf4424280faf448dd83e0faf3a0faf0cbd9cc404086b4510ff6bb0ffffc704246b4fffff6b37ff6b05feff83f8006b7cfdff316b14fcff6b04f7ff6b14f5ff8d8dbffe6b54f2ff836b8cecff31c08b4d6ba4eaff8d8d73fe6b9ce2ff31c08b4d6b24e2ff6b54e1ff83266bbaff8d8d8bfef36bacff31c08b8df0676ba9ff31c06b1ce508890424c7f36b0f856b54e508e86b84e508890424c7266b6d140f646b7c0f8515676b270f3e366b940f851a000000676b791b0f6b242443676b200ff26bbdff2df96a17676b9c0f851a676bb50f851af26b45020f676b5f0f852e6b0409896669a5ffc70424010066690f851564f266690f8515f266690f85156669610f851566696d0f8515666991ff2d1b6054b6666974ff3dd6a466691c610f856669bcff83f8000f8d0c69000000e90069aeffffc70424010000697effffc70424690dffff31c08b8dd4fe69b4feff0fb645f3890424695cfbff8945b0836945fbff83f8006904f8ffc70424698cecff8d8dd7fdffff89699ce7ff31c08b8d98feff691ce3ff8d9598695ce2ffc704241c696cddffc70424016934ddff8d8dc0fdffff89692cdcff83f8006769b7ff8b85b8fdff3e69afff83f8000f8d0c006769a1ff8b45988b08f2699cff8b45988338020f8364691108899534646964b50f851a000069a4e4890f851a000000e96934a50f851a000000e900646956ec0f851a006944e4ff8d05ed3367690f85150000676955340f851500f269bdff8d05bd96f1088967693cc50f851a676966c70f851a006769130f85150067696cff3d785f692e69340f85150000676960ff89e0c74067690509e892ee6769b5ff83f8000f8df62cfff629f66de2f6acff31c08b4d26f6a9ffc70424f6acf5ff89e0c7f62ced100f851567f62af6aca5ff8d053e3ef62d480f851567f66bff66f72cff67666666f72d666666f72d78fd04086666f72d78fd0408f7a8010f8505f76afdf76cf5fff72df5ff83f8f72cf4f76cf3fff72ce7f76ce3ff64f7acff83f8000f36f76ca80f67f72f67f7a8ff8d"])
    .DI(S32(0x5000).segments(0).EAX(0x60).EBX(0xffffffff).EIP(0x6000))
    .case(run_bytes=["f7eb"],
          DE = set_imul_undef(MS32()).EIP(0x6002).EAX(0xffffffa0).EDX(0xffffffff).ts(1),
          R=True),

    Test("x86_imul-a_all").tags({"tiny86", "imul"})
    .mode("--verify")
    .arch("x86")
    .bytes(["f7eb"])
    .DI(S32(0x5000).segments(0).EAX(0x60).EBX(0xffffffff).EIP(0x6000))
    .case(run_bytes=["f7eb"],
          DE = set_imul_undef(MS32()).EIP(0x6002).EAX(0xffffffa0).EDX(0xffffffff).ts(1),
          R=True)
}

circuitous_tests = [test_imul, test_x86_imul]
