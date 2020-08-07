print(".intel_syntax noprefix")
regs = ["eax", "ebx", "ecx", "edx", "ebp", "esi", "edi", "esp"]

for reg1 in regs:
  #print("not {}".format(reg1))
  #print("neg {}".format(reg1))
  #print("inc {}".format(reg1))
  #print("dec {}".format(reg1))
  #print("jmp {}".format(reg1))
  #print("pop {}".format(reg1))
  for reg2 in regs:
    print("mov {}, {}".format(reg1, reg2))
    print("add {}, {}".format(reg1, reg2))
    print("sub {}, {}".format(reg1, reg2))
    print("and {}, {}".format(reg1, reg2))
    print("or {}, {}".format(reg1, reg2))
    print("xor {}, {}".format(reg1, reg2))
    #for reg3 in regs:
    #  print("lea {}, [{} + {}]".format(reg1, reg2, reg3))
    #  print("lea {}, [{} + {} * 2]".format(reg1, reg2, reg3))
    #  print("lea {}, [{} + {} * 4]".format(reg1, reg2, reg3))
