# # to i64
[arithmetic:mul-extensions]

# mul
extend-mul-i8-to-i64:
  - ((let Mul (op_Mul:8 ?a ?b)) (commutative-match $Mul))
  - ((let Ext (op_Trunc:8 (op_Mul:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

extend-mul-i16-to-i64:
  - ((let Mul (op_Mul:16 ?a ?b)) (commutative-match $Mul))
  - ((let Ext (op_Trunc:16 (op_Mul:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

extend-mul-i32-to-i64:
  - ((let Mul (op_Mul:32 ?a ?b)) (commutative-match $Mul))"
  - ((let Ext (op_Trunc:32 (op_Mul:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# # add
# extend-add-i8-to-i64:
#   - ((let Mul (op_Add:8 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:8 (op_Add:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-add-i16-to-i64:
#   - ((let Mul (op_Add:16 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:16 (op_Add:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-add-i32-to-i64:
#   - ((let Mul (op_Add:32 ?a ?b)) (commutative-match $Mul))"
#   - ((let Ext (op_Trunc:32 (op_Add:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# # sub
# extend-sub-i8-to-i64:
#   - ((let Mul (op_Sub:8 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:8 (op_Sub:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-sub-i16-to-i64:
#   - ((let Mul (op_Sub:16 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:16 (op_Sub:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-sub-i32-to-i64:
#   - ((let Mul (op_Sub:32 ?a ?b)) (commutative-match $Mul))"
#   - ((let Ext (op_Trunc:32 (op_Sub:64 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

[arithmetic:bond:freeze]

advice-and-bond-mul-i64:
  - ((let Muls (op_Mul:64):C) (disjoint C...) (commutative-match $Muls...))
  - ((let Bond (bond $Muls...)) (let Adviced (op_Mul:64 op_Advice:64 op_Advice:64)) (union $Bond $Adviced))

# advice-and-bond-add-i64:
#   - ((let Muls (op_Add:64):C) (disjoint C...) (commutative-match $Muls...))
#   - ((let Bond (bond $Muls...)) (let Adviced (op_Add:64 op_Advice:64 op_Advice:64)) (union $Bond $Adviced))

# advice-and-bond-sub-i64:
#   - ((let Muls (op_Sub:64):C) (disjoint C...) (commutative-match $Muls...))
#   - ((let Bond (bond $Muls...)) (let Adviced (op_Sub:64 op_Advice:64 op_Advice:64)) (union $Bond $Adviced))

# to i128
# [arithmetic:mul-extensions]

# extend-mul-i8-to-i128:
#   - ((let Mul (op_Mul:8 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:8 (op_Mul:128 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-mul-i16-to-i128:
#   - ((let Mul (op_Mul:16 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:16 (op_Mul:128 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-mul-i32-to-i128:
#   - ((let Mul (op_Mul:32 ?a ?b)) (commutative-match $Mul))"
#   - ((let Ext (op_Trunc:32 (op_Mul:128 (op_SExt:64 ?a) (op_SExt:64 ?b)))) (union $Mul $Ext))

# extend-mul-i64-to-i128:
#   - ((let Mul (op_Mul:64 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:64 (op_Mul:128 ?a ?b))) (union $Mul $Ext))

# [arithmetic:bond:freeze]

# advice-and-bond-mul-i128:
#   - ((let Muls (op_Mul:128):C) (disjoint C...) (commutative-match $Muls...))
#   - ((let Bond (bond $Muls...)) (let Adviced (op_Mul:128 op_Advice:64 op_Advice:64)) (union $Bond $Adviced))

# [arithmetic:advice-sext]

# advice-sext-i128:
#   - ((let Mul (op_Mul:128 ?a ?b)) (commutative-match $Mul))
#   - ((let Bond (op_Mul:128 (op_SExt ?a) (op_SExt ?b))) (union $Bond $Mul))

# [arithmetic:mul-extensions]

# extend-mul-i8-to-i128:
#   - ((let Mul (op_Mul:8 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:8 (op_Mul:128 (op_SExt:128 ?a) (op_SExt:128 ?b)))) (union $Mul $Ext))

# extend-mul-i16-to-i128:
#   - ((let Mul (op_Mul:16 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:16 (op_Mul:128 (op_SExt:128 ?a) (op_SExt:128 ?b)))) (union $Mul $Ext))

# extend-mul-i32-to-i128:
#   - ((let Mul (op_Mul:32 ?a ?b)) (commutative-match $Mul))"
#   - ((let Ext (op_Trunc:32 (op_Mul:128 (op_SExt:128 ?a) (op_SExt:128 ?b)))) (union $Mul $Ext))

# extend-mul-i64-to-i128:
#   - ((let Mul (op_Mul:64 ?a ?b)) (commutative-match $Mul))
#   - ((let Ext (op_Trunc:64 (op_Mul:128 (op_SExt:128 ?a) (op_SExt:128 ?b)))) (union $Mul $Ext))

# [arithmetic:bond:freeze]

# advice-and-bond-mul-i128:
#   - ((let Muls (op_Mul:128):C) (disjoint C...) (commutative-match $Muls...))
#   - ((let Bond (bond $Muls...)) (let Adviced (op_Mul:128 op_Advice:128 op_Advice:128)) (union $Bond $Adviced))
