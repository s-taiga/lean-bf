import Bf.AST

open Lean Syntax Meta Elab Term

declare_syntax_cat bf_op
syntax ">" : bf_op
syntax "<" : bf_op
syntax "+" : bf_op
syntax "-" : bf_op
syntax "." : bf_op
syntax "," : bf_op

open BFOp in
macro_rules
  | `(bf_op| > ) => `(PointerIncrement)
  | `(bf_op| < ) => `(PointerDecrement)
  | `(bf_op| + ) => `(ValueIncrement)
  | `(bf_op| - ) => `(ValueDecrement)
  | `(bf_op| . ) => `(PrintValue)
  | `(bf_op| , ) => `(ReadAndSet)

instance : Coe (TSyntax `bf_op) (TSyntax `term) where
  coe s := ⟨s.raw⟩

macro "[bf_op|" op:bf_op "]" : term => `($op)
-- #check [bf_op| . ]

declare_syntax_cat bf_expr
syntax bf_op            : bf_expr
syntax "[" bf_expr* "]" : bf_expr

instance : Coe (TSyntax `bf_expr) (TSyntax `term) where
  coe s := ⟨s.raw⟩

open BFExpr in
macro_rules
  | `(bf_expr| $op:bf_op) => `(SingleCommand $op)
  | `(bf_expr| [ $[$exp:bf_expr]* ]) => `(BracketCommant [$exp,*])

instance : Coe (TSyntax `bf_expr) (TSyntax `term) where
  coe s := ⟨s.raw⟩

macro "[bf_expr|" exp:bf_expr "]" : term => `($exp)
-- #check [bf_expr|
--   [ . , + < + - [ . ] [ ] ]
-- ]

macro "[bf|" exp:bf_expr+ "]" : term => `([$exp,*])
-- #check [bf|
--   . >
-- ]
