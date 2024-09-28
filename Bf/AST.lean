import Lean

inductive BFOp where
  | PointerIncrement
  | PointerDecrement
  | ValueIncrement
  | ValueDecrement
  | PrintValue
  | ReadAndSet

inductive BFExpr where
  | SingleCommand  : BFOp → BFExpr
  | BracketCommant : List BFExpr → BFExpr
