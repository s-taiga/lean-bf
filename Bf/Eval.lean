import Bf.AST
import Mathlib.Control.Lawful
import Mathlib.Data.Vector.Basic

open Mathlib

structure BFRegister (n : Nat) where
  position : Fin n
  register : Vector (Fin 256) n := Vector.replicate n 0

structure BFIO where
  input  : List UInt8 := []
  output : List UInt8 := []

def BFState n := BFRegister n × BFIO

def initState' (n : Nat) (h : 0 < n) (input : List UInt8) : BFState n := ⟨{position := ⟨0, by simpa⟩}, {input := input}⟩

macro "initState" n:num i:term : term => `(initState' $n (by simp) $i)

def BFMonad n := StateT (BFState n) <| Except String

instance : Monad (BFMonad n) where
  pure := .pure
  bind := .bind

instance : MonadState (BFState n) (BFMonad n) where
  get := .get
  set := .set
  modifyGet := .modifyGet

instance : MonadExcept String (BFMonad n) where
  throw s := StateT.mk λ _ ↦ throw s
  tryCatch mbf action := StateT.mk λ σ ↦ tryCatch (mbf.run σ) <| (flip StateT.run σ) ∘ action

section

variable {m} [Monad m] [MonadState (BFState n) m] [MonadExcept String m]

def pointerIncrement (h : 1 < n) : m Unit := do
  modify λ ⟨r, i⟩ ↦ ⟨{r with position := r.position + ⟨1, h⟩}, i⟩
def pointerDecrement (h : 1 < n) : m Unit := do
  modify λ ⟨r, i⟩ ↦ ⟨{r with position := r.position - ⟨1, h⟩}, i⟩
def valueIncrement : m Unit := do
  modify λ ⟨r, i⟩ ↦
    let ⟨pos, reg⟩ := r
    let c := reg.get pos
    ⟨{r with register := reg.set pos (c + ⟨1, by simp⟩)}, i⟩
def valueDecrement : m Unit := do
  modify λ ⟨r, i⟩ ↦
    let ⟨pos, reg⟩ := r
    let c := reg.get pos
    ⟨{r with register := reg.set pos (c - ⟨1, by simp⟩)}, i⟩
def printValue : m Unit := do
  modify λ ⟨r, i⟩ ↦
    let ⟨pos, reg⟩ := r
    let ⟨c, _⟩ := reg.get pos
    ⟨r, {i with output := ⟨c⟩ :: i.output}⟩
def readAndSet : m Unit := do
  let ⟨_, i⟩ ← get
  match i.input with
  | [] => throw "input exhausted."
  | ⟨x⟩ :: xs =>
  modify λ ⟨r, i⟩ ↦
    let ⟨pos, reg⟩ := r
    ⟨{r with register := reg.set pos x}, {i with input := xs}⟩

open BFOp in
def evalBFOp (h : 1 < n) : BFOp → m Unit
  | .PointerIncrement => pointerIncrement h
  | .PointerDecrement => pointerDecrement h
  | .ValueIncrement => valueIncrement
  | .ValueDecrement => valueDecrement
  | .PrintValue => printValue
  | .ReadAndSet => readAndSet

open BFExpr in
partial def evalBFExpr (h : 1 < n) : BFExpr → m Unit
  | .SingleCommand op => evalBFOp h op
  | .BracketCommant exps => do
    let mut ⟨⟨pos, reg⟩, _⟩ ← get
    while reg.get pos ≠ 0 do
      for exp in exps do
        evalBFExpr h exp
      ⟨⟨pos, reg⟩, _⟩ ← get

end

def evalBF (h : 1 < n) (exps : List BFExpr) : BFMonad n Unit := do
  for exp in exps do
    evalBFExpr h exp

macro "[evalBF: " n:num " | " exp:term " ]" : term => `((evalBF (by simp) $exp).run (initState $n []))
macro "[evalBF: " n:num " : " i:term " | " exp:term " ]" : term => `((evalBF (by simp) $exp).run (initState $n $i))
