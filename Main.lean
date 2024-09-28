import «Bf»

open IO in
def main : IO Unit := do
  match [evalBF:10:[20]| [bf|
    , -- read input
    [
      >
      + +
      < -
    ]
    > .
  ] ] with
  | .ok ⟨_, ⟨⟨pos, reg⟩, ⟨inp, out⟩⟩⟩ => do
    println s!"position: {pos}"
    println s!"register: {reg.toList}"
    println s!"input   : {inp}"
    println s!"output  : {out}"
    pure ()
  | .error s => println s
