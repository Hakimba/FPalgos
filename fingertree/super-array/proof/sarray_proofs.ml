open Easier_proof.DslProp


let t = to_proofs [
  block "commutative property of Nat addition" [
    prop "add_right_zero" ~context:(forall [("n","nat")]) ((atom "add n Zero" =.= atom "n") >> induction "n");

    prop "add_s" ~context:(forall [("x","nat");("y","nat")]) ((atom "S (add x y)" =.= atom "add x (S y)") >> induction "x");

    prop "add_commut"
      ~context:(forall [("x","nat");("y","nat")])
      ((atom "add x y" =.= atom "add y x") >> induction "x")
      ~hints:["add_right_zero";"add_s"]
  ]
]

let _ = run t