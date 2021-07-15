open OUnit2
open BinTree

let tests = "test suite for binTree" >::: [
  "insert" >:: (fun _ -> assert_equal true (lookup 4 (insertNode h2 4)));
  "deletion" >:: (fun _ -> assert_equal false (lookup 3 (deleteNode h2 3)))
]

let _ = run_test_tt_main tests