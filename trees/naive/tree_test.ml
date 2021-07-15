open OUnit2
open Tree



let tests = "tests suite for tree" >::: [
  "lookup empty" >:: (fun _ -> assert_equal false (lookup 7 Leaf));
  "lookup false" >:: (fun _ -> assert_equal false (lookup 11 ex1));
  "lookup true" >:: (fun _ -> assert_equal true (lookup 4 ex1));
  "wikipedia tree preorder" >:: (fun _ -> assert_equal [1;2;4;5;7;8;3;6;9] (preorder_l ex1));
  "wikipedia tree inorder" >:: (fun _ -> assert_equal [4;2;7;5;8;1;3;9;6] (inorder ex1));
  "wikipedia tree postorder" >:: (fun _ -> assert_equal [4;7;8;5;2;9;6;3;1] (postorder ex1));
]

let _ = run_test_tt_main tests