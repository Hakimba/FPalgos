type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
    value: 'a;
    left: 'a tree;
    right: 'a tree
}

(* Exemple 1 : 
              1
            /   \
           2     3
        /    \     \
       4     5      6
           /   \   /
          7     8 9
*)

let ex1 =
  Node {
      value = 1;
      left = Node {
                value = 2;
                left = Node{
                         value = 4;
                         left = Leaf;
                         right = Leaf
                };
                right = Node{
                          value = 5;
                          left = Node{value=7;left=Leaf;right=Leaf};
                          right = Node{value=8;left=Leaf;right=Leaf}
                }
            };
      right = Node {
                value=3;
                left= Leaf;
                right= Node{value=6;left=Node{value=9;left=Leaf;right=Leaf};right=Leaf}
            }
  }

let rec lookup x = function
  | Leaf -> false
  | Node {value; left; right} -> value = x || lookup x left || lookup x right

(** l'ordre prefixe/préordre dans une liste *)
(** O(n^2) *)
let rec preorder_q = function
  | Leaf -> []
  | Node {value; left; right} -> [value] @ preorder_q left @ preorder_q right

(** preorder O(n) *)
let preorder_l tree =
  let rec preorder_aux acc = function
    | Leaf -> acc
    | Node {value; left; right} -> value :: (preorder_aux (preorder_aux acc right) left)
  in preorder_aux [] tree

let equivalence_preorder tree = preorder_q tree = preorder_l tree

(** infixe, inorder O(n)*)
let inorder tree =
  let rec inorder_aux acc = function
    | Leaf -> acc
    | Node {value; left; right} -> inorder_aux (value :: (inorder_aux acc right)) left
  in inorder_aux [] tree

(** postfixe, postordre, suffixe O(n) *)
let postorder tree =
  let rec postorder_aux acc = function
    | Leaf -> acc
    | Node {value; left; right} -> postorder_aux (postorder_aux (value :: acc) right) left
  in postorder_aux [] tree