
(**
  require these laws :
    mappend empty x = x
    mappend x empty = x
    mappend x (mappend y z ) = mappend (mappend x y) z
*)
module type MONOID = sig
  type t
  val mempty : t
  val mappend : t -> t -> t
end


module IntMonoid : MONOID = struct
  type t = int
  let mempty = 0
  let mappend = (+)
end

module type FINGERTREE_SEQ = functor (Monoid : MONOID) ->
sig
  type 'a finger_tree
  type 'a view

  val empty : 'a finger_tree
  val prepend : 'a -> 'a finger_tree -> 'a finger_tree
  val append : 'a -> 'a finger_tree -> 'a finger_tree
  val viewl : 'a finger_tree -> 'a view
  val viewr : 'a finger_tree -> 'a view
  val concat : 'a finger_tree -> 'a finger_tree -> 'a finger_tree
end

module Ft_seq (M : MONOID) = struct
  exception Invalid_n of string
  exception Impossible_state

  type 'a node = 
    | Branch2 of 'a * 'a
    | Branch3 of 'a * 'a * 'a

  (**Represent the prefix/suffix part of the spine of a finger tree*)
  type 'a affix =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

  type 'a finger_tree =
    | Empty
    | Single of 'a
    | Deep of {
      prefix : 'a affix;
      deeper : 'a node finger_tree;
      suffix : 'a affix
    }

  type 'a view =
    | Nil
    | View of 'a * 'a finger_tree

  let empty = Empty

  let affixPrepend el affix = match affix with
    | One(a) -> Two(el,a)
    | Two(a,b) -> Three(el,a,b)
    | Three(a,b,c) -> Four(el,a,b,c)
    | _ -> raise (Invalid_n "must contain a number of element between 1 and 4 included")

  let affixAppend el affix = match affix with
    | One(a) -> Two(a,el)
    | Two(a,b) -> Three(a,b,el)
    | Three(a,b,c) -> Four(a,b,c,el)
    | _ -> raise (Invalid_n "must contain a number of element between 1 and 4 included")

  let affixToList = function
    | One(x) -> [x] 
    | Two(x,y) -> [x;y]
    | Three(x,y,z) -> [x;y;z]
    | Four(x,y,z,w) -> [x;y;z;w]

  let listToAffix = function
    | [x]  -> One(x)
    | [x;y] -> Two(x,y)
    | [x;y;z] -> Three(x,y,z) 
    | [x;y;z;w] -> Four(x,y,z,w)
    | _ -> raise (Invalid_n "must have 1..4 elements")

  let nodeToList = function
    | Branch2(x,y) -> [x;y]
    | Branch3(x,y,z) -> [x;y;z]

  let listToNode = function
    | [x;y] -> Branch2(x,y)
    | [x;y;z] -> Branch3(x,y,z)
    | _ -> raise (Invalid_n "node must contain two or three elements")

  let rec nodes = function
    | [] -> raise (Invalid_argument "not enough elements for nodes")
    | [_] -> raise (Invalid_argument "not enough elements for nodes")
    | [x;y] -> [Branch2(x,y)]
    | [x;y;z] -> [Branch3(x,y,z)]
    | x::y::rest -> Branch2(x,y)::(nodes rest)

  let rec prepend : 'a. 'a -> 'a finger_tree -> 'a finger_tree =
    fun x ft -> match ft with
      | Empty -> Single x
      | Single(a) -> Deep{prefix=One(x);deeper=Empty;suffix=One(a)}
      | Deep {prefix=Four(a,b,c,d);deeper=deeper;suffix=suffix} ->
          let node = Branch3(b,c,d) in
          Deep{prefix=Two(x,a);deeper=(prepend node deeper);suffix=suffix}
      | Deep{prefix=prefix;deeper=deeper;suffix=suffix} -> Deep{prefix=affixPrepend x prefix;deeper;suffix}

  let rec append : 'a. 'a -> 'a finger_tree -> 'a finger_tree =
    fun x ft -> match ft with
      | Empty -> Single x
      | Single(a) -> Deep{prefix=One(a);deeper=Empty;suffix=One(x)}
      | Deep {prefix=prefix;deeper=deeper;suffix=Four(a,b,c,d)} ->
          let node = Branch3(a,b,c) in
          Deep{prefix=prefix;deeper=(append node deeper);suffix=Two(d,x)}
      | Deep{prefix=prefix;deeper=deeper;suffix=suffix} -> Deep{prefix;deeper;suffix=affixAppend x suffix}

  let rec viewl : 'a. 'a finger_tree -> 'a view = function
    | Empty -> Nil
    | Single(a) -> View(a,Empty)
    | Deep{prefix=One(x);deeper;suffix} -> 
        let rest = match viewl deeper with
          | View(node,rest') -> Deep{prefix=(listToAffix(nodeToList node));deeper=rest';suffix=suffix}
          | Nil -> match suffix with
              | One(v) -> Single(v)
              | Two(v,u) -> Deep{prefix=One(v);deeper=Empty;suffix=One(u)}
              | Three(v,u,o) -> Deep{prefix=Two(v,u);deeper=Empty;suffix=One(o)}
              | Four(v,u,o,p) -> Deep{prefix=Three(v,u,o);deeper=Empty;suffix=One(p)}
    in View(x,rest) 
    | Deep{prefix=prefix;deeper=deeper;suffix=suffix} ->
        let (head,rest) = 
          match affixToList prefix with
            | [] -> raise Impossible_state
            | head::rest -> (head,rest)
        in
        View(head,Deep{prefix=listToAffix rest;deeper;suffix})

  let rec viewr : 'a. 'a finger_tree -> 'a view = function
    | Empty -> Nil
    | Single(a) -> View(a,Empty)
    | Deep{prefix=prefix;deeper;suffix=One(x)} ->
        let rest = match viewr deeper with
          | View(node,rest') -> Deep{prefix=prefix;deeper=rest';suffix=(listToAffix(nodeToList(node)))}
          | Nil -> match prefix with
            | One(v) -> Single(v)
            | Two(v,u) -> Deep{prefix=One(v);deeper=Empty;suffix=One(u)}
            | Three(v,u,o) -> Deep{prefix=Two(v,u);deeper=Empty;suffix=One(o)}
            | Four(v,u,o,p) -> Deep{prefix=Three(v,u,o);deeper=Empty;suffix=One(p)}
        in View(x,rest) 
    | Deep{prefix=prefix;deeper=deeper;suffix=suffix} ->
        let (head,rest) = 
          match suffix with
          | One(_) -> raise Impossible_state
          | Two(v,u) -> (u,listToAffix [v])
          | Three(v,u,o) -> (o,listToAffix [v;u])
          | Four(v,u,o,p) -> (p,listToAffix [v;u;o])
        in
        View(head,Deep{prefix=prefix;deeper;suffix=rest})

  let rec concatWithMiddle : 'a. 'a finger_tree -> 'a list -> 'a finger_tree -> 'a finger_tree =
    fun ftl middle ftr ->
    match ftl,middle,ftr with
    | Empty, [], right -> right
    | Empty, (x::xs), right -> prepend x (concatWithMiddle Empty xs right)
    | Single(a), xs, right -> prepend a (concatWithMiddle Empty xs right)
    | left, [], Empty -> left
    | left, (x::xs), Empty -> append x (concatWithMiddle left xs Empty)
    | left, xs, Single(a) -> append a (concatWithMiddle left xs Empty)
    | Deep{prefix=pl;deeper=dl;suffix=sl},mid,Deep{prefix=pr;deeper=dr;suffix=sr} ->
        let mid' = nodes ( (affixToList sl) @ mid @ (affixToList pr)) in
        let deeper' = concatWithMiddle (dl) mid' (dr) in
        Deep{prefix=pl;deeper=deeper';suffix=sr}

  let concat fa fb = concatWithMiddle fa [] fb

end

