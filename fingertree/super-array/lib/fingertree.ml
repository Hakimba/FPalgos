module type MONOID = sig 
  type t 
  val neutral : t 
  val (<|>) : t -> t -> t
end

module type MEASURABLE = sig
  type t 
  module Monoid : MONOID
  val measure : t -> Monoid.t
end

module type DIGIT = functor (M : MEASURABLE) -> sig
  type 'a digit =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a
  type t = M.t digit
  val listToDigit : M.t list -> t
  val lastInit : t -> M.t * t 

  include MEASURABLE with type t := t and module Monoid = M.Monoid
end

module Digit (M : MEASURABLE) = struct
  exception Invalid_n of string
  exception Impossible_state
  type 'a digit =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a
  type t = M.t digit

  module Monoid = M.Monoid

  let fold_map f =
    let open Monoid in
    function
    | One a -> f a
    | Two (a, b) -> f a <|> f b
    | Three (a, b, c) -> f a <|> f b <|> f c
    | Four (a, b, c, d) -> f a <|> f b <|> f c <|> f d
  
  let listToDigit = function
    | [x]  -> One(x)
    | [x;y] -> Two(x,y)
    | [x;y;z] -> Three(x,y,z) 
    | [x;y;z;w] -> Four(x,y,z,w)
    | _ -> raise (Invalid_n "must have 1..4 elements")

  let lastInit = function
      | One(_) -> raise Impossible_state
      | Two(v,u) -> (u,listToDigit [v])
      | Three(v,u,o) -> (o,listToDigit [v;u])
      | Four(v,u,o,p) -> (p,listToDigit [v;u;o])

  let measure = fold_map M.measure
end

module type NODE = functor (M : MEASURABLE) -> sig
type 'a node =
  | Node2 of (M.Monoid.t * 'a * 'a)
  | Node3 of (M.Monoid.t * 'a * 'a * 'a)
type t = M.t node
val node2 : M.t -> M.t -> t
val node3 : M.t -> M.t -> M.t -> t
val measure : t -> M.Monoid.t
val nodeToList : t -> M.t list

include MEASURABLE with type t := t and module Monoid = M.Monoid
end

module Node (M : MEASURABLE) = struct
  type 'a node =
    | Node2 of (M.Monoid.t * 'a * 'a)
    | Node3 of (M.Monoid.t * 'a * 'a * 'a)
  type t = M.t node
  module Monoid = M.Monoid

  let node2 a b =
    let v = Monoid.(M.measure a <|> M.measure b) in
    Node2 (v, a, b)

  let node3 a b c =
    let v = Monoid.(M.measure a <|> M.measure b <|> M.measure c) in
    Node3 (v, a, b, c)

  let measure = function
    | Node2 (x, _, _) | Node3 (x, _, _, _) -> x

  let nodeToList =
    function
    | Node2(_,x,y) -> [x;y]
    | Node3(_,x,y,z) -> [x;y;z]
end

module Finger_tree (M : MEASURABLE) : sig
  type t

  val empty : t
  val singleton : M.t -> t
end = struct
  module D = Digit (M)
  module N = Node (M)

  type 'a f =
    | Empty
    | Single of 'a
    | Deep of M.Monoid.t * D.t * N.t f * D.t

  type t = M.t f
  type 'a view = Nil | View of 'a * 'a f

  let empty = Empty
  let singleton x = Single x

  let measure_node = function
    | Empty -> M.Monoid.neutral
    | Single x -> N.measure x
    | Deep (x, _, _, _) -> x
  
  let digitToTree digit =
    let open D in
    match digit with
    |One a -> Single a
    |Two(a,b) -> Deep(measure digit,One(a),Empty,One(b))
    |Three(a,b,c) -> Deep(measure digit,One(a),Empty,Two(b,c))
    |Four(a,b,c,d) -> Deep(measure digit,Two(a,b),Empty,Two(c,d))

  (** here the problem, i dont know how to write a polymorphic recursion with my actual types*)
  let rec viewr : 'a. 'a f -> 'a view = function
    | Empty -> Nil
    | Single x -> View(x,Empty)
    | Deep(_,prefix,deeper,One(x)) -> 
      let open M.Monoid in
      let dp = (match viewr deeper with
      |View(node,rest') -> 
        let suff = D.listToDigit (N.nodeToList node) in
        let annot = D.measure prefix <|> measure_node deeper <|> D.measure suff in
        Deep(annot,prefix,rest',suff)
      |Nil -> digitToTree prefix)
      in View(x,dp)
    | _ -> raise (Invalid_argument "e") (*Deep{annotation;prefix;deeper;suffix} ->
      let (head,rs) = Digit.lastInit suffix in
      annot = measure prefix <|> measure deeper <|> measure rs in
      View(head,Deep{annotation=annot;prefix=prefix;deeper=deeper;suffix=rs})*)

        (*let deep prefix deeper suffix = match prefix,suffix with
            |[],[] -> *)
end
