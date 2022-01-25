(**
  The Super array, based on javascript array.

*)

module type SARRAY =
sig
  type 'a t
  val empty : 'a t
  val length : 'a t -> int
  val capacity : 'a t -> int
  val get : int -> 'a t -> 'a
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val shift : 'a t -> 'a t
  val unshift : 'a -> 'a t -> 'a t
  val indexOf : 'a -> 'a t -> int
  val splice : int -> int -> 'a t -> 'a t
  val slice : int -> int -> 'a t
end

module Sarray = struct
  type 'a t = 'a list
  let empty = []
  let length l = List.length l
  let capacity l = List.length l
  let get i l = List.nth l i
  let push el l = el :: l
  let pop l = l
  let shift l = l
  let unshift _ l = l
  let indexOf _ _ = 0
  let splice _ _ l = l
  let slice _ _ l = l
end