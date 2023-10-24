open Core

module Tree : sig
  type 'a t
  val node : 'a t -> 'a
  val children : 'a t -> ('a t) list
  val map : ('a -> 'b) -> 'a t -> 'b t
  end = struct
  type 'a t =
    | Node of 'a * (('a t) list)
  let node (Node (a, _)) = a
  let children = function
    | Node (_, b) -> b

  let[@tail_mod_cons] rec map f = function
  | Node (x, []) -> Node (f x, [])
  | Node (x, hd :: []) -> Node (f x, map f hd :: [])
  | Node (x, hd :: tl) -> Node (f x, map f hd :: List.map ~f:(map f) tl)

end
