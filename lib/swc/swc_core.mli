open Core
    
type node = {
    id : int;
    nodetype : int;
    x : float;
    y : float;
    z : float;
    r : float;
    parent_id : int option
  }

type attr = {
  height : float;
  total_length : float
}
  
type 'a tree = private {
  value : 'a;
  mutable children : ('a tree) list
}

type swc_forest = (node tree) list

val map : ('a -> 'b) -> 'a tree -> 'b tree

val coords : node -> float * float * float

val filter_map : ('a -> 'b option) -> 'a tree -> ('b tree) list

val count : 'a tree -> int

val dist : node -> node -> float

val neuron_height : node tree -> float

val swc_height : (node tree) list -> float

val length : swc_forest -> float

val neuron_length : (node tree) -> float

(**  The following function is equivalent in its behavior to:

     let rec fold f t =
       let b_children = List.map (fun z -> (fold f z)) t.children in
       let values = List.map (fun z -> z.value) in
       { value = f (t.value)
                   values t.children
                   values b_children;
         children = values b_children }

     However, it is implememented in a stack-safe way.
**)
val fold : ('a -> 'a list -> 'b list -> 'b) -> 'a tree -> 'b tree

val swc_forest_of_seq : (node Sequence.t) -> swc_forest
