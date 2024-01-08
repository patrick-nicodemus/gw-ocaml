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

let coords n = (n.x, n.y, n.z)

type attr = {
  height : float;
  total_length : float
}
  
type 'a tree = {
  value : 'a;
  mutable children : ('a tree) list
}

type swc_forest = (node tree) list

let map f : 'a tree -> 'b tree = fun t -> 
  let b = { value = f t.value ; children = [] } in
  let l1 = List.rev_map t.children ~f:( fun a -> (a, b) ) in
  (* map' f acts on the accumulator, a list of pairs
     (a_node, b_node) where a_node is at level k+1 and
     b_node is at level k.
     It initializes a new node b' of tree b at level k+1
     and appends it to b_node's list of children (at the head).
     Then it adds each of (a', b') to the stack, where a' ranges
     over the children of a_node.
  *)
  let rec map' = function
    | (a_node, b_node) :: tl ->
      let b' = { value = f a_node.value; children = [] } in
      let () = b_node.children <- b' :: b_node.children in
      let acc' = List.rev_map_append a_node.children tl ~f:(fun a -> (a, b')) in
      map' acc'
    | [] -> ()
  in
  let () = map' l1
  in b

let filter_map (f : 'a -> 'b option) : 'a tree -> ('b tree) list = fun t ->
  let rec filter_map' dirty_trees pairs output =
    match pairs with
    | [] -> (match dirty_trees with
        | [] -> output
        | t :: tl -> (match f t.value with
            | Some value -> let b' = { value = value; children = [] } in
              let pairs' = List.rev_map_append t.children pairs ~f:(fun a -> (a, b')) in
              filter_map' tl pairs' (b' :: output)
            | None -> filter_map' (List.append t.children tl) pairs output))
    | (a_node, b_node) :: tl ->
      (match f a_node.value with
        | Some value -> 
          let b' = { value = value; children = [] } in
          let () = b_node.children <- b' :: b_node.children in
          let acc' = List.rev_map_append a_node.children tl ~f:(fun a -> (a, b')) in
          filter_map' dirty_trees acc' output
        | None -> filter_map' (List.rev_append a_node.children dirty_trees) tl output)
  in
  filter_map' [t] [] []

let count t =
  (* acclist is a list of lists of trees. *)
  let rec count' acclist accnum =
    match acclist with
    | [] -> accnum
    | h :: tl -> (match h with
        | [] -> count' tl (1 + accnum)
        | a :: tl2 -> count' (a.children :: tl2 :: tl) accnum)
  in count' [ [ t ] ] 0

let dist n n' =
  let x = n.x -. n'.x in
  let y = n.y -. n'.y in
  let z = n.z -. n'.z in
  Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))

let fold (f : 'a -> 'a list -> 'b list -> 'b) (t : 'a tree) =
  (* Idea: If the function fold' is being called at a *depth* of n,
     then the *length* of a_nodes and b_nodes are both n.
     Each element of a_nodes and b_nodes is a list that contains values left to compute
     at the current level and values already computed at the current level;
     it represents the status of the computation *at that depth.*
  *)
  let rec fold' a_nodes b_nodes =
    match a_nodes with
    | [] -> List.hd_exn (List.hd_exn b_nodes)
    | (a_remain, a_parent) :: a_tl -> (* a_hd is a list of trees that we still have to apply fold' to. *)
      (match a_remain with
       | ak :: an -> fold' ((ak.children, ak) :: (an, a_parent) :: a_tl) ([] :: b_nodes)
       | [] ->
         (match b_nodes with
          | b_current :: b_parent :: b_rest ->
            let brec = { value = f a_parent.value (List.map a_parent.children ~f:(fun a-> a.value))
                             (List.rev_map ~f:(fun b-> b.value) b_current);
                         children = List.rev b_current } in
            fold' a_tl ((brec :: b_parent) :: b_rest)
          | _ -> raise (Invalid_argument "WAH")))
  in
  fold' [ (t.children, t) ] [ [ ] ]

(* The following functions explain how to convert a sequence of nodes to an swc.
   Our approach is:
   1. build a map associating indices to records. This doesn't give any more information than the sequence.
   2. Build a map associating indices to their set of children (indices)
   3. Use the map from step 2 to build a reversed linked list of node indices, in reverse topological order.
   4. Build a third map (???) which associates to each node in the list its set of of actual children.
*)

type child_index = Int.Set.t Int.Map.t

let update_child_index record child_index_table =
  let map1 = Map.change child_index_table record.id ~f:(function
      | Some value -> Some value
      | None -> Some Int.Set.empty) in
  match record.parent_id with
  | Some parent_id ->
    Map.update map1 parent_id
      ~f:(fun opt -> match opt with
          | Some childset -> (Set.add childset record.id)
          | None -> (Int.Set.singleton record.id))
  | None -> map1

(* We will use the hashtable instead probably. *)
(* let map_of_seq seq = *)
(*   let seq' = Sequence.map seq ~f:(fun r -> (r.id, r)) in *)
(*   match Int.Map.of_sequence seq' with *)
(*   | `Ok m -> m *)
(*   | _ -> raise (Invalid_argument "Duplicate key") *)

(* Ok. Now that we have parsed the file and returned a stream of its contents.
   This probably has to be broken into two tasks: form a map, and then form a tree.  *)

(* let hash_of_seq seq = *)
(*   let h = Hashtbl.create ~growth_allowed:true ~size:100 (module Int) in *)
(*   let () = Sequence.iter seq ~f:(fun r -> Hashtbl.set h ~key:r.id ~data:r) in *)
(*   h *)

(* Our convention for the child index will be that
   if a node has no children, it will appear in the map with an empty set.
   All nodes should appear in the tree regardless of whether they have children. *)


(* Ok. I think what I want to do is:
   1. First, just build the naive graph as a map (nodes carry pointers to their parents)
   2. Given the map M, get any key K from the map. Look up its parent.
     2.1 If it has null parent, it's a top level node, add it to the list.
     2.2 Otherwise, if it has a valid parent id, look to see if the key is still in the map
         or if we already deleted it.
     2.2.1 If the key *is* still in the map, recursively repeat this algorithm,
           adding all parents to the list and deleting them from the map.
           Then, delete this entry from the map, and return the new list.
     2.2.2 If the key is not still in the map, good news, we can go home early -
           we don't have to do a recursive function call.
           (It should be an invariant here that if the (parent) key is *not* in the map,
            it *is* in the linked list.)
           (In fact, it might be better to enforce that invariant.)
           Add the current key to the list, return the new list.
   This is probably too naive as written and will lead to stack overflow.
*)

(* This function takes an int -> node map and associates to it a list of
   children in reverse topological order.
*)
let rev_list (swc : node Int.Map.t) =
  (* Unload the stack and put it on acclist. This mutates the stack. *)
  let stack_to_acclist stack acclist =
    let acclist' = Stack.fold stack ~init:acclist ~f:(fun acclist key -> key :: acclist) in
    let () = Stack.clear stack in
    acclist'
  in
  let rec rev_list_stack swc stack acclist =
    match Map.min_elt swc with
    | Some (k, r) -> rev_list_stack_kr k r swc stack acclist
    | None -> stack_to_acclist stack acclist
  and
    (* Invariant: it's assumed key is in swc, and points to record.
       key is assumed *not* to be on the stack. *)
    rev_list_stack_kr (key :int) (record :node)
      (swc : node Int.Map.t) (stack : int Stack.t) (acclist : int list) =
    let () = Stack.push stack key in
    let swc' = Map.remove swc key in
    match record.parent_id with
    | Some parent -> (match Map.find swc' parent with
        | Some r ->  rev_list_stack_kr parent r swc' stack acclist
        | None -> let acclist' = stack_to_acclist stack acclist in
          rev_list_stack swc' stack acclist')
    | None -> let acclist' = stack_to_acclist stack acclist in
      rev_list_stack swc' stack acclist'
  in
  rev_list_stack swc (Stack.create ()) []

(* At this point we have completed steps 1., 2., 3., now step 4.*)

module NeuronTree = struct
  module T = struct
    type t = node tree
                 
    (* let compare (r : t) (s : t) = Int.compare r.value.id s.value.id *)
    (* let children t = t.children *)
    (* let sexp_of_t t = Sexp.Atom (Int.to_string t.id) *)
  end
  include T

  let fold swc_forest ~(f : 'a -> t -> 'a) (init : 'a) =
  let rec fold_acc neuron_tree a_accum stack =
    fold_acc_list (f a_accum neuron_tree) (List.append neuron_tree.children stack)
  and
     fold_acc_list a_accum stack =
      match stack with
      | h :: tail -> fold_acc h a_accum tail
      | [] -> a_accum
  in fold_acc_list init swc_forest

  (* let disttriple (x,y,z) (x',y',z') = *)
  (*   let a = x -. x' in *)
  (*   let b = y -. y' in *)
  (*   let c = z -. z' in     *)
  (*   Float.sqrt ((a *. a) +. (b *. b) +. (c *. c)) *)
    
end

let length swc_forest = NeuronTree.fold swc_forest
    ~f:(fun acc n -> List.fold n.children ~init:acc ~f:(fun h n' -> h +. dist n.value n'.value))
    0.0

let neuron_length nt = length [nt]

(* let rec height_tree (t : node tree) = *)
(*   let { value ; children } = t in *)
(*   let children' = List.map children ~f:height_tree in *)
(*   { value = List.fold (List.zip_exn children children') *)
(*         ~init:0.0 *)
(*         ~f:(fun acc z -> let tree, treehtnode = z in *)
(*              Float.max acc (treehtnode.value +. (dist value tree.value))); *)
(*     children = children' } *)

let neuron_height neuron =
  let rec f stack acc_max =
    match Stack.pop stack with
    | Some (h, parent) ->
      let depths = List.map parent.children ~f:(fun a -> h +. (dist parent.value a.value)) in
      let () = List.iter (List.zip_exn depths parent.children)
          ~f:(Stack.push stack) in
      f stack (List.fold depths ~init:acc_max ~f:Float.max)
    | None -> acc_max
  in
  let (s : (float * (node tree)) Stack.t) = (Stack.singleton (0.0,neuron)) in
  f s 0.0
    
let swc_height swc_forest =
  List.fold (List.map ~f:neuron_height swc_forest) ~init:0.0 ~f:Float.max


let swc_forest_revlist revlist (reference_nm : node Int.Map.t) ( cm : child_index ) : swc_forest =
  let rec swc_forest_acc revlist (neuron_map : NeuronTree.t Int.Map.t) roots =
    match revlist with
    | h :: t -> 
      let s = Set.to_sequence (Map.find_exn cm h) in
      let r = Map.find_exn reference_nm h in
      let s' = Sequence.map ~f:(Map.find_exn neuron_map) s in
      let l = Sequence.to_list_rev s' in
      let (newr : NeuronTree.t) =
        { value = 
            { id = r.id;
              parent_id  = r.parent_id;
              r = r.r;
          nodetype = r.nodetype;
          x = r.x; y = r.y; z = r.z;
          (* height = *)
          (*   (let heights = List.map l *)
          (*       ~f:(fun child -> *)
          (*          (disttriple (r.x, r.y, r.z) (child.x, child.y, child.z) +. child.height)) in *)
          (*   (List.fold heights ~init:0.0  ~f:Float.max)); *)
          (* totallength = *)
          (*     let lengths = List.map l *)
          (*       ~f:(fun child -> *)
          (*          (disttriple (r.x, r.y, r.z) (child.x, child.y, child.z) +. child.totallength)) in *)
          (*     List.fold lengths ~init:0.0 ~f:Float.add *)
          } ;
          children = l }
         in
      let (newmap : NeuronTree.t Int.Map.t ) = Map.add_exn neuron_map ~key:h ~data:newr in
      let roots' = match r.parent_id with
        | Some _ -> roots
        | None -> newr :: roots in
      swc_forest_acc t newmap roots'
    | [] -> roots
  in
  swc_forest_acc revlist Int.Map.empty []

(* let swc_forest_of_seq seq = *)
(*   let reference_nm = map_of_seq seq in *)
(*   let  = rev_list reference_nm *)

let fold2 ~init1 ~f1 ~init2 ~f2 =
  Sequence.fold ~init:(init1, init2) ~f:(fun (acc1, acc2) (val1, val2) -> (f1 acc1 val1, f2 acc2 val2))

let fork (seq: 'a Sequence.t) = Sequence.map seq ~f:(fun a -> (a, a))

let swc_forest_of_seq seq =
  let reference_nm, ci =
    fold2 ~init1:Int.Map.empty ~f1:(fun m r -> Map.add_exn m ~key:r.id ~data:r)
      ~init2:Int.Map.empty ~f2:(fun m r -> update_child_index r m)
      (fork seq)
  in
  let revlist = rev_list reference_nm in
  swc_forest_revlist revlist reference_nm ci

    (* Sequence.fold seq ~init:(Int.Map.empty, Int.Map.empty) *)
    (*   ~f:(fun (m1, m2) r -> *)
    (*       ( Map.add_exn m1 ~key:r.id ~data:r , *)
    (*         update_child_index r m2)) in *)
