open Core
open Stdio

module Read_swc : sig
  type swcline' =
  { id : int;
    nodetype : int;
    x : float;
    y : float;
    z : float;
    r : float;
    parent_id : int option    
  }

  val seq_of_swc : string -> swcline' Sequence.t
  val list_of_swc : string -> (int * swcline') list      
end =
struct
  type swcline' =
    { id : int;
      nodetype : int;
      x : float;
      y : float;
      z : float;
      r : float;
      parent_id : int option    
    }

  let fhdtl_exn ell f =
    match ell with
    | x :: a -> (f x, a)
    | [] -> raise (Invalid_argument "List is empty.")

  let parse_line str =
    let ell = String.split_on_chars ~on:[' '] str
              |> List.filter ~f:(fun s -> String.length s > 0) in
    let id, t1 = fhdtl_exn ell Int.of_string in
    let nodetype, t2 = fhdtl_exn t1 Int.of_string in
    let x, t3 = fhdtl_exn t2 Float.of_string in
    let y, t4 = fhdtl_exn t3 Float.of_string in
    let z, t5 = fhdtl_exn t4 Float.of_string in
    let r, t6 = fhdtl_exn t5 Float.of_string in
    let parent_id, _ = fhdtl_exn t6
        (fun str -> let i = Int.of_string str in
          if i > 0 then Some i else None) in
    { id; nodetype; x;y;z;r; parent_id }

  (** Return a sequence of nodes. *)
  let seq_of_swc filename =
    let data = In_channel.read_lines filename in
    (* let stream = In_channel.create filename in *)
    (* let data = In_channel.input_lines stream in *)
    let dataseq = Sequence.of_list data
                  |> Sequence.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"#" || String.is_empty s))
                  |> Sequence.map ~f:parse_line
    in dataseq

  let list_of_swc filename =
    let data = In_channel.read_lines filename in
    List.filter_map ~f:(
      fun s -> if String.is_prefix s ~prefix:"#" || String.is_empty s then None else
          Some (let r = parse_line s in (r.id, r))) data

end
open Read_swc

(* Ok. Now that we have parsed the file and returned a stream of its contents.
   This probably has to be broken into two tasks: form a map, and then form a tree.  *)

(* Our approach is:
   1. build a map associating indices to records. This doesn't give any more information than the sequence.
   2. Build a map associating indices to their set of children (indices)
   3. Use the map from step 2 to build a reversed linked list of node indices, in reverse topological order.
   4. Build a third map (???) which associates to each node in the list its set of of actual children.
*)

(* Our convention for the child index will be that
   if a node has no children, it will appear in the map with an empty set.
   Ideally all nodes should appear in the tree regardless of whether they have children. *)
type child_index = Int.Set.t Int.Map.t

let update_child_index record (child_index_table : child_index) =
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

let map_of_seq seq =
  let seq' = Sequence.map seq ~f:(fun r -> (r.id, r)) in
  match Int.Map.of_sequence seq' with
  | `Ok m -> m
  | _ -> raise (Invalid_argument "Duplicate key")

let hash_of_seq seq =
  let h = Hashtbl.create ~growth_allowed:true ~size:100 (module Int) in
  let () = Sequence.iter seq ~f:(fun r -> Hashtbl.set h ~key:r.id ~data:r) in
  h
  (* let ell = Sequence.to_list_rev (Sequence.map seq ~f:(fun r -> (r.id, r))) in *)
  (* Hashtbl.of_alist_exn ~growth_allowed:true ~size:100 (module Int) ell *)

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
let rev_list (swc : swcline' Int.Map.t) =
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
    rev_list_stack_kr (key :int) (record :swcline')
      (swc : swcline' Int.Map.t) (stack : int Stack.t) (acclist : int list) =
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

module type TreeBase = sig
  (* type a *)
  type t
  val children : t -> t list
  val compare : t -> t -> int

end

module Tree (S : TreeBase) = struct
  type ternary =
    | Pos
    | Zero
    | Neg

  let ternary_of k =
    if k = 0 then Zero
    else if k > 0 then Pos
    else Neg  

  let treecompare (a : S.t) (b: S.t)  =
  let rec treecompare_acc_list l1 l2 acc =
    match l1, l2 with
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | hdx :: tlx, hdy :: tly -> treecompare_acc hdx hdy ((tlx, tly) ::  acc)
    | [], [] -> (match acc with
        | (l1' , l2') :: acc' -> treecompare_acc_list l1' l2' acc'
        | [] -> 0)
  and
    treecompare_acc a b acc =
    match ternary_of (S.compare a b) with
    | Zero -> treecompare_acc_list (S.children a) (S.children b) acc
    | Pos -> 1
    | Neg -> -1
  in treecompare_acc a b []
end

(* module TreeSexp (S : Tree)  *)
(*     (A : Map.Sexp_of_m with type t = S.a) : sig *)
(*   include Map.Sexp_of_m with type t = S.tree *)
(* end = struct *)
(*   type t = S.tree *)
(*   let sexp_of_t t = match t with *)
(* end *)

module NeuronTree = struct
  module T = struct
    type t = { id : int; nodetype : int; x : float; y : float; z : float; children : t list }
    type swc_forest = t list
                 
    let compare (r : t) (s : t) = Int.compare r.id s.id
    let children t = t.children
    let sexp_of_t t = Sexp.Atom (Int.to_string t.id)
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

  let dist n1 n2 =
    let x = (n1.x -. n2.x) in
    let y = (n1.y -. n2.y) in
    let z = (n1.z -. n2.z) in    
    Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))

  let length swc_forest = fold swc_forest
      ~f:(fun acc n -> List.fold n.children ~init:acc ~f:(fun h n' -> h +. dist n n'))
      0.0

  let count swc_forest = fold swc_forest ~f:(fun k _ -> k + 1) 0
  
  let nh neuron =
    let rec f stack acc_max =
      match Stack.pop stack with
      | Some (h, parent) ->
        let depths = List.map parent.children ~f:(fun a -> h +. (dist parent a)) in
        let () = List.iter (List.zip_exn depths parent.children)
            ~f:(Stack.push stack) in
        f stack (List.fold depths ~init:acc_max ~f:Float.max)
    | None -> acc_max
    in
    let (s : (float * t) Stack.t) = (Stack.singleton (0.0,neuron)) in
    f s 0.0
    
  let height swc_forest =
    List.fold (List.map ~f:nh swc_forest) ~init:0.0 ~f:Float.max
end

open NeuronTree
let swc_forest_revlist revlist (reference_nm : swcline' Int.Map.t) ( cm : child_index ) : swc_forest =
  let rec swc_forest_acc revlist (neuron_map : NeuronTree.t Int.Map.t) roots =
    match revlist with
    | h :: t -> 
      let s = Set.to_sequence (Map.find_exn cm h) in
      let r = Map.find_exn reference_nm h in
      let s' = Sequence.map ~f:(Map.find_exn neuron_map) s in
      let l = Sequence.to_list_rev s' in
      let (newr : NeuronTree.t) = { id = r.id;
                                    nodetype = r.nodetype;
                                    x = r.x; y = r.y; z = r.z;
                                    children = l } in
      let (newmap : NeuronTree.t Int.Map.t ) = Map.add_exn neuron_map ~key:h ~data:newr in
      let roots' = match r.parent_id with
        | Some _ -> roots
        | None -> newr :: roots in
      swc_forest_acc t newmap roots'
    | [] -> roots
  in
  swc_forest_acc revlist Int.Map.empty []

let swc_forest_filename filename =
  let seq = seq_of_swc filename in
  let reference_nm, ci = 
    Sequence.fold seq ~init:(Int.Map.empty, Int.Map.empty)
      ~f:(fun (m1, m2) r ->
          ( Map.add_exn m1 ~key:r.id ~data:r ,
            update_child_index r m2)) in
  let revlist = rev_list reference_nm in
  swc_forest_revlist revlist reference_nm ci

  

(* module Tree (\* (S : Map.Sexp_of_m) *\) : sig *)
(*   type tree *)
  
(*   include (Map.Sexp_of_m with type t = tree) *)
(* end = struct *)
(*   type tree = Node of S.t * S.t list *)
(*   type t = tree *)
(*   let sexp_of_t _ = (Sexp.Atom "0") *)
(* end *)
(* Can't do this until we figure out how to equip swc_forests with a sexp_of. *)
(* let rec swc_forest = Map.empty ( module NeuronTree ) *)

(* Given an SWC filename, parse the file and return :
   1. A map associating ids to records for that node.
   2. A map associating each id to its set of children.
*)

(* let map_helper2 record child_index_table = ((record.id,record), update_child_index record child_index_table) *)

(* let map_of_swc filename = *)
(*   let stream = In_channel.create filename in *)
(*   let data = In_channel.input_lines stream *)
(*              |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"#" || String.is_empty s)) *)
(*              |> List.map ~f:parse_line *)
(*              |> List.fold ~init:([],Int.Map.empty) *)
(*                ~f:(fun (kvpairs, child_index_table) thisrecord -> *)
(*                    let p1, p2 = map_helper2 thisrecord child_index_table in (p1 :: kvpairs,p2)) *)
(*              (\* |> List.map ~f:(fun z -> (z.id,z))  *\)in *)
(*   let kvpairs, parent_ids = data in *)
(*   (Int.Map.of_alist_exn kvpairs, parent_ids) *)

(* type neurontree = *)
(*   | EmptyNT of float * float * float *)
(*   | ParentNT of float * float * float * (neurontree list) *)

(* type swcforest = neurontree list *)

(* let swcforest_of_naive_recursive intrecordmap childtable = *)
(*   let seq = Int.Map.to_sequence intrecordmap in *)
(*   match seq with *)
(*   | Nil -> (\* Algorithm terminates and finishes up. *\) *)
(*   | Cons f -> let (key, record), _  = f () in *)
(*     let *)

(* let swcforest_of' intrecordmap childtable acc = *)
(*   let seq = Int.Map.to_sequence intrecordmap in *)










