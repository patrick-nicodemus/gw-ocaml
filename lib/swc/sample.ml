open Core
open Swc_core

let addt (x, y, z) (x', y', z') =
  (x +. x', y +. y', z +. z')
let subt (x, y, z) (x', y', z') =
  (x -. x', y -. y', z -. z')
let scal t (x, y, z)  =
  (t *. x, t *. y, t *. z)
let norm (x, y, z) =
  Float.sqrt (x *. x +. y *. y +. z *. z)

let dist a b = norm (subt a b)

let lin n1 n2 offset =
  let d = dist n1 n2 in
  let diff = subt n2 n1 in
  addt n1 (scal (offset /. d) diff)

let points_w_offset n1 n2 offset intvl =
  let rec points_with_offset' n1 n2 offset intvl acc =
  let d = dist n1 n2 in
  if Float.compare d offset <= 0 then (acc, offset -. d) else
    let na = (lin n1 n2 offset) in
    points_with_offset' na n2 intvl intvl (na :: acc)
  in
  points_with_offset' n1 n2 offset intvl []

(* Given a tree nt and a starting offset offset, and given an interval to work with,
   returns a pair of two lists: the first list is (nt, offset) for all children; the second list is
   the list of all points collected in the traversal. The lists are generally not of the same length.
   In particular, the second list is flat, it is just a list of tuples.
 *)
let sample_tree_eu_at_res_offset_onenode (nt : node tree) (offset : float) intvl =
  List.fold nt.children ~f:(fun (nextjobsacc, pointsacc) nt' ->
      let pointsalong, new_offset = points_w_offset (coords nt.value) (coords nt'.value) offset intvl in
      ( ((nt', new_offset) :: nextjobsacc), (List.append pointsalong pointsacc)))
    ~init:( [] , [] )

let rec sample_tree_eu_at_res_offset nt_offset_list intvl acc  =
  match nt_offset_list with
  | [] -> acc
  | nt_offset_list ->
  let new_jobs, new_floats =
    List.fold nt_offset_list ~init:([], acc) ~f:(
        fun (nt_offset_pairs, pts) (nt, offset) ->
        let new_jobs, new_pts = sample_tree_eu_at_res_offset_onenode nt offset intvl in
        (List.append new_jobs nt_offset_pairs, List.append new_pts pts)) in
  (* List.iter ~f:(fun (_,h)-> printf "%f " h) new_jobs; *)
  (* printf "%d\n" (List.length new_floats); *)
  sample_tree_eu_at_res_offset new_jobs intvl new_floats

let sample_tree_eu_at_res nt intvl = sample_tree_eu_at_res_offset [(nt,intvl)] intvl
                                       [(nt.value.x,nt.value.y,nt.value.z)]

let binary_search nt numpts =
  let epsilon = 1e-5 in
  let rec binary_search' current_guess current_delta =
  if Float.compare current_delta epsilon <= 0 then raise (Invalid_argument "Help!") else
    let l = sample_tree_eu_at_res nt current_guess in
    let i = Int.compare (List.length l) numpts in
    if i > 0 then binary_search' (current_guess +. current_delta) (current_delta /. 2.)
    else if i < 0 then binary_search' (current_guess -. current_delta) (current_delta /. 2.)
    else l in
  let h = neuron_height nt in
  let init_guess = h /. (Float.of_int numpts) in
  binary_search' init_guess (init_guess /. 2.)

(* Delete the smallest trees from the list until
   1. at most proportion p is remaining
   2. at most 50 elements are remaining
   Return the list in sorted order
 *)
let largest_trees forest fraction cap =
  let l1 = List.map forest ~f:(fun a -> (a, neuron_length a)) in
  let l2 = List.sort l1 ~compare:(fun (_,a) (_,b) -> Float.compare a b) in
  let total_length = List.sum (module Float) l2 ~f:(fun (_,a) ->a) in
  let p = (1.  -. fraction) *. total_length in
  let rec trim l2 acc =
    match l2 with
    | (hd, a) :: tl ->
       let b = a +. acc in
       if Float.compare b p <= 0 then trim tl b else (hd, a) :: tl
    | [] -> [] in
  let l3 = trim l2 0.0 in
  let length = List.length l3 in
  let l4 = if length > cap then List.drop l3 (length - cap ) else l3 in
  List.rev (List.map l4 ~f:(fun (a,_) -> a))

let binary_search_forest forest numpts =
  let forest = largest_trees forest 0.98 numpts in
  let epsilon = 1e-9 in
  (* Core.printf "NUM TREES: %d\n" (List.length forest); *)

  let rec binary_search' current_guess current_delta =
    (* Core.printf "CURRENT GUESS : %f\n" current_guess; *)
  if Float.compare current_delta epsilon <= 0 then raise (Invalid_argument "Help!") else
    let l =
      List.map ~f:(fun nt -> sample_tree_eu_at_res nt current_guess) forest
      |> List.concat
    in

    (* printf "NUM %d\n" (List.length forest); *)
    (* printf "NUM PTS : %d\n" (List.length l); *)
    (* printf "%f\n" current_guess; *)
    let i = Int.compare (List.length l) numpts in
    if i > 0 then binary_search' (current_guess +. current_delta) (current_delta /. 2.)
    else if i < 0 then binary_search' (current_guess -. current_delta) (current_delta /. 2.)
    else l in
  let h =
    List.fold ~f:Float.max ~init:0.0 (List.map ~f:neuron_height forest) in
  let init_guess = h +. 0.1 (* /. (Float.of_int numpts) *) in
  (* printf "HEIGHT: %f\n" h; *)
  binary_search' init_guess (init_guess /. 2.)
