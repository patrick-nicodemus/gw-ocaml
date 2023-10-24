open Core
open Stdio

module Read_swc : sig
  val seq_of_swc : string -> Swc_core.node Sequence.t
  (* val list_of_swc : string -> (int * Swc_core.node) list *)
end =
struct
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
    let open Swc_core in
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

  (* let list_of_swc filename = *)
  (*   let data = In_channel.read_lines filename in *)
  (*   List.filter_map ~f:( *)
  (*     fun s -> if String.is_prefix s ~prefix:"#" || String.is_empty s then None else *)
  (*         Some (let r = parse_line s in (r.id, r))) data *)

end
open Read_swc

(* module type TreeBase = sig *)
(*   (\* type a *\) *)
(*   type t *)
(*   val children : t -> t list *)
(*   val compare : t -> t -> int *)

(* end *)

(* module Tree (S : TreeBase) = struct *)
(*   type ternary = *)
(*     | Pos *)
(*     | Zero *)
(*     | Neg *)

(*   let ternary_of k = *)
(*     if k = 0 then Zero *)
(*     else if k > 0 then Pos *)
(*     else Neg   *)

(*   let treecompare (a : S.t) (b: S.t)  = *)
(*   let rec treecompare_acc_list l1 l2 acc = *)
(*     match l1, l2 with *)
(*     | _ :: _, [] -> 1 *)
(*     | [], _ :: _ -> -1 *)
(*     | hdx :: tlx, hdy :: tly -> treecompare_acc hdx hdy ((tlx, tly) ::  acc) *)
(*     | [], [] -> (match acc with *)
(*         | (l1' , l2') :: acc' -> treecompare_acc_list l1' l2' acc' *)
(*         | [] -> 0) *)
(*   and *)
(*     treecompare_acc a b acc = *)
(*     match ternary_of (S.compare a b) with *)
(*     | Zero -> treecompare_acc_list (S.children a) (S.children b) acc *)
(*     | Pos -> 1 *)
(*     | Neg -> -1 *)
(*   in treecompare_acc a b [] *)
(* end *)

let swc_forest_filename filename =
  let seq = seq_of_swc filename in
  Swc_core.swc_forest_of_seq seq


