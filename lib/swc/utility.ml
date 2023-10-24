open Core
(* open Random *)

(* let () = Random.self_init () *)

(* let random_elt l = *)
(*   let sum = List.fold ~init:0. ~f:Float.add l in *)
(*   let rr = Random.float sum in *)
(*   let rec random_elt' l acc_float acc_int = *)
(*     match l with *)
(*     | x :: a -> *)
(*       let x' = x +. acc_float in *)
(*       if Float.compare rr x' < 0 then acc_int else *)
(*         random_elt' a x' (acc_int + 1) *)
(*     | [] -> raise (Invalid_argument "Uh oh.") *)
(*   in *)
(*   random_elt' l 0.0 0 *)

let ordered_pairs seq =
  let seq_of_seqs seq =
  Sequence.unfold_step
    ~init:seq
    ~f:(fun s ->
      let open Sequence.Step in
      match Sequence.next s with
      | Some (elt, tail) ->
         Yield { value =
                   Sequence.map ~f:(fun x -> elt, x) tail ;
                 state = tail }
      | None -> Done) in
  Sequence.interleave (seq_of_seqs (Sequence.memoize seq))
