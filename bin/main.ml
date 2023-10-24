open Swc.Parse
open Swc.Batch

let dir = Sys.argv.(1)    

let neurons = read_dir swc_forest_filename dir

let () = Core.Sequence.iter neurons ~f:(fun a ->  let _ = Swc.Swc_core.length a in ())



(* let f neuron  = *)
(*   let x = Unix.gettimeofday () in *)
(*   let _ = NeuronTree.height neuron in *)
(*   let y = Unix.gettimeofday () in *)
(*   (y -. x) *)

(* let hashtbl_seq = map_over_dir Read_swc.seq_of_swc *)
(*         hash_of_seq dir *)
(* let () = Core.Sequence.iter hashtbl_seq ~f:(fun a -> *)
(*     let ell = Core.Hashtbl.keys a in *)
(*     Printf.printf "%d\n" (List.hd ell)) *)

(* let () = print_endline "Hello, World!" *)

(* NeuronTree.height *)

(* let x = Core.Sequence.fold map_seq ~f:(+.) ~init:0. *)
(* let () = Printf.printf "%f\n" x *)


(* let () = Core.Sequence.iter map_seq ~f:(Printf.printf "%f\n") *)
(* let () = Core.Sequence.iter map_seq ~f:(fun _ -> ()) *)

(* let hashtbl_seq = map_over_dir Read_swc.list_of_swc *)
(*         (Core.Hashtbl.of_alist_exn ~growth_allowed:true ~size:100 (module Core.Int)) dir *)


(* let () = Core.Sequence.iter map_seq ~f:(fun a -> *)
(*     match (Core.Map.min_elt a) with *)
(*     | Some (k, _)-> Printf.printf "%d\n" k *)
(*     | None -> print_endline "NULL\n") *)

