open Core
open Swc.Batch
open Swc.Parse
open Swc.Utility
open Gw_partial

module Mat = Owl_dense_matrix_d

let nts = read_dir swc_forest_filename "/home/patn/recon/swc100"

(* let forest = match (Sequence.next nts) with *)
(*   | Some (forest, _) -> forest *)
(*   | _ -> raise (Invalid_argument "!") *)

let icdm forest numpts =
  let tuplelist = Swc.Sample.binary_search_forest forest numpts in
  let open Owl_helper in
  icdm tuplelist

(* let%test _ = (Mat.row_num (icdm forest 50) = 50) *)

let icdms = Sequence.map nts ~f:(fun a -> icdm a 50)
            |> ordered_pairs

let%test _ =
  (List.hd_exn (Sequence.map nts ~f:(fun a -> icdm a 50) |> Sequence.to_list) |>
    Mat.row_num) = 50
            (* |> Sequence.map ~f:Bigarray.array2_of_genarray *)

