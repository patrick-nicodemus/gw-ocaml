open Filename
open Core
open Swc.Batch
open Swc.Parse
(* open Swc.Utility *)

module Mat = Owl_dense_matrix_d

let nts = read_dir swc_forest_filename "/home/patn/recon/swc"
let file_names = Array.to_sequence (Sys_unix.readdir "/home/patn/recon/swc")
                 |> Sequence.map ~f:(remove_extension)

let filter_to_dendrites swcforest =
  let f = Swc.Swc_core.filter_map (fun (a : Swc.Swc_core.node) -> match a.nodetype with
                                        | 1 | 3 | 4 -> Some a
                                        | _ -> None) in
  List.concat (List.map ~f:f swcforest) 

let num_sample_pts = 100

let icdm forest numpts =
  let pruned_forest = filter_to_dendrites forest in
  let tuplelist = Swc.Sample.binary_search_forest pruned_forest numpts in
  let open Gw_partial.Owl_helper in
  pdist (mat_of_tuple_list tuplelist)

let icdms = Sequence.map nts ~f:(fun a -> icdm a num_sample_pts)

let () =
  Sequence.iter (Sequence.zip file_names icdms)
    ~f:(fun (file_name, icdm0) ->
      Mat.save_npy  ~out:("/home/patn/ocaml/gw_partial/analyses/100samplepts/" ^
                            ( file_name)) icdm0)
