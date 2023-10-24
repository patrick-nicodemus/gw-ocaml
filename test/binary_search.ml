open Core
open Swc.Parse
open Swc.Sample
open Swc.Batch
open Swc.Swc_core

let forest = swc_forest_filename "/home/patn/recon/swc/313861608.swc"

let%test _ =
  let tree = List.hd_exn forest in
  let ht = neuron_height tree in
  let pts = sample_tree_eu_at_res tree (ht +. 0.01) in
  List.length pts = 1

let%test _ = List.length (binary_search_forest forest 50) = 50


let nts = read_dir swc_forest_filename "/home/patn/recon/swc"
let%test _ =
  Sequence.for_all nts
    ~f:(fun a -> List.length (binary_search_forest a 100) = 100)
