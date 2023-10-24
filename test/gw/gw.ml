open Core
open Swc.Batch
open Swc.Parse
(* open Swc.Utility *)
open Gw_partial
module Mat = Owl_dense_matrix_d

let u50 = Gw.uniform_dist 50
let%test _ =
  Mat.row_num (Mat.transpose
               (Mat.reshape (Bigarray.genarray_of_array1 u50) [| 1; 50 |] )) = 50

let nts = read_dir swc_forest_filename "/home/patn/recon/swc100"

let icdm forest numpts =
  let tuplelist = Swc.Sample.binary_search_forest forest numpts in
  let open Owl_helper in
  pdist (mat_of_tuple_list tuplelist)

let t1, s1 = match Sequence.next nts with
  | Some (t1, s1) -> t1, s1
  | None -> raise (Invalid_argument "!")

let t2, s2 = match Sequence.next s1 with
  | Some (t1, s1) -> t1, s1
  | None -> raise (Invalid_argument "!")

let icdm1_gen = (icdm t1 50)
let icdm2_gen = (icdm t2 50)

(* let () = printf "%d %d \n" (Mat.row_num icdm1_gen) (Mat.col_num icdm1_gen); *)
(*          printf "%d %d \n" (Mat.row_num icdm2_gen) (Mat.col_num icdm2_gen) *)

let icdm1 = Bigarray.array2_of_genarray (icdm t1 50)
let icdm2 = Bigarray.array2_of_genarray (icdm t2 50)

let u50 = Gw.uniform_dist 50

let%test _ =
  (* printf "%d %d \n" (Mat.row_num icdm1) (Mat.col_num icdm1); *)
  (* printf "%d %d \n" (Mat.row_num icdm2) (Mat.col_num icdm2);   *)
  let a, _  = Gw.gw_uniform icdm1 u50 icdm2 u50 
               in Float.compare a 0.0 > 0

let u100 = Gw.uniform_dist 100

let pt_cloud1 = Bigarray.array2_of_genarray (Mat.load_npy "/home/patn/ocaml/gw_partial/analyses/100samplepts/526573598")
let pt_cloud2 = Bigarray.array2_of_genarray (Mat.load_npy "/home/patn/ocaml/gw_partial/analyses/100samplepts/473593847")

let c = Gw.uniform_coupling u100 u100
let c_A = Gw.c_a pt_cloud1 u100
let c_B = Gw.c_a pt_cloud2 u100
let () = printf "c_A: %f\n" c_A
let () = printf "c_B: %f\n" c_B

let () = printf "Initial cost: %f\n" (Gw.gw_cost pt_cloud1 pt_cloud2 c c_A c_B)

let gw_init_coupling a_dmat a_pdist b_dmat b_pdist init_coupling =
  let init_cost = Gw.gw_cost a_dmat b_dmat init_coupling c_A c_B in
  let rec gw_init_coupling' a_dmat a_pdist b_dmat b_pdist current_coupling current_cost =
    let result = Gw.gw_new_coupling a_dmat a_pdist b_dmat b_pdist c_A c_B current_coupling in
    match result with
    | Optimal { cost = _ ; coupling = coupling ; u = _ ; v = _ } ->
       let new_cost = Gw.gw_cost a_dmat b_dmat coupling c_A c_B in
       let () = printf "Current cost: %f\n" current_cost in
       let () = printf "New cost: %f\n" new_cost in
       if Float.compare new_cost 0. <= 0 ||
            Float.compare new_cost current_cost >= 0 then (current_cost, current_coupling)
      else gw_init_coupling' a_dmat a_pdist b_dmat b_pdist coupling new_cost
    | _ -> raise (Invalid_argument "Borked")
  in
  gw_init_coupling' a_dmat a_pdist b_dmat b_pdist init_coupling init_cost

let%test _ =
  (* let () = print_endline "Hello, world!" in *)
  let a, _ = gw_init_coupling pt_cloud1 u100 pt_cloud2 u100 c in
  Float.compare a a = 0

let () = print_endline ""
