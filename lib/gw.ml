open Transport
open Bigarray
open Owl

type matrix =(float,  float64_elt, c_layout) Array2.t

type vector = (float,  float64_elt, c_layout) Array1.t

let uniform_dist n = let a = Array1.create Float64 C_layout n in
                     let () = Array1.fill a (1. /. Float.of_int n) in
                     a

let (@) x y = Mat.dot x y             
(* let cdot x y =  *)
(*   Owl_dense_ndarray_d.get (Owl_dense_ndarray_d.contract2 [| (0, 0) |] x y) [| |] *)
  (* let n = Mat.col_num x in *)
  (* let x' = Mat.reshape x [| 1 ; n |] in *)
(* Mat.get (x' @ y) 0 0 *)

let frobenius x y =
  let t = (Owl_dense_ndarray_d.contract2 [| (0, 0); (1, 1) |] x y) in
  Owl_dense_ndarray_d.get t [| |]

let c_a a_dmat a_pdist =
    let a_dmat' = (genarray_of_array2 a_dmat) in
    let a_pdist' = reshape (genarray_of_array1 a_pdist) [| (Array1.dim a_pdist) ; 1 |] in
    frobenius ((Mat.mul a_dmat' a_dmat') @ a_pdist') a_pdist'

let gw_cost (a_dmat : matrix) (b_dmat : matrix)
      (coupling : matrix) (c_A : float) (c_B : float) =
  (* let frobenius x y = Mat.sum' (Mat.mul x y) in *)
  let a_dmat' = (genarray_of_array2 a_dmat) in
  let b_dmat' =(genarray_of_array2 b_dmat) in
  let coupling' = (genarray_of_array2 coupling) in
  c_A +. c_B -. (2. *. (frobenius (a_dmat' @ coupling' @ b_dmat') coupling'))
    
let cost_matrix a_dmat b_dmat coupling c_A c_B =
  Mat.add_scalar (Mat.scalar_mul (-2.)
    (Mat.dot (Mat.dot (genarray_of_array2 a_dmat) (genarray_of_array2 coupling))
       (genarray_of_array2 b_dmat))) (c_A +. c_B)

let gw_new_coupling
    (a_dmat : matrix)
    (a_pdist : vector)
    (b_dmat : matrix)
    (b_pdist : vector)
    (c_A : Mat.elt)
    (c_B : Mat.elt)
    (init_coupling : matrix)
  =
  let c = cost_matrix a_dmat b_dmat init_coupling c_A c_B in
  kantorovich ~x:a_pdist ~y:b_pdist ~d:(array2_of_genarray c) ~num_iter:200000

let gw_init_coupling a_dmat a_pdist b_dmat b_pdist init_coupling =
  let c_A = c_a a_dmat a_pdist in
  let c_B = c_a b_dmat b_pdist in
  let init_cost = gw_cost a_dmat b_dmat init_coupling c_A c_B in
  let rec gw_init_coupling' current_coupling current_cost =
    let result = gw_new_coupling a_dmat a_pdist b_dmat b_pdist c_A c_B current_coupling in
    match result with
    | Optimal { cost = _ ; coupling = coupling ; u = _ ; v = _ } ->
      let new_cost = gw_cost a_dmat b_dmat coupling c_A c_B in
      if new_cost <= 0. || new_cost >= current_cost then (current_cost, current_coupling)
      else gw_init_coupling' coupling new_cost
    | _ -> raise (Invalid_argument "Borked")
  in
  gw_init_coupling' init_coupling init_cost

let uniform_coupling (a_pdist : vector) (b_pdist : vector) =
  let n = Array1.dim a_pdist in
  let m = Array1.dim b_pdist in
  let a_pdist' = reshape (genarray_of_array1 a_pdist) [| n; 1 |] in
  let b_pdist' = reshape (genarray_of_array1 b_pdist) [| m; 1 |] in
  array2_of_genarray (a_pdist' @ (Mat.transpose b_pdist'))

let gw_uniform a_dmat a_pdist b_dmat b_pdist  =
  gw_init_coupling a_dmat a_pdist b_dmat b_pdist (uniform_coupling a_pdist b_pdist)


(* let sinkhorn v w m = *)
