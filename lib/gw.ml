open Transport
open Bigarray
open Owl
open Owl_dense_matrix_d    
    

(* let gw_cost (a_dmat : ((float,  float64_elt, c_layout) Array2.t)) *)
(*     (a_pdist : ((float,  float64_elt, c_layout) Array1.t)) *)
(*     (b_dmat : ((float,  float64_elt, c_layout) Array2.t)) *)
(*     (b_pdist : ((float,  float64_elt, c_layout) Array1.t)) *)
(*     (coupling : ((float,  float64_elt, c_layout) Array2.t)) *)
(*   = let aa = Mat.dot a_dmat a_pdist in aa *)

type matrix =(float,  float64_elt, c_layout) Array2.t

type vector = (float,  float64_elt, c_layout) Array1.t
let (@) x y = Mat.dot x y             
let cdot x y = Mat.get ((Mat.transpose x) @ y) 0 0

let gw_cost (a_dmat : matrix)
    (a_pdist : vector)
    (b_dmat : matrix)
    (b_pdist : vector)
    (coupling : matrix)
  =
  let frobenius x y = Mat.sum' (Mat.mul x y) in
  let a_dmat' = (genarray_of_array2 a_dmat) in
  let a_pdist' = (genarray_of_array1 a_pdist) in
  let b_dmat' =(genarray_of_array2 b_dmat) in
  let b_pdist' = (genarray_of_array1 b_pdist) in
  let coupling' = (genarray_of_array2 coupling) in    
  let aa = a_dmat' @ a_pdist' in
  let bb = b_dmat'  @ b_pdist' in
  (cdot aa aa) +. (cdot bb bb) -. (2. *. (frobenius (a_dmat' @ coupling' @ b_dmat') coupling'))

let cost_matrix a_dmat b_dmat coupling =
  Mat.scalar_mul (-2.)
    (Mat.dot (Mat.dot (genarray_of_array2 a_dmat) (genarray_of_array2 coupling))
                (genarray_of_array2 b_dmat))

let gw_new_coupling
    (a_dmat : matrix)
    (a_pdist : vector)
    (b_dmat : matrix)
    (b_pdist : vector)
    (init_coupling : matrix)
  =
  let c = cost_matrix a_dmat b_dmat init_coupling in
  kantorovich ~x:a_pdist ~y:b_pdist ~d:(array2_of_genarray c) ~num_iter:20000

let gw_init_coupling a_dmat a_pdist b_dmat b_pdist init_coupling =
  let init_cost = gw_cost a_dmat a_pdist b_dmat b_pdist init_coupling in
  let rec gw_init_coupling a_dmat a_pdist b_dmat b_pdist current_coupling current_cost =
    let result = gw_new_coupling a_dmat a_pdist b_dmat b_pdist init_coupling in
    match result with
    | Optimal { cost = _ ; coupling = coupling ; u = _ ; v = _ } ->
      let new_cost = gw_cost a_dmat a_pdist b_dmat b_pdist coupling in
      if new_cost <= 0. || new_cost >= 0.99999 *. current_cost then (current_cost, current_coupling)
      else gw_init_coupling a_dmat a_pdist b_dmat b_pdist coupling new_cost
    | _ -> raise (Invalid_argument "Borked")
  in
  gw_init_coupling a_dmat a_pdist b_dmat b_pdist init_coupling init_cost
    
let uniform_coupling (a_pdist : vector) (b_pdist : vector) =
  let a_pdist' = (genarray_of_array1 a_pdist) in
  let b_pdist' = (genarray_of_array1 b_pdist) in
  array2_of_genarray (a_pdist' @ (Mat.transpose b_pdist'))

let gw_uniform a_dmat a_pdist b_dmat b_pdist  =
  gw_init_coupling a_dmat a_pdist b_dmat b_pdist (uniform_coupling a_pdist b_pdist)


(* let sinkhorn v w m = *)
  
  
