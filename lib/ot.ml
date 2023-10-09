open Owl
(* open Owl_dense_ndarray_generic *)
open Owl_dense_matrix_d
(* open Ndarray *)

(* Convention: given two metric measure spaces a = (a_dmat, a_dist)
   and b=(b_dmat, b_dist), where a_dist has length n and b_dist has length m,
   a coupling from a to b is an nxm matrix whose sum along axis = 1 is a_dist
   and whose sum along axis 0 is b_dist.
 *)

module Bothfree = struct
  
let gradient a_dmat a_dist b_dmat b_dist coupling lambda_1 lambda_2 =
  let n = row_num a_dmat in
  let m = col_num b_dmat in
  let u = (sum ~axis:1 coupling) in
  let v = transpose (sum ~axis:0 coupling) in
  mul_scalar
    (List.fold_left Mat.add
    (Mat.tile (Mat.dot (sqr a_dmat) u) [| 1; m |])
    [ Mat.tile (transpose (Mat.dot (sqr b_dmat) v)) [| n; 1 |];
      mul_scalar (dot (dot a_dmat coupling) b_dmat) (-2.0);
      repeat (mul_scalar (sub u a_dist) lambda_1) [| 1; m |];
      repeat (mul_scalar (transpose (sub v b_dist)) lambda_2) [| n; 1 |];
    ]) 2.0
  
let gw_loss a_dmat a_dist b_dmat b_dist coupling lambda_1 lambda_2 =
  let u = (sum ~axis:1 coupling) in
  let v = transpose (sum ~axis:0 coupling) in
  let a_dmat_sq = mul a_dmat a_dmat in
  let b_dmat_sq = mul b_dmat b_dmat in
  (get (dot (transpose (dot a_dmat_sq u)) u) 0 0) +.
    (get (dot (transpose (dot b_dmat_sq v)) v) 0 0) +.
    (-2. *. (sum' (mul (dot (dot a_dmat coupling) b_dmat) coupling))) +.
    lambda_1 *. (ssqr_diff' (copy a_dist) u) +.
    lambda_2 *. (ssqr_diff' (copy b_dist) v)
end
