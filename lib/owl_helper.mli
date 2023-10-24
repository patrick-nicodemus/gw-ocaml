open Owl_dense_matrix_d

(* Returns a 1x3 matrix. *)
val mat_of_tuple : float * float * float -> mat

(* Converts a list of n ordered triples (x_i,y_i,z_i) into an nx3 matrix. *)
val mat_of_tuple_list : (float * float * float) list -> mat

(* Takes an k x n matrix A and a p x n matrix B and returns a k x p
   matrix whose (i,j)-th entry is the distance between row i of A and
   row j of B.  *)
val cdist : mat -> mat -> mat

(* Returns pairwise distances between rows of cdist. *)
val pdist : mat -> mat

(* Pairwise distances between all points in a list of ordered triples. *)
val icdm : (float * float * float) list -> mat

