open Owl_dense_matrix_d
let random_matrix_uniform n m = init_2d n m (fun _ _ -> Owl_stats.std_uniform_rvs ())

let circle n =
  let a = linspace 0. 1. n in
  let sina = sin a in
  let cosa = cos a in
  concat_vertical sina cosa
