open Owl_dense_matrix_d

let mat_of_tuple (x, y, z) = init 1 3
                               (function
                                | 0 -> x
                                | 1 -> y
                                | _ -> z)
let mat_of_tuple_list l =
    List.map mat_of_tuple l |>
      Array.of_list |>
      of_rows

let cdist m n =
  assert (col_num m = col_num n);
  let rnm = (row_num m) in
  let rnn = (row_num n) in
  init_2d rnm rnn
    (fun i j ->
      l2norm' (sub (row m i) (row n j)));;
    
let pdist m =
  let rnm = (row_num m) in
  let a = zeros rnm rnm in
  for i = 0 to rnm - 1 do
    for j = i + 1 to rnm - 1 do
      set a i j (l2norm' (sub (row m i) (row m j)))
    done
  done;
    for i = 0 to rnm - 1 do
      for j = 0 to i - 1 do
        set a i j (get a j i)
      done
    done;
    a;;

let icdm tuple_list = pdist (mat_of_tuple_list tuple_list)
