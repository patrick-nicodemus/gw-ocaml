open Bigarray

let k_of_i ~n ~i =
  n * i - (i * (i+1))/2

let vectorform_coords ~n ~i ~j = 
  assert (j > i);
  (k_of_i ~n ~i) + j - i - 1

let read_dir (readfile : string -> 'a) (dir : string) =
  let filenames = Sys.readdir dir in
  let pipeline a = a |> Filename.concat dir |> readfile in
  Base.Sequence.map ~f:pipeline (Base.Array.to_sequence filenames)

let read_dir_i (readfile : int -> string -> 'a) (dir : string) (n : int option)=
  let filenames =
    match n with
    | None -> Sys.readdir dir
    | Some k -> Array.sub (Sys.readdir dir) 0 k
  in
  let pipeline i a = a |> Filename.concat dir |> readfile i in
  Base.Sequence.mapi ~f:pipeline (Base.Array.to_sequence filenames)

let bigarray_of_npy filename =
  Npy.read_mmap2 filename ~shared:false
  |> Npy.to_bigarray2 c_layout Float64
  |> function
    | None -> raise (Invalid_argument "Help")
    | Some bigarray ->
       genarray_of_array2 bigarray

let copy_to index filename bigarray () =
  Genarray.blit
  (bigarray_of_npy filename)
  (Genarray.slice_left bigarray [| index |] )

let create_gen_2d x y =
  Genarray.create Float64 c_layout [| x ; y |]

let read_npy_pt_clouds dirname (n : int option) =
  let file_names =
    let a = Sys.readdir dirname in
    match n with
    | None -> a 
    | Some k -> Array.sub a 0 k
  in
  let first_file = (Filename.concat dirname (file_names.(0))) in
  let bigarray0 = (bigarray_of_npy first_file) in
  let num_pts = Genarray.nth_dim bigarray0 0 in
  let spatial_dim = Genarray.nth_dim bigarray0 1 in
  let pt_cloud_array =
    Genarray.create Float64 c_layout [| Array.length file_names ; num_pts; spatial_dim |]
  in
  read_dir_i (fun i a -> copy_to i a pt_cloud_array ()) dirname n |>
    (fun s -> Base.Sequence.iter s ~f:(fun () -> ()));
  pt_cloud_array

let uniform k = Genarray.init Float64 c_layout [| k |] (fun _ -> 1.0 /. (float_of_int k ))

type params = { rho1 : float; rho2 : float; epsilon : float;
               exp_absorb_cutoff : float;
               tol_dykstra : float;
               tol_sinkhorn : float;
               tol_outerloop : float
             }

let unbalanced_gw_pairwise_f64 ctx pt_cloud_array params =
  let open Scaling_unbalanced in
  let num_pt_clouds = Genarray.nth_dim pt_cloud_array 0 in
  let m = (num_pt_clouds * (num_pt_clouds-1))/2 in 
  let main_input = Array_f64_3d.v ctx pt_cloud_array in
  let results =
    unbalanced_gw_pairwise ctx main_input
      params.rho1 params.rho2 params.epsilon params.exp_absorb_cutoff
      params.tol_dykstra params.tol_sinkhorn params.tol_outerloop
  in
  let output = Genarray.create Float64 c_layout [| m |] in
  let () = Array_f64_1d.values results output in
  let return = Array.init m (fun index -> (Genarray.get output [| index |])) in
  return

type params2 = { rho1 : float; rho2 : float; epsilon : float;
                 max_cbar_val : float;
                 inner_count : int;
                 tol_outerloop : float
             }

let unbalanced_gw_pairwise_v2 ctx pt_cloud_array params =
  let open Scaling_unbalanced in
  let num_pt_clouds = Genarray.nth_dim pt_cloud_array 0 in
  let m = (num_pt_clouds * (num_pt_clouds-1))/2 in
  let main_input = Array_f64_3d.v ctx pt_cloud_array in
  let results =
    unbalanced_gw_pairwise_v2 ctx main_input
      params.rho1 params.rho2 params.epsilon params.max_cbar_val
      (Int32.of_int params.inner_count) params.tol_outerloop
  in
  let output = Genarray.create Float64 c_layout [| m; 5 |] in
  let () = Array_f64_2d.values results output in
  let return = Array.init m (fun index -> (Genarray.get output [| index; 4 |])) in
  return

let write_to_file output_filename arr =
  let out_channel = open_out output_filename in
  Array.iter
    (fun a -> Printf.fprintf out_channel "%f\n" a) arr
  ;
    close_out out_channel

let pdist m =
  let open Owl_dense_matrix_d in
  let rnm = (row_num m) in
  let a = zeros rnm rnm in
  for i = 0 to rnm - 1 do
    for j = i + 1 to rnm - 1 do
      set a i j (l2norm' (sub (row m i ) (row m j)))
      done
      (* set a i j (l2norm' (sub (row m i) (row m j))) *)
  done;
    for i = 0 to rnm - 1 do
      for j = 0 to i - 1 do
        set a i j (get a j i)
        (* set a i j (get a j i) *)
      done
    done;
    a;;

let unbalanced_gw_f64_increasing_eps ctx rho1 rho2 (eps: float)
      (x  : Scaling_unbalanced.Array_f64_2d.t) mu y nu
      ~exp_absorb_cutoff ~tol_dykstra ~tol_sinkhorn tol_outerloop =
  let rec unbalanced_gw_f64_helper ctx rho1 rho2 (eps : float) x mu y nu ~exp_absorb_cutoff ~tol_dykstra ~tol_sinkhorn tol_outerloop =
    let result = Scaling_unbalanced.unbalanced_gw_total_cost
                   ctx rho1 rho2 eps x mu y nu exp_absorb_cutoff
                   tol_dykstra tol_sinkhorn tol_outerloop in
    Printf.printf "%f\n" eps;
        if Float.is_nan result || result <= 0.0 then unbalanced_gw_f64_helper ctx rho1 rho2 (eps *. 1.04) x mu y nu ~exp_absorb_cutoff ~tol_dykstra ~tol_sinkhorn tol_outerloop
    else result
  in
  unbalanced_gw_f64_helper ctx rho1 rho2 eps x mu y nu ~exp_absorb_cutoff ~tol_dykstra ~tol_sinkhorn tol_outerloop
 
let unbalanced_gw_f64_ptclouds ctx filename1 filename2 (params : params) =
  let open Scaling_unbalanced in
  let x1 = pdist (bigarray_of_npy filename1)  in
  let u = uniform (Genarray.nth_dim x1 0) |> Array_f64_1d.v ctx in
  let x2 = pdist (bigarray_of_npy filename2) in
  let return = unbalanced_gw_total_cost ctx
      params.rho1 params.rho2 params.epsilon
      (Array_f64_2d.v ctx x1) u
      (Array_f64_2d.v ctx x2) u
      params.exp_absorb_cutoff
      params.tol_dykstra
      params.tol_sinkhorn
      params.tol_outerloop
  in
  return
