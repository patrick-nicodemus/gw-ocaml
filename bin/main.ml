open Ugw_futhark
open Ugw_futhark.Utilities

let () = Cli.get_inputs ()

let rho1 = !Cli.rho1
let rho2 = !Cli.rho2
let eps = !Cli.epsilon
let file_dir = !Cli.ptcloud_dir
let output_file = !Cli.output_file


let num_pt_clouds =
  match !Cli.num_pt_clouds with
  | Some n -> n
  | None -> Array.length (Sys.readdir file_dir)
(* let num_pt_clouds = 20 *)
let ctx = Scaling_unbalanced.Context.v
            ~debug:false ~log:false ~profile:false ~auto_sync:false ()
let pt_cloud_array = read_npy_pt_clouds file_dir (!Cli.num_pt_clouds)
let u = uniform 60 |> Scaling_unbalanced.Array_f64_1d.v ctx
let t0 = Unix.gettimeofday ()

(* let output = unbalanced_gw_pairwise_f64 ctx pt_cloud_array *)
(*                { rho1; rho2; epsilon = eps; *)
(*                  max_cbar_val = 30.; *)
(*                  inner_count = 30; *)
(*                  tol_outerloop = 0.00001; *)
(*                } *)

let output = unbalanced_gw_pairwise_f64 ctx pt_cloud_array
               { rho1; rho2; epsilon = eps;
                 exp_absorb_cutoff = 1e30;
                 tol_dykstra = 30.;
                 tol_sinkhorn = 0.001;
                 tol_outerloop = 0.00001;
               }


let () = Printf.printf "%f\n" (Unix.gettimeofday () -. t0)

let ctr = ref 0

let () =
  let open Scaling_unbalanced in
  for i = 0 to num_pt_clouds-1 do
    for j = i + 1 to num_pt_clouds-1 do
      let index = vectorform_coords ~n:num_pt_clouds ~i ~j in
      while Float.is_nan (output.(index)) || output.(index) <= 0.0 do
        let pt_cloud_1 = Array_f64_2d.v ctx
                           (pdist
                              (Bigarray.Genarray.slice_left pt_cloud_array [| i |] )) in
        let pt_cloud_2 = Array_f64_2d.v ctx
                           (pdist
                              (Bigarray.Genarray.slice_left pt_cloud_array [| j |] )) in
        let () = (output.(index) <-
                    (unbalanced_gw_f64_increasing_eps
                       ctx rho1 rho2 eps pt_cloud_1 u pt_cloud_2 u
                       ~exp_absorb_cutoff:1.0e10
                       ~tol_dykstra:8.0
                       ~tol_sinkhorn:0.00001
                       0.00001)) in
        let () = ctr := !ctr + 1 in
        let () = Array_f64_2d.free pt_cloud_1 in
        let () = Array_f64_2d.free pt_cloud_2 in
        ()
        done
    done
  done

let () = Printf.printf "%d\n" !ctr
let () = write_to_file output_file output
