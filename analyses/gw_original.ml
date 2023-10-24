open Swc.Batch
open Core
open Gw_partial
module Mat = Owl_dense_matrix_d

let sequence_dir = "/home/patn/ocaml/gw_partial/analyses/100samplepts/"

let args = Sys.get_argv ()

let num_sample_pts = 100

let numfiles = Int.of_string args.(1)

(* let output_matrix_file_name = "/home/patn/ocaml/gw_partial/analyses/" ^ *)
(*                                 (Int.to_string num_sample_pts) ^ "samplepts" ^ *)
(*                                   (Int.to_string numfiles) ^ "files.txt" *)

let pt_clouds = read_dir Mat.load_npy sequence_dir |> (fun a -> Sequence.take a numfiles)

(* let names = Sys_unix.readdir sequence_dir *)

let icdms = pt_clouds
            |> Sequence.map ~f:Bigarray.array2_of_genarray
            |> Sequence.to_array

let u = Gw.uniform_dist num_sample_pts

let gw_by_index gw_dmat i j () =
  let (a, _) = Gw.gw_uniform icdms.(i) u icdms.(j) u in
  let () = Bigarray.Genarray.set gw_dmat [|i; j|] a in
  Bigarray.Genarray.set gw_dmat [|j; i|] a

(* module T = Domainslib.Task *)

(* let gw_parallel pool gw_dmat n = *)
(*   T.parallel_for pool ~start:0 ~finish:n ~body:( *)
(*       fun i -> *)
(*       for j = i + 1 to n - 1 do *)
(*         gw_by_index gw_dmat i j () *)
(*       done *)
(*     ) *)


let n = Array.length icdms
let () = printf "%d\n" n

let gw_dmat = Mat.zeros n n

let start = Core_unix.gettimeofday ()

(* let () = *)
(*   let pool = T.setup_pool ~num_domains:(14) () in *)
(*   T.run pool (fun _ -> gw_parallel pool gw_dmat n); *)
(*   T.teardown_pool pool; *)
(*   Mat.save_npy  ~out:output_matrix_file_name gw_dmat *)

let () =
  for i=0 to n - 1 do
    for j = i + 1 to n - 1 do
      gw_by_index gw_dmat i j ()
    done
  done

let _ = Out_channel.printf "%f\n" (Core_unix.gettimeofday () -. start)

(* let () = *)
(*   let out = Out_channel.create "/home/patn/ocaml/gw_partial/analyses/filenames.txt" in *)
(*   Array.iter ~f:(fun s -> Out_channel.fprintf out "%s\n" s) names; *)
(*   Out_channel.close out;; *)
