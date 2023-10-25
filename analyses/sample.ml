open Filename
open Core

module Mat = Owl_dense_matrix_d


module WithPruning = struct

  (* open Swc.Sample_cli *)

  let no_pruning_dir = "no_pruning"

  let output_dirs output_dir floatstrlist =
    List.map 
      (no_pruning_dir :: (List.map floatstrlist ~f:(fun s -> String.substr_replace_all s  ~pattern:"." ~with_:"_")))
      ~f:(fun a -> Filename.concat output_dir a)

end

let () = Arg.parse (Swc.Sample_cli.speclist) (fun _ -> ()) Swc.Sample_cli.usage_msg



(* Create the output directory. *)
let () =
  let open Swc.Sample_cli in
  match Sys_unix.file_exists !outputdir with
  | `Yes -> if (Array.length (Sys_unix.readdir !outputdir) > 0) then raise (Invalid_argument "Directory exists and is non-empty.")
            else ()
  | _ -> Core_unix.mkdir !outputdir

let prune_dirnames = match !Swc.Sample_cli.prune with
  | None -> None
  | Some floatstringlist -> Some (List.map floatstringlist ~f:(fun s -> String.substr_replace_all s  ~pattern:"." ~with_:"_"))

let () =
  match prune_dirnames with
  | None -> ()
  | Some floatstringlist -> List.iter ~f:(fun s -> Core_unix.mkdir (Filename.concat !Swc.Sample_cli.input_dir s)) floatstringlist;
                            Core_unix.mkdir (Filename.concat !Swc.Sample_cli.input_dir no_pruning_dir)

let file_names =
  let open Swc.Sample_cli in
  Array.to_sequence (Sys_unix.readdir !input_dir)
  |> Sequence.map ~f:(remove_extension)
  |> (fun a -> match !numfiles with
               | None -> a
               | Some n -> Sequence.take a n)

let filter_to_dendrites ok_node_types swcforest =
  let open Swc.Swc_core in   
  let f = filter_map (fun (a : node) -> if (List.mem ok_node_types a.nodetype ~equal:Int.equal) then Some a else None) in
  List.concat (List.map ~f:f swcforest)

let swcs =
  let open Swc.Sample_cli in
  let swcs0 = Swc.Batch.read_dir (Swc.Parse.swc_forest_filename) !input_dir in
  let swcs1 = match !numfiles with
    | None -> swcs0
    | Some n -> Sequence.take swcs0 n in
  match !dendrites with
  | None -> swcs1
  | Some ok_node_types -> swcs1 |> Sequence.map ~f:(filter_to_dendrites ok_node_types)

let () =
  let write_ptclouds_to_file pt_clouds dir =
    Sequence.iter (Sequence.zip file_names pt_clouds)
      ~f:(fun (name, pt_cloud) -> Mat.save_npy ~out:(Filename.concat dir (name ^ ".npy")) pt_cloud)
  in
  let pt_clouds_no_pruning =
    if !Swc.Sample_cli.samplepts > 0 then
      Sequence.map swcs 
        ~f:(fun forest -> Gw_partial.Owl_helper.mat_of_tuple_list (Swc.Sample.binary_search_forest forest !(Swc.Sample_cli.samplepts)))
    else raise (Invalid_argument "-samplepts should be a strictly positive integer.")
  in
  match !Swc.Sample_cli.prune with
  | None ->
     write_ptclouds_to_file pt_clouds_no_pruning !Swc.Sample_cli.outputdir
  | Some floatstrlist ->
     write_ptclouds_to_file pt_clouds_no_pruning (Filename.concat !Swc.Sample_cli.outputdir no_pruning_dir);
     let floatlist = List.map ~f:float_of_string floatstrlist in
     let dirnames = match prune_dirnames with
       | Some a -> a
       | None -> raise (Failure "?")
     in
     
     
     List.iter (List.zip floatlist dirnames)
       ~f:

     
let icdm forest numpts =
       let pruned_forest = filter_to_dendrites forest in
       let tuplelist = Swc.Sample.binary_search_forest pruned_forest numpts in
       let open Gw_partial.Owl_helper in
       pdist (mat_of_tuple_list tuplelist)

let icdms = Sequence.map nts ~f:(fun a -> icdm a num_sample_pts)

let () =
  Sequence.iter (Sequence.zip file_names icdms)
    ~f:(fun (file_name, icdm0) ->
      Mat.save_npy  ~out:("/home/patn/ocaml/gw_partial/analyses/100samplepts/" ^
                            ( file_name)) icdm0)
