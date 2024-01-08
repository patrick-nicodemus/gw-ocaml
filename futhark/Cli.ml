let usage_msg = "unbalanced_gw -rho1 <rho1> -rho2 <rho2> -eps <epsilon> [-n <num>] -i <ptcloud_dir> -o <output_file>"

let rho1 = ref 0.0
let rho2 = ref 0.0
let num_pt_clouds= ref (None : int option)
let epsilon = ref 0.0
let ptcloud_dir = ref ""
let output_file = ref ""

(* let set_ptcloud_dir input_folder = ptcloud_dir := input_folder *)

let speclist =
  [
    ("-rho1", Arg.Set_float rho1, "Set KL divergence penalty coefficient for first space");
    ("-rho2", Arg.Set_float rho2, "Set KL divergence penalty coefficient for second space");
    ("-eps", Arg.Set_float epsilon, "Set entropy coefficient for regularization");
    ("-n", Arg.Int (fun n -> num_pt_clouds := Some n) , "Set number of files to be processed");
    ("-i", Arg.Set_string output_file, "Set input directory name");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let get_inputs () = Arg.parse speclist (fun _ -> ()) usage_msg
