let usage_msg = "sample.exe -i swcdir -samplepts n [-full] [-dendritesonly] [-bynodeid \"1,3, 4\"] [-numfiles numfiles] -o outputdir [-prune \"p1,p2,...pn\"]"

let input_dir = ref ""
let dendrites = ref (None : (int list) option)
let numfiles = ref (None : int option)
let outputdir = ref ""
let prune = ref (None : string list option)
let samplepts = ref 0


let speclist = [
    ( "-i", Arg.String (fun str -> input_dir := str), "Input directory to read swcs from");
    ("-samplepts", Arg.Set_int samplepts, "How many points to sample from each neuron");
    ("-full", Arg.Unit (fun () -> dendrites := None), "Sample whole neuron");
    ("-dendritesonly", Arg.Unit (fun () -> dendrites := Some [1; 3; 4]), "Sample only from dendrites");
    ("-bynodeid", Arg.String
                    (fun str ->
                      dendrites := Some ((String.split_on_char ',' str) |> List.map (fun a -> a |> String.trim |> int_of_string))),
     "Sample from given node ids");
    ("-numfiles", Arg.Int (fun n -> numfiles := Some n), "How many files to sample from. If this argument is omitted we take all files.");
    ("-o", Arg.String (fun output -> outputdir := output), "Where to write output");
    ("-prune", Arg.String (fun str ->
                   prune := Some ((String.split_on_char ',' str) |> List.map (fun a -> a |> String.trim ))), "Prune trees with these proportions")
  ]
