open Sys_unix

let map_over_dir (readfile : string -> 'a) (f : 'a -> 'b) (dir : string) =
  let filenames = readdir dir in
  let pipeline a = a |> Filename.concat dir |> readfile |> f in
  Core.Sequence.map ~f:pipeline (Core.Array.to_sequence filenames)
