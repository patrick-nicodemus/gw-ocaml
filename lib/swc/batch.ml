open Sys_unix

let read_dir (readfile : string -> 'a) (dir : string) =
  let filenames = readdir dir in
  let pipeline a = a |> Filename.concat dir |> readfile in
  Core.Sequence.map ~f:pipeline (Core.Array.to_sequence filenames)
