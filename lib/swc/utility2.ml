let () = Random.self_init ()

let random_elt l =
  let sum = Core.List.fold ~init:0. ~f:Float.add l in
  let rr = Random.float sum in
  let rec random_elt' l acc_float acc_int =
    match l with
    | x :: a ->
      let x' = x +. acc_float in
      if Float.compare rr x' < 0 then acc_int else
        random_elt' a x' (acc_int + 1)
    | [] -> raise (Invalid_argument "Uh oh.")
  in
  random_elt' l 0.0 0
