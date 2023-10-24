let myfun a b c =
  let x = a +. b in
  let y =
    let x = b +. c in
    x
  in x +. y

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons (x, y) -> Cons (f x, map f y)
 
