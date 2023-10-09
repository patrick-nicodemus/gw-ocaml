open Helper
open Owl_dense_matrix_d
open Ot.Bothfree


let n = 50;;
let m = 69;;
let coupling = random_matrix_uniform n m;;
let a_dmat =
  let x = random_matrix_uniform n n in add x (transpose x);;
let b_dmat =
    let y = random_matrix_uniform m m in add y (transpose y);;
let a_dist = random_matrix_uniform n 1 ;;
let b_dist = random_matrix_uniform m 1 ;;
let lambda_1 = 4.2 ;;
let lambda_2 = 3.9 ;;


let gw_loss_fun coupling_mat =
  gw_loss a_dmat a_dist b_dmat b_dist coupling_mat lambda_1 lambda_2;;
let gw_loss_coupling = gw_loss_fun coupling
let differential = random_matrix_uniform n m;;

let coupling' k = (2.0 ** ( -.(float_of_int k)))
    |> mul_scalar differential
                  |> add coupling;;

  (* let ssqrd k = ssqr_diff' coupling (coupling' k) in *)
let gw_loss' k = coupling' k |> gw_loss_fun;;
(* let actual_differential k = *)
let gradient_matrix = gradient a_dmat a_dist b_dmat b_dist coupling lambda_1 lambda_2
let predicted_differential k =
    let derivative = sum' (mul gradient_matrix differential) in
    (2.0 ** (-.(float_of_int k))) *. derivative;;
(* let compare k = Printf.printf *)
(*                   "Actual difference: %F Predicted difference: %F\n" *)
(*                   (gw_loss' k -. gw_loss_coupling) *)
(*                   (predicted_differential k) *)

(* let a = *)
(*   print_float (gw_loss_fun coupling); *)
(*   print_endline "\n"; *)
(*   List.map compare [1;2;3;4;5;6;7;8;9] *)
