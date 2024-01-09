(* Generated by futhark-bindgen *)

open Ctypes
open! Unsigned
open! Signed

module Bindings = struct
  external _stub: unit -> unit = "futhark_context_new"

  let fn = Foreign.foreign ~release_runtime_lock:true
  let context = typedef (ptr void) "context"
  let context_config = typedef (ptr void) "context_config"
  let futhark_context_new = fn "futhark_context_new" (context_config @-> returning context)
  let futhark_context_free = fn "futhark_context_free" (context @-> returning int)
  let futhark_context_sync = fn "futhark_context_sync" (context @-> returning int)
  let futhark_context_config_new = fn "futhark_context_config_new" (void @-> returning context_config)
  let futhark_context_config_free = fn "futhark_context_config_free" (context_config @-> returning int)
  let futhark_context_config_set_profiling = fn "futhark_context_config_set_profiling" (context_config @-> int @-> returning void)
  let futhark_context_config_set_debugging = fn "futhark_context_config_set_debugging" (context_config @-> int @-> returning void)
  let futhark_context_config_set_logging = fn "futhark_context_config_set_logging" (context_config @-> int @-> returning void)
  let futhark_context_config_set_cache_file = fn "futhark_context_config_set_cache_file" (context_config @-> string @-> returning void)
  let futhark_context_pause_profiling = fn "futhark_context_pause_profiling" (context @-> returning void)
  let futhark_context_unpause_profiling = fn "futhark_context_unpause_profiling" (context @-> returning void)
  let futhark_context_clear_caches = fn "futhark_context_clear_caches" (context @-> returning int)
  let futhark_context_get_error = fn "futhark_context_get_error" (context @-> returning (ptr char))
  let futhark_context_report = fn "futhark_context_report" (context @-> returning (ptr char))
  let free = fn "free" (ptr void @-> returning void)
  let strlen = fn "strlen" (ptr char @-> returning size_t)

  let array_f64_3d = typedef (ptr void) "array_f64_3d"
  let futhark_new_f64_3d = fn "futhark_new_f64_3d" (context @-> ptr double @-> int64_t @-> int64_t @-> int64_t @-> returning (array_f64_3d))
  let futhark_values_f64_3d = fn "futhark_values_f64_3d" (context @-> array_f64_3d @-> ptr double @-> returning (int))
  let futhark_free_f64_3d = fn "futhark_free_f64_3d" (context @-> array_f64_3d @-> returning (int))
  let futhark_shape_f64_3d = fn "futhark_shape_f64_3d" (context @-> array_f64_3d @-> returning (ptr int64_t))
  let array_f64_2d = typedef (ptr void) "array_f64_2d"
  let futhark_new_f64_2d = fn "futhark_new_f64_2d" (context @-> ptr double @-> int64_t @-> int64_t @-> returning (array_f64_2d))
  let futhark_values_f64_2d = fn "futhark_values_f64_2d" (context @-> array_f64_2d @-> ptr double @-> returning (int))
  let futhark_free_f64_2d = fn "futhark_free_f64_2d" (context @-> array_f64_2d @-> returning (int))
  let futhark_shape_f64_2d = fn "futhark_shape_f64_2d" (context @-> array_f64_2d @-> returning (ptr int64_t))
  let array_f64_1d = typedef (ptr void) "array_f64_1d"
  let futhark_new_f64_1d = fn "futhark_new_f64_1d" (context @-> ptr double @-> int64_t @-> returning (array_f64_1d))
  let futhark_values_f64_1d = fn "futhark_values_f64_1d" (context @-> array_f64_1d @-> ptr double @-> returning (int))
  let futhark_free_f64_1d = fn "futhark_free_f64_1d" (context @-> array_f64_1d @-> returning (int))
  let futhark_shape_f64_1d = fn "futhark_shape_f64_1d" (context @-> array_f64_1d @-> returning (ptr int64_t))
  let array_i64_1d = typedef (ptr void) "array_i64_1d"
  let futhark_new_i64_1d = fn "futhark_new_i64_1d" (context @-> ptr int64_t @-> int64_t @-> returning (array_i64_1d))
  let futhark_values_i64_1d = fn "futhark_values_i64_1d" (context @-> array_i64_1d @-> ptr int64_t @-> returning (int))
  let futhark_free_i64_1d = fn "futhark_free_i64_1d" (context @-> array_i64_1d @-> returning (int))
  let futhark_shape_i64_1d = fn "futhark_shape_i64_1d" (context @-> array_i64_1d @-> returning (ptr int64_t))
  let futhark_entry_unbalanced_gw_pairwise = fn "futhark_entry_unbalanced_gw_pairwise" (context @-> ptr array_f64_1d @-> array_f64_3d @-> double @-> double @-> double @-> double @-> double @-> double @-> double @-> returning (int))
  let futhark_entry_unbalanced_gw_pairwise_v2 = fn "futhark_entry_unbalanced_gw_pairwise_v2" (context @-> ptr array_f64_2d @-> array_f64_3d @-> double @-> double @-> double @-> double @-> int32_t @-> double @-> returning (int))
  let futhark_entry_unbalanced_gw_parallel_while = fn "futhark_entry_unbalanced_gw_parallel_while" (context @-> ptr array_f64_1d @-> array_f64_1d @-> array_i64_1d @-> returning (int))
  let futhark_entry_unbalanced_gw_total_cost = fn "futhark_entry_unbalanced_gw_total_cost" (context @-> ptr double @-> double @-> double @-> double @-> array_f64_2d @-> array_f64_1d @-> array_f64_2d @-> array_f64_1d @-> double @-> double @-> double @-> double @-> returning (int))
end

type error =
  | InvalidShape of int * int
  | NullPtr
  | Code of int
  | UseAfterFree of [`context | `array | `opaque]

exception Error of error

let check_use_after_free t b = if b then raise (Error (UseAfterFree t))

let () = Printexc.register_printer (function
  | Error (InvalidShape (a, b)) -> Some (Printf.sprintf "futhark error: invalid shape, expected %d but got %d" a b)
  | Error NullPtr -> Some "futhark error: null pointer"
  | Error (Code c) -> Some (Printf.sprintf "futhark error: code %d" c) 
  | Error (UseAfterFree `context) -> Some "futhark: context used after beeing freed"
  | Error (UseAfterFree `array) -> Some "futhark: array used after beeing freed"
  | Error (UseAfterFree `opaque) -> Some "futhark: opqaue value used after beeing freed"
  | _ -> None)



open Bigarray

module Context = struct
  type t = { handle: unit ptr; config: unit ptr; cache_file: string option; auto_sync: bool; mutable context_free: bool }

  let free t =
    if not t.context_free then
      let () = ignore (Bindings.futhark_context_sync t.handle) in
      let () = ignore (Bindings.futhark_context_free t.handle) in
      let () = ignore (Bindings.futhark_context_config_free t.config) in
      t.context_free <- true

  let v ?(debug = false) ?(log = false) ?(profile = false) ?cache_file ?(auto_sync = true)  () =
    let config = Bindings.futhark_context_config_new () in
    if is_null config then raise (Error NullPtr);
    Bindings.futhark_context_config_set_debugging config (if debug then 1 else 0);
    Bindings.futhark_context_config_set_profiling config (if profile then 1 else 0);
    Bindings.futhark_context_config_set_logging config (if log then 1 else 0);
    
    Option.iter (Bindings.futhark_context_config_set_cache_file config) cache_file;
    let handle = Bindings.futhark_context_new config in
    if is_null handle then 
      let () = ignore @@ Bindings.futhark_context_config_free config in
      raise (Error NullPtr)
    else
      let t = { handle; config; cache_file; auto_sync; context_free = false } in
      let () = Gc.finalise free t in
      t

  let sync t =
    check_use_after_free `context t.context_free;
    let rc = Bindings.futhark_context_sync t.handle in
    if rc <> 0 then raise (Error (Code rc))

  let auto_sync t =
    if t.auto_sync then sync t
  
  let clear_caches t =
    check_use_after_free `context t.context_free;
    let rc = Bindings.futhark_context_clear_caches t.handle in
    if rc <> 0 then raise (Error (Code rc))

  let string_opt_of_ptr ptr = 
    if is_null ptr then None
    else
      let len = Bindings.strlen ptr |> Unsigned.Size_t.to_int in
      let s = String.init len (fun i -> !@(ptr +@ i)) in
      let () = Bindings.free (coerce (Ctypes.ptr Ctypes.char) (Ctypes.ptr void) ptr) in Some s

  let get_error t = 
    check_use_after_free `context t.context_free;
    let ptr = Bindings.futhark_context_get_error t.handle in string_opt_of_ptr ptr

  let report t = 
    check_use_after_free `context t.context_free;
    let ptr = Bindings.futhark_context_report t.handle in string_opt_of_ptr ptr

  let pause_profiling t = 
    check_use_after_free `context t.context_free;
    Bindings.futhark_context_pause_profiling t.handle

  let unpause_profiling t =
    check_use_after_free `context t.context_free;
    Bindings.futhark_context_unpause_profiling t.handle
end

type futhark_array = { ptr: unit ptr; shape: int array; ctx: Context.t; mutable array_free: bool }
type opaque = { opaque_ptr: unit ptr; opaque_ctx: Context.t; mutable opaque_free: bool }

module Array_f64_3d = struct
  type t = futhark_array

  type kind = (float, Bigarray.float64_elt) Bigarray.kind
  
  let kind = Bigarray.float64

  let free t =
    if not t.array_free && not t.ctx.Context.context_free then
      let () = ignore (Bindings.futhark_free_f64_3d t.ctx.Context.handle t.ptr) in
      t.array_free <- true

  let cast x =
    coerce (ptr void) (ptr double) (to_voidp x)
  
  let v ctx ba =
    check_use_after_free `context ctx.Context.context_free;
    let dims = Genarray.dims ba in
    let ptr = Bindings.futhark_new_f64_3d ctx.Context.handle (cast @@ bigarray_start genarray ba) (Int64.of_int dims.(0)) (Int64.of_int dims.(1)) (Int64.of_int dims.(2)) in
    if is_null ptr then raise (Error NullPtr);
    Context.auto_sync ctx;
    let t = { ptr; ctx; shape = dims; array_free = false } in
    Gc.finalise free t; t

  let values t ba =
    check_use_after_free `context t.ctx.Context.context_free;
    check_use_after_free `array t.array_free;
    let dims = Genarray.dims ba in
    let a = Array.fold_left ( * ) 1 t.shape in
    let b = Array.fold_left ( * ) 1 dims in
    if (a <> b) then raise (Error (InvalidShape (a, b)));
    let rc = Bindings.futhark_values_f64_3d t.ctx.Context.handle t.ptr (cast @@ bigarray_start genarray ba) in
    Context.auto_sync t.ctx;
    if rc <> 0 then raise (Error (Code rc))

  let values_array1 t ba =
    let ba = genarray_of_array1 ba in
    let ba = reshape ba t.shape in
    values t ba

  let get t =
    let dims = t.shape in
    let g = Genarray.create kind C_layout dims in
    values t g;
    g

  let get_array1 t =
    let len = Array.fold_left ( * ) 1 t.shape in
    let g = Array1.create kind C_layout len in
    values_array1 t g;
    g

  let shape t = t.shape

  let of_array1 ctx dims arr =
    let len = Array.fold_left ( * ) 1 dims in
    assert (len = Array1.dim arr);
    let g = genarray_of_array1 arr in
    let g = reshape g dims in
    v ctx g

  let of_array ctx dims arr =
    let arr = Array1.of_array kind C_layout arr in
    of_array1 ctx dims arr

  let ptr_shape ctx ptr =
    let s = Bindings.futhark_shape_f64_3d ctx ptr in
    Array.init 3 (fun i -> Int64.to_int !@ (s +@ i))

  let of_ptr ctx ptr =
    check_use_after_free `context ctx.Context.context_free;
    if is_null ptr then raise (Error NullPtr);
    let shape = ptr_shape ctx.Context.handle ptr in
    let t = { ptr; ctx; shape; array_free = false } in
    Gc.finalise free t; t
    
  let _ = of_ptr
end


module Array_f64_2d = struct
  type t = futhark_array

  type kind = (float, Bigarray.float64_elt) Bigarray.kind
  
  let kind = Bigarray.float64

  let free t =
    if not t.array_free && not t.ctx.Context.context_free then
      let () = ignore (Bindings.futhark_free_f64_2d t.ctx.Context.handle t.ptr) in
      t.array_free <- true

  let cast x =
    coerce (ptr void) (ptr double) (to_voidp x)
  
  let v ctx ba =
    check_use_after_free `context ctx.Context.context_free;
    let dims = Genarray.dims ba in
    let ptr = Bindings.futhark_new_f64_2d ctx.Context.handle (cast @@ bigarray_start genarray ba) (Int64.of_int dims.(0)) (Int64.of_int dims.(1)) in
    if is_null ptr then raise (Error NullPtr);
    Context.auto_sync ctx;
    let t = { ptr; ctx; shape = dims; array_free = false } in
    Gc.finalise free t; t

  let values t ba =
    check_use_after_free `context t.ctx.Context.context_free;
    check_use_after_free `array t.array_free;
    let dims = Genarray.dims ba in
    let a = Array.fold_left ( * ) 1 t.shape in
    let b = Array.fold_left ( * ) 1 dims in
    if (a <> b) then raise (Error (InvalidShape (a, b)));
    let rc = Bindings.futhark_values_f64_2d t.ctx.Context.handle t.ptr (cast @@ bigarray_start genarray ba) in
    Context.auto_sync t.ctx;
    if rc <> 0 then raise (Error (Code rc))

  let values_array1 t ba =
    let ba = genarray_of_array1 ba in
    let ba = reshape ba t.shape in
    values t ba

  let get t =
    let dims = t.shape in
    let g = Genarray.create kind C_layout dims in
    values t g;
    g

  let get_array1 t =
    let len = Array.fold_left ( * ) 1 t.shape in
    let g = Array1.create kind C_layout len in
    values_array1 t g;
    g

  let shape t = t.shape

  let of_array1 ctx dims arr =
    let len = Array.fold_left ( * ) 1 dims in
    assert (len = Array1.dim arr);
    let g = genarray_of_array1 arr in
    let g = reshape g dims in
    v ctx g

  let of_array ctx dims arr =
    let arr = Array1.of_array kind C_layout arr in
    of_array1 ctx dims arr

  let ptr_shape ctx ptr =
    let s = Bindings.futhark_shape_f64_2d ctx ptr in
    Array.init 2 (fun i -> Int64.to_int !@ (s +@ i))

  let of_ptr ctx ptr =
    check_use_after_free `context ctx.Context.context_free;
    if is_null ptr then raise (Error NullPtr);
    let shape = ptr_shape ctx.Context.handle ptr in
    let t = { ptr; ctx; shape; array_free = false } in
    Gc.finalise free t; t
    
  let _ = of_ptr
end


module Array_f64_1d = struct
  type t = futhark_array

  type kind = (float, Bigarray.float64_elt) Bigarray.kind
  
  let kind = Bigarray.float64

  let free t =
    if not t.array_free && not t.ctx.Context.context_free then
      let () = ignore (Bindings.futhark_free_f64_1d t.ctx.Context.handle t.ptr) in
      t.array_free <- true

  let cast x =
    coerce (ptr void) (ptr double) (to_voidp x)
  
  let v ctx ba =
    check_use_after_free `context ctx.Context.context_free;
    let dims = Genarray.dims ba in
    let ptr = Bindings.futhark_new_f64_1d ctx.Context.handle (cast @@ bigarray_start genarray ba) (Int64.of_int dims.(0)) in
    if is_null ptr then raise (Error NullPtr);
    Context.auto_sync ctx;
    let t = { ptr; ctx; shape = dims; array_free = false } in
    Gc.finalise free t; t

  let values t ba =
    check_use_after_free `context t.ctx.Context.context_free;
    check_use_after_free `array t.array_free;
    let dims = Genarray.dims ba in
    let a = Array.fold_left ( * ) 1 t.shape in
    let b = Array.fold_left ( * ) 1 dims in
    if (a <> b) then raise (Error (InvalidShape (a, b)));
    let rc = Bindings.futhark_values_f64_1d t.ctx.Context.handle t.ptr (cast @@ bigarray_start genarray ba) in
    Context.auto_sync t.ctx;
    if rc <> 0 then raise (Error (Code rc))

  let values_array1 t ba =
    let ba = genarray_of_array1 ba in
    let ba = reshape ba t.shape in
    values t ba

  let get t =
    let dims = t.shape in
    let g = Genarray.create kind C_layout dims in
    values t g;
    g

  let get_array1 t =
    let len = Array.fold_left ( * ) 1 t.shape in
    let g = Array1.create kind C_layout len in
    values_array1 t g;
    g

  let shape t = t.shape

  let of_array1 ctx dims arr =
    let len = Array.fold_left ( * ) 1 dims in
    assert (len = Array1.dim arr);
    let g = genarray_of_array1 arr in
    let g = reshape g dims in
    v ctx g

  let of_array ctx dims arr =
    let arr = Array1.of_array kind C_layout arr in
    of_array1 ctx dims arr

  let ptr_shape ctx ptr =
    let s = Bindings.futhark_shape_f64_1d ctx ptr in
    Array.init 1 (fun i -> Int64.to_int !@ (s +@ i))

  let of_ptr ctx ptr =
    check_use_after_free `context ctx.Context.context_free;
    if is_null ptr then raise (Error NullPtr);
    let shape = ptr_shape ctx.Context.handle ptr in
    let t = { ptr; ctx; shape; array_free = false } in
    Gc.finalise free t; t
    
  let _ = of_ptr
end


module Array_i64_1d = struct
  type t = futhark_array

  type kind = (int64, Bigarray.int64_elt) Bigarray.kind
  
  let kind = Bigarray.int64

  let free t =
    if not t.array_free && not t.ctx.Context.context_free then
      let () = ignore (Bindings.futhark_free_i64_1d t.ctx.Context.handle t.ptr) in
      t.array_free <- true

  let cast x =
    coerce (ptr void) (ptr int64_t) (to_voidp x)
  
  let v ctx ba =
    check_use_after_free `context ctx.Context.context_free;
    let dims = Genarray.dims ba in
    let ptr = Bindings.futhark_new_i64_1d ctx.Context.handle (cast @@ bigarray_start genarray ba) (Int64.of_int dims.(0)) in
    if is_null ptr then raise (Error NullPtr);
    Context.auto_sync ctx;
    let t = { ptr; ctx; shape = dims; array_free = false } in
    Gc.finalise free t; t

  let values t ba =
    check_use_after_free `context t.ctx.Context.context_free;
    check_use_after_free `array t.array_free;
    let dims = Genarray.dims ba in
    let a = Array.fold_left ( * ) 1 t.shape in
    let b = Array.fold_left ( * ) 1 dims in
    if (a <> b) then raise (Error (InvalidShape (a, b)));
    let rc = Bindings.futhark_values_i64_1d t.ctx.Context.handle t.ptr (cast @@ bigarray_start genarray ba) in
    Context.auto_sync t.ctx;
    if rc <> 0 then raise (Error (Code rc))

  let values_array1 t ba =
    let ba = genarray_of_array1 ba in
    let ba = reshape ba t.shape in
    values t ba

  let get t =
    let dims = t.shape in
    let g = Genarray.create kind C_layout dims in
    values t g;
    g

  let get_array1 t =
    let len = Array.fold_left ( * ) 1 t.shape in
    let g = Array1.create kind C_layout len in
    values_array1 t g;
    g

  let shape t = t.shape

  let of_array1 ctx dims arr =
    let len = Array.fold_left ( * ) 1 dims in
    assert (len = Array1.dim arr);
    let g = genarray_of_array1 arr in
    let g = reshape g dims in
    v ctx g

  let of_array ctx dims arr =
    let arr = Array1.of_array kind C_layout arr in
    of_array1 ctx dims arr

  let ptr_shape ctx ptr =
    let s = Bindings.futhark_shape_i64_1d ctx ptr in
    Array.init 1 (fun i -> Int64.to_int !@ (s +@ i))

  let of_ptr ctx ptr =
    check_use_after_free `context ctx.Context.context_free;
    if is_null ptr then raise (Error NullPtr);
    let shape = ptr_shape ctx.Context.handle ptr in
    let t = { ptr; ctx; shape; array_free = false } in
    Gc.finalise free t; t
    
  let _ = of_ptr
end


let unbalanced_gw_pairwise ctx input0 input1 input2 input3 input4 input5 input6 input7 =
  check_use_after_free `context ctx.Context.context_free;
  let out_ptr = allocate (ptr void) null in
  let rc = Bindings.futhark_entry_unbalanced_gw_pairwise ctx.Context.handle out_ptr input0.ptr input1 input2 input3 input4 input5 input6 input7 in
  if rc <> 0 then raise (Error (Code rc));
  ((Array_f64_1d.of_ptr ctx !@out_ptr))

let unbalanced_gw_pairwise_v2 ctx input0 input1 input2 input3 input4 input5 input6 =
  check_use_after_free `context ctx.Context.context_free;
  let out_ptr = allocate (ptr void) null in
  let rc = Bindings.futhark_entry_unbalanced_gw_pairwise_v2 ctx.Context.handle out_ptr input0.ptr input1 input2 input3 input4 input5 input6 in
  if rc <> 0 then raise (Error (Code rc));
  ((Array_f64_2d.of_ptr ctx !@out_ptr))

let unbalanced_gw_parallel_while ctx input0 input1 =
  check_use_after_free `context ctx.Context.context_free;
  let out_ptr = allocate (ptr void) null in
  let rc = Bindings.futhark_entry_unbalanced_gw_parallel_while ctx.Context.handle out_ptr input0.ptr input1.ptr in
  if rc <> 0 then raise (Error (Code rc));
  ((Array_f64_1d.of_ptr ctx !@out_ptr))

let unbalanced_gw_total_cost ctx input0 input1 input2 input3 input4 input5 input6 input7 input8 input9 input10 =
  check_use_after_free `context ctx.Context.context_free;
  let out_ptr = allocate_n double ~count:1 in
  let rc = Bindings.futhark_entry_unbalanced_gw_total_cost ctx.Context.handle out_ptr input0 input1 input2 input3.ptr input4.ptr input5.ptr input6.ptr input7 input8 input9 input10 in
  if rc <> 0 then raise (Error (Code rc));
  (!@out_ptr)
