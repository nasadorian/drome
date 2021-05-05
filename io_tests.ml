(*
  io_tests.ml -- tests for the IO monad and its typeclass implementations
*)

open Dsl
open IO
open Instances.IOInstances
open CS51Utils.Absbook

(* infinite looping test -- interactive and can be tested manually *)
let repl_test _ =
  let read = make read_line in
  let print (s : string) : unit io = make (fun _ -> print_endline s) in
  (* trampolining used in IO.unsafe_run_async allows infinite loops! *)
  let rec loop () = read >>= print >>= loop in
  loop

(* functor map tests *)
let map_tests _ =
  let m0 = Pure 38 in
  let m1 = m0 <$> ( + ) 1 <$> ( + ) 1 <$> ( + ) 2 in
  unit_test (unsafe_run_sync m1 = 42) "IO.map -- multiple map actions"

(* check monadic bind between IO actions *)
let bind_tests _ =
  let v = pure 39 in
  let plus i = make (fun _ -> i + 1) in
  let prog = v >>= plus >>= plus >>= plus in
  unit_test (unsafe_run_sync prog = 42) "IO.bind -- triple bind"

(* productL/productR run two effects in sequence while discarding one result *)
let product_tests _ =
  let r = ref 41 in
  let left = make (fun _ -> r := !r - 1) in
  let right = make (fun _ -> r := !r + 2) in
  let lr = left *> right in
  let rl = pure "hello" <* (left <* right <* make (fun _ -> r := 42)) in
  unit_test
    (unsafe_run_sync lr;
     !r = 42)
    "IO.productR -- sequence and discard left";
  unit_test
    (let out = unsafe_run_sync rl in
     !r = 42 && out = "hello")
    "IO.productL -- run and discard right"

(* check that attempt catches failure in Error and produces results in Ok *)
let attempt_tests _ =
  let v = pure 39 in
  let up i =
    if i = 42 then raise (Invalid_argument "bork") else pure (succ i)
  in
  let prog = v >>= up >>= up >>= up >>= up in
  unit_test
    (unsafe_run_sync (attempt prog) = Result.error (Invalid_argument "bork"))
    "IO.attempt -- catch exception";
  let prog = v >>= up >>= up >>= up in
  unit_test
    (unsafe_run_sync (attempt prog) = Result.ok 42)
    "IO.attempt -- right side success"

(* callback should execute when running asyncrhonously *)
let async_tests _ =
  let mem = ref 0 in
  let prog = make (fun _ -> mem := !mem + 2) in
  let cb _ = mem := !mem + 40 in
  let thread = unsafe_run_async cb prog in
  unit_test
    (Thread.join thread;
     !mem = 42)
    "IO.unsafe_run_async -- execute callback in new thread"

(* error handling should work *)
let handle_test _ =
  let err = raise_error (Invalid_argument "Shouldn't throw") in
  let handled = handle_error (fun _ -> 42) err in
  unit_test (unsafe_run_sync handled = 42) "IO.handle_error -- does not throw"

(* an erroneous action, after being "attempted", can be rethrown *)
let rethrow_test _ =
  let err = raise_error (Invalid_argument "Should throw") in
  let test =
    try
      attempt err |> rethrow |> unsafe_run_sync;
      false
    with _ -> true
  in
  unit_test test "IO.rethrow -- should reverse attempt"

(* ensure should run a predicate, throw when it fails, and vice versa *)
let ensure_test _ =
  let x = pure 42 in
  let y = pure 41 in
  let p i = i mod 2 = 0 in
  let x' = ensure p (Invalid_argument "even only!") x in
  let y' = ensure p (Invalid_argument "even only!") y in
  unit_test
    (unsafe_run_sync x' = 42)
    "IO.ensure -- should not throw on when predicate passes";
  unit_test
    (try
       unsafe_run_sync y' |> ignore;
       false
     with _ -> true)
    "IO.ensure -- should throw when predicate fails"

(* adapt_error should modify error types *)
let adapt_error_test _ =
  let err = raise_error (Invalid_argument "bad") in
  let adapted = adapt_error (fun _ -> End_of_file) err in
  unit_test
    (try
       unsafe_run_sync adapted;
       false
     with
    | End_of_file -> true
    | _ -> false)
    "IO.adapt_error should transform the error type"

(* basic applicative syntax *)
let ap_test _ =
  let kabob = Printf.sprintf "%s-%s-%s" in
  let x = return "homer" in
  let y = make (fun _ -> "simpson") in
  let z = pure "kravkalash" in
  let out = pure kabob <*> x <*> y <*> z |> unsafe_run_sync in
  unit_test
    (out = "homer-simpson-kravkalash")
    "IO.ap -- apply parameters in IO context to a function"

(* applicative zip should work as intended *)
let zip_test _ =
  let x = return "hello" in
  let y = return 42 in
  unit_test
    (x >*< y |> unsafe_run_sync = ("hello", 42))
    "IO.zip -- join two values applicative style"

(* run all of the tests! *)
let _ =
  map_tests ();
  bind_tests ();
  product_tests ();
  attempt_tests ();
  async_tests ();
  handle_test ();
  adapt_error_test ();
  rethrow_test ();
  ensure_test ();
  ap_test ();
  zip_test ()
