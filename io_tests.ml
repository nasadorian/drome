(*
  io_tests.ml -- tests for the IO monad and its typeclass implementations
*)

open Dsl
open Drome.IO
open Drome
open Instances.IOInstances
open CS51Utils.Absbook

(* infinite looping test -- should simply compile; can be tested manually *)
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

let async_tests _ =
  let mem = ref 0 in
  let prog = make (fun _ -> mem := !mem + 2) in
  let cb _ = mem := !mem + 40 in
  let thread = unsafe_run_async cb prog in
  unit_test
    (Thread.join thread;
     !mem = 42)
    "IO.unsafe_run_async -- execute callback in new thread"

let _ =
  map_tests ();
  bind_tests ();
  product_tests ();
  attempt_tests ();
  async_tests ()
