open Io.IO (* TODO don't like this import *)

open CS51Utils.Absbook

let loop_test _ =
  let read = suspend read_line in
  let print (s : string) : unit io = suspend (fun _ -> print_endline s) in
  let rec loop () =
    let run = read >>= print in
    run *> Defer loop
  in
  loop

let product_tests _ =
  let r = ref 41 in
  let left = suspend (fun _ -> r := !r - 1) in
  let right = suspend (fun _ -> r := !r + 2) in
  let lr = left *> right in
  let rl = left <* right <* suspend (fun _ -> r := 42) in
  unit_test
    (unsafe_run_sync lr;
     !r = 42)
    "productR left first";
  unit_test
    (unsafe_run_sync rl;
     !r = 43)
    "productL right first"

let _ = product_tests ()
