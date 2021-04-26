open Io (* TODO don't like this import *)

open CS51Utils.Absbook

(* This should simply compile and can be tested manually *)
let repl_test _ =
  let read = suspend read_line in
  let print (s : string) : unit io = suspend (fun _ -> print_endline s) in
  (*let rec loop () = (read >>= print) *> loop () in*)
  let rec loop () = read >>= print >>= loop in
  loop

let bind_tests _ =
  let v = pure 39 in
  let plus i = suspend (fun _ -> i + 1) in
  let prog = v >>= plus >>= plus >>= plus in
  unit_test (unsafe_run_sync prog = 42) "bind -- triple bind"

let product_tests _ =
  let r = ref 41 in
  let left = suspend (fun _ -> r := !r - 1) in
  let right = suspend (fun _ -> r := !r + 2) in
  let lr = left *> right in
  let rl = left <* right <* suspend (fun _ -> r := 42) in
  unit_test
    (unsafe_run_sync lr;
     !r = 42)
    "productR -- left first";
  unit_test
    (unsafe_run_sync rl;
     !r = 43)
    "productL -- right first"

(*let attempt_tests _ =*)
  (*let v = pure 39 in*)
  (*let up i =*)
    (*if i = 42 then raise (Invalid_argument "bork") else pure (succ i)*)
  (*in*)
  (*let prog = v >>= up >>= up >>= up >>= up in*)
  (*unit_test*)
    (*(unsafe_run_sync (attempt prog) = Result.error (Invalid_argument "bork"))*)
    (*"attempt -- catch exception"*)

let _ =
  bind_tests ();
  product_tests ()
  (*attempt_tests ()*)
