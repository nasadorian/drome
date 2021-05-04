(*
  refio_tests.ml -- unit tests for functional reference type
*)

open Drome
open CS51Utils.Absbook

(* setting then getting should return the same value *)
let set_and_get _ =
  let test =
    IO.(
      RefIO.make 666
      >>= (fun r -> RefIO.set 42 r *> RefIO.get r)
      |> unsafe_run_sync)
  in
  unit_test (test = 42) "RefIO -- set and then get"

(* concurrent modification of a ref should work *)
let modify _ =
  let open IO in
  let r = RefIO.make [ 13; 42 ] in
  let go ar =
    let p0 = RefIO.modify (fun l -> (List.tl l, List.hd l)) ar in
    let p1 = RefIO.modify (fun l -> (List.tl l, List.hd l)) ar in
    suspend_async' p0 *> suspend_async' p1 *> sleep 0.1 *> RefIO.get ar
  in
  unit_test (IO.unsafe_run_sync (r >>= go) = []) "RefIO -- modify should work"

(* run 2 threads and ensure they both update the same ref *)
let concurrent_update _ =
  let open IO in
  let r = RefIO.make 0 in
  let go rf =
    rf >>= fun r' ->
    let p0 = RefIO.update (( + ) 2) r' in
    let p1 = RefIO.update (( + ) 40) r' in
    suspend_async' p0 *> suspend_async' p1 *> sleep 0.1 *> RefIO.get r'
  in
  let out = unsafe_run_sync (go r) in
  unit_test (out = 42) "RefIO -- concurrent update"

type record = { mutable artist : string; mutable rank : int }

(* run 3 threads and prove atomic update of a mutable record
 * final state of record should reflect both fields modified together *)
let atomicity _ =
  let open IO in
  let r = RefIO.make { artist = "Sting"; rank = 36 } in
  let up artist rank r =
    RefIO.update
      (fun a ->
        a.artist <- artist;
        a.rank <- rank;
        a)
      r
  in
  let go ar =
    ar >>= fun r ->
    let p0 = up "Nirvana" 1 r in
    let p1 = up "Primus" 13 r in
    let p2 = up "Celine Dion" 42 r in
    let zipd = suspend_async' p0 >*< suspend_async' p1 >*< suspend_async' p2 in
    zipd <* sleep 0.1 >*< RefIO.get r
  in
  let ((t0, t1), t2), { artist; rank } = go r |> unsafe_run_sync in
  Thread.join t0;
  Thread.join t1;
  Thread.join t2;
  unit_test
    ((artist = "Nirvana" && rank = 1)
    || (artist = "Primus" && rank = 13)
    || (artist = "Celine Dion" && rank = 42))
    "RefIO -- ensure atomicity of updates"

let _ =
  set_and_get ();
  modify ();
  concurrent_update ();
  atomicity ()
