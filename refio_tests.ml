(*
  refio_tests.ml -- unit tests for functional reference type
*)

open Drome
open CS51Utils.Absbook

let set_and_get _ =
  let test =
    IO.(
      RefIO.make 666
      >>= (fun r -> RefIO.set 42 r *> RefIO.get r)
      |> unsafe_run_sync)
  in
  unit_test (test = 42) "RefIO -- set and then get"

let concurrent_update _ =
  let open IO in
  let r = RefIO.make 2 in
  let go rf =
    rf >>= fun r ->
    let p0 = RefIO.update (( + ) 2) r in
    let p1 = RefIO.update (( * ) 20) r in
    make (fun _ ->
        let t0 = unsafe_run_async' p0 in
        let t1 = unsafe_run_async' p1 in
        Thread.join t0;
        Thread.join t1;
        RefIO.get r |> unsafe_run_sync)
  in
  unit_test (unsafe_run_sync (go r) = 42) "RefIO -- concurrent update"

type record = { mutable artist : string; mutable rank : int }

let atomicity _ =
  let open IO in
  let r = RefIO.make { artist = "Sting"; rank = 36 } in
  let go ar =
    ar >>= fun r ->
    let p0 =
      RefIO.update
        (fun a ->
          a.artist <- "Nirvana";
          a.rank <- 1;
          a)
        r
    in
    let p1 =
      RefIO.update
        (fun a ->
          a.artist <- "Primus";
          a.rank <- 69;
          a)
        r
    in
    make (fun _ ->
        let t0 = unsafe_run_async' p0 in
        let t1 = unsafe_run_async' p1 in
        Thread.join t0;
        Thread.join t1;
        unsafe_run_sync (RefIO.get r))
  in
  unit_test
    (go r |> unsafe_run_sync = { artist = "Primus"; rank = 69 })
    "RefIO  -- atomic update"

let _ =
  set_and_get ();
  concurrent_update ();
  atomicity ()
