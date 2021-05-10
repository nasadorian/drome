(*
  io.ml -- runtime implementation for the IO monad
*)

open Util
open Dsl
include Instances.IOInstances

(* make a -- lifts a deferred action into IO;
 * this is the main starting point for most IO programs *)
let make (a : 'a thunk) : 'a io = Suspend a

(* noop -- unit in IO; an action representing doing nothing *)
let noop : unit io = pure ()

(* sleep t -- pauses the current thread for `t` seconds *)
let sleep (t : float) : unit io = make (fun _ -> Thread.delay t)

(* unsafe_run_sync io -- synchronous runtime for IO programs
 * N.B. "unsafe" means unhandled errors in program sequence will be thrown *)
let rec unsafe_run_sync : type a. a io -> a =
  (* used for Attempt nodes; catches exceptions in `result` for a given IO *)
  let lift_attempt : type a. a io -> (a, exn) result io =
   fun io ->
    Suspend
      (fun _ -> try Result.ok (unsafe_run_sync io) with e -> Result.error e)
  in
  function
  | Pure a -> a
  | Suspend a -> a ()
  (* inspect next node below Bind and perform optimizations *)
  | Bind (f, io) -> (
      match io with
      | Pure a -> unsafe_run_sync (f a)
      | Suspend ta -> unsafe_run_sync (f (ta ()))
      (* trampolining occurs here via monad associativity *)
      | Bind (g, io') -> unsafe_run_sync (Bind (g >=> f, io'))
      | Map (g, io') -> unsafe_run_sync (Bind ((fun a -> f (g a)), io'))
      | RaiseError e -> raise e
      | Attempt io -> unsafe_run_sync (Bind (f, lift_attempt io))
      | HandleErrorWith (h, io) ->
          unsafe_run_sync
            (lift_attempt io >>= Result.fold ~ok:f ~error:(h >=> f)))
  (* inspect next node below Map and perform optimizations *)
  | Map (f, io) -> (
      match io with
      | Pure a -> f a
      | Suspend ta -> f (ta ())
      | Bind (g, io') -> unsafe_run_sync (io' >>= fun a -> g a <$> f)
      (* map fusion via functor composition law *)
      | Map (g, io') -> unsafe_run_sync (Map (f << g, io'))
      | RaiseError e -> raise e
      | Attempt io -> unsafe_run_sync (Map (f, lift_attempt io))
      | HandleErrorWith (h, io) ->
          unsafe_run_sync
            (lift_attempt io
            >>= Result.fold ~ok:(pure << f) ~error:(fun a -> h a <$> f)))
  | RaiseError e -> raise e
  | Attempt io -> unsafe_run_sync (lift_attempt io)
  | HandleErrorWith (h, io) ->
      unsafe_run_sync (lift_attempt io >>= Result.fold ~ok:pure ~error:h)

(* unsafe_run_async io cb -- executes IO program in another thread;
 * callback `cb` is executed once finished *)
let unsafe_run_async (cb : 'a -> 'b) : 'a io -> Thread.t =
  Thread.create (cb << unsafe_run_sync)

(* unsafe_run_async' io -- executes IO in another thread; no callback
 * N.B. defining this function as partially applied confuses the compiler *)
let unsafe_run_async' (io : 'a io) : Thread.t = unsafe_run_async id io

(* suspend_async cb io -- deferred run of IO with callback in another thread *)
let suspend_async (cb : 'a -> 'b) (io : 'a io) : Thread.t io =
  make (fun _ -> unsafe_run_async cb io)

(* suspend_async' io -- deferred run of IO in another thread; no callback *)
let suspend_async' (io : 'a io) : Thread.t io =
  make (fun _ -> unsafe_run_async' io)
