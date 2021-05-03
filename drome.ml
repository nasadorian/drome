(*
  drome.ml -- main modules for the drome library
*)

open Dsl
open Instances
open Util
open Thread

(* the IO monad -- *)
module IO = struct
  include IOInstances

  (* make a -- lifts a deferred action into IO;
   * this is the main starting point for most IO programs *)
  let make (a : 'a thunk) : 'a io = Suspend a

  (* noop -- unit in IO; an action representing doing nothing *)
  let noop : unit io = pure ()

  (* sleep t -- pauses the current thread for `t` seconds *)
  let sleep (t : float) : unit io = make (fun _ -> Thread.delay t)

  (* unsafe_run_sync -- executes an IO program synchronously;
   * "unsafe" means unhandled errors in sequence will be thrown *)
  let rec unsafe_run_sync : type a. a io -> a = function
    | Pure a -> a
    | Suspend a -> a ()
    | Bind (f, io) -> (
        match io with
        | Pure a -> unsafe_run_sync (f a)
        | Suspend ta -> unsafe_run_sync (f (ta ()))
        (* Trampolining occurs here via monad associativity *)
        | Bind (g, io') -> unsafe_run_sync (Bind (g >=> f, io'))
        | Map (g, io') -> unsafe_run_sync (Bind ((fun a -> f (g a)), io'))
        | RaiseError e -> raise e
        | Attempt io -> unsafe_run_sync (Bind (f, lift_attempt io))
        | HandleErrorWith (h, io) ->
            unsafe_run_sync
              (lift_attempt io >>= Result.fold ~ok:f ~error:(h >=> f)))
    | Map (f, io) -> (
        match io with
        | Pure a -> f a
        | Suspend ta -> f (ta ())
        | Bind (g, io') -> unsafe_run_sync (io' >>= fun a -> g a <$> f)
        (* Map fusion via functor composition law *)
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

  (* lift_attempt -- suspends an IO action, catching exceptions in `result` *)
  and lift_attempt : type a. a io -> (a, exn) result io =
   fun io ->
    Suspend
      (fun _ -> try Result.ok (unsafe_run_sync io) with e -> Result.error e)

  (* unsafe_run_async io cb -- executes IO program in another thread; calling
   * callback `cb` when finished executing *)
  let unsafe_run_async (cb : 'a -> 'b) (io : 'a io) : Thread.t =
    Thread.create (cb << unsafe_run_sync) io

  (* unsafe_run_async' io -- executes IO in another thread; no callback *)
  let unsafe_run_async' (io : 'a io) : Thread.t = unsafe_run_async id io
end

module Resource = struct
  include ResourceInstances

  (* make a r -- acquire resource `a` in IO, and encode how to release it
   * with function `r` *)
  let make (acq : 'a io) (rel : 'a -> unit io) : 'a resource =
    Allocate (rel, acq)

  (* use u r -- apply `u` a resource then release it *)
  let rec use : type a b. (a -> b io) -> a resource -> b io =
   fun u r ->
    match r with
    | Allocate (release, acquire) ->
        IO.(
          acquire >>= fun a ->
          (* handle failure during use and ensure resource release *)
          let action =
            handle_error_with (fun e -> release a *> raise_error e) (u a)
          in
          action <* release a)
    | RBind (f, res) -> (
        match res with
        | Allocate (release, acquire) ->
            IO.(
              acquire >>= fun a ->
              let res' = f a in
              use u res' <* release a)
        | RBind (g, res') -> use u (RBind (g >=> f, res'))
        | RPure a -> use u (f a))
    | RPure a -> u a

  (* use' r -- acquire and release resource with a no-op `u` action *)
  let use' : type a. a resource -> a io = fun r -> use IO.pure r
end
