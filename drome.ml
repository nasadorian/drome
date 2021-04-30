open Io_base
open Instances
open Util
open Thread

module IO = struct
  include Io_base
  include IOInstances

  (* suspend a -- lifts a deferred action into the IO context
   * This is the main entry point for most IO programs *)
  let suspend (a : 'a thunk) : 'a io = Suspend a

  (* unsafe_run_sync -- executes an IO program synchronously
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

  (* unsafe_run_async io cb -- executes IO program in another thread; call
   * callback cb when finished executing *)
  let unsafe_run_async (io : 'a io) (cb : 'a -> 'b) : Thread.t =
    Thread.create (cb << unsafe_run_sync) io

  (* unsafe_run_async' io -- executes IO program in another thread no callback *)
  let unsafe_run_async' (io : 'a io) : Thread.t = unsafe_run_async io id
end
