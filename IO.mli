open Util
open Dsl
open Typeclasses

include Functor with type 'a f = 'a io

include Applicative with type 'a f = 'a io

include Monad with type 'a f = 'a io

include ApplicativeError with type 'a f = 'a io

include MonadError with type 'a f = 'a io

(* make a -- lifts a deferred action into IO;
 * this is the main starting point for most IO programs *)
val make : 'a thunk -> 'a io

(* noop -- unit in IO; an action representing doing nothing *)
val noop : unit io

(* sleep t -- pauses the current thread for `t` seconds *)
val sleep : float -> unit io

(* unsafe_run_sync io -- synchronous runtime for IO programs
 * N.B. "unsafe" means unhandled errors in program sequence will be thrown *)
val unsafe_run_sync : 'a io -> 'a

(* unsafe_run_async io cb -- executes IO program in another thread;
 * callback `cb` is executed once finished *)
val unsafe_run_async : ('a -> 'b) -> 'a io -> Thread.t

(* unsafe_run_async' io -- executes IO in another thread; no callback
 * N.B. defining this function as partially applied confuses the compiler *)
val unsafe_run_async' : 'a io -> Thread.t

(* suspend_async cb io -- deferred run of IO with callback in another thread *)
val suspend_async : ('a -> 'b) -> 'a io -> Thread.t io

(* suspend_async' io -- deferred run of IO in another thread; no callback *)
val suspend_async' : 'a io -> Thread.t io
