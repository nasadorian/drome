open Util
open Dsl
open Typeclasses

include Functor with type 'a f = 'a io

include Applicative with type 'a f = 'a io

include Monad with type 'a f = 'a io

include ApplicativeError with type 'a f = 'a io

include MonadError with type 'a f = 'a io

val make : 'a thunk -> 'a io

val noop : unit io

val sleep : float -> unit io

val unsafe_run_sync : 'a io -> 'a

val unsafe_run_async : ('a -> 'b) -> 'a io -> Thread.t

val unsafe_run_async' : 'a io -> Thread.t

val suspend_async : ('a -> 'b) -> 'a io -> Thread.t io

val suspend_async' : 'a io -> Thread.t io
