open Util

module type Functor = sig
  type 'a f

  val map : ('a -> 'b) -> 'a f -> 'b f
end

module type Applicative = sig
  type 'a f

  val pure : 'a -> 'a f

  val ap : ('a -> 'b) f -> 'a f -> 'b f
end

module type Monad = sig
  type 'a f

  val return : 'a -> 'a f

  val bind : ('a -> 'b f) -> 'a f -> 'b f
end

module type ApplicativeError = sig
  type 'a f

  val raise_error : exn -> 'a f

  val handle_error_with : (exn -> 'a f) -> 'a f -> 'a f

  val handle_error : (exn -> 'a) -> 'a f -> 'a f

  val attempt : 'a f -> (exn, 'a) result f
end

module type MonadError = sig
  type 'a f

  val rethrow : exn -> (exn, 'a) result f -> 'a f

  val ensure : ('a -> bool) -> exn thunk -> 'a f -> 'a f
end
