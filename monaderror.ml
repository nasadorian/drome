module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t

  val ap : ('a -> 'b) t -> 'a t -> 'b t
end

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type ApplicativeError = sig
  type 'a t

  val raise_error : exn -> 'a t

  val handle_error_with : (exn -> 'a t) -> 'a t -> 'a t

  val handle_error : (exn -> 'a) -> 'a t -> 'a t

  val attempt : 'a t -> (exn, 'a) result t
end

module type MondError = sig end
