(*
 * typeclass instances for io
 *)

open Typeclasses
open Util
open Io_base

module IOMonad : Monad with type 'a f = 'a io = struct
  type 'a f = 'a io

  (* return a -- suspends value `a` in IO
     N.B.: does not defer side effects, due to lack of CBN arguments *)
  let return a = Suspend (fun _ -> a)

  (* Monadic bind -- deferred to support trampolined infinite recursion *)
  let bind f io = Bind (f, io)

  let ( >>= ) io f = Bind (f, io)

  (* left-to-right composition of Kleisli arrows aka the "fish operator" *)
  let ( >=> ) f g a = f a >>= g

  let productR (aio : 'a io) (bio : 'b io) : 'b io = aio >>= fun _ -> bio

  let ( *> ) = productR

  let productL (aio : 'a io) (bio : 'b io) : 'a io = bio >>= fun _ -> aio

  let ( <* ) = productL
end

module IOApplicative = MakeApplicative (IOMonad)

module IOFunctor : Functor with type 'a f = 'a io = struct
  type 'a f = 'a io

  (* Unfused map operation; maps are optimized at runtime *)
  let map f io = Map (f, io)

  let ( <$> ) io f = map f io
end

module IOApplicativeError : ApplicativeError with type 'a f = 'a io = struct
  type 'a f = 'a io

  let raise_error (e : exn) : 'a io = Suspend (fun _ -> raise e)

  let handle_error_with (h : exn -> 'a io) (io : 'a io) : 'a io =
    HandleErrorWith (h, io)

  let handle_error (h : exn -> 'a) (io : 'a io) : 'a io =
    HandleErrorWith (IOMonad.return << h, io)

  let attempt (io : 'a io) : ('a, exn) result io = Attempt io

  let adapt_error (io : 'a io) (h : exn -> exn) : 'a io =
    let h' e = raise_error (h e) in
    HandleErrorWith (h', io)
end

module IOMonadError = MakeMonadError (IOApplicativeError) (IOMonad)
