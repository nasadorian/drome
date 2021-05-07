(*
  instances.ml -- typeclass instance implementations for drome types
*)

open Typeclasses
open Util
open Dsl

module IOMonad : Monad with type 'a f = 'a io = MakeMonad (struct
  type 'a f = 'a io

  (* return a -- suspends value `a` in IO
     N.B.: does not defer side effects, due to lack of CBN arguments *)
  let return a = Suspend (fun _ -> a)

  (* Monadic bind -- deferred to support trampolined infinite recursion *)
  let bind f io = Bind (f, io)
end)

module IOApplicative = MakeApplicative (IOMonad)

module IOFunctor : Functor with type 'a f = 'a io = struct
  type 'a f = 'a io

  (* Unfused map operation; maps are optimized at runtime *)
  let map f io = Map (f, io)

  let ( <$> ) io f = map f io

  let void io = map (fun _ -> ()) io
end

module IOApplicativeError : ApplicativeError with type 'a f = 'a io = struct
  type 'a f = 'a io

  let raise_error (e : exn) : 'a io = Suspend (fun _ -> raise e)

  let handle_error_with (h : exn -> 'a io) (io : 'a io) : 'a io =
    HandleErrorWith (h, io)

  let handle_error (h : exn -> 'a) (io : 'a io) : 'a io =
    HandleErrorWith (IOMonad.return << h, io)

  let attempt (io : 'a io) : ('a, exn) result io = Attempt io

  let adapt_error (h : exn -> exn) (io : 'a io) : 'a io =
    let h' e = raise_error (h e) in
    HandleErrorWith (h', io)
end

module IOMonadError = MakeMonadError (IOApplicativeError) (IOMonad)

(* Gather all the IO instances for convenient import *)
module IOInstances = struct
  include IOFunctor
  include IOApplicative
  include IOMonad
  include IOApplicativeError
  include IOMonadError
end

(* Resource instances -- deriving Monad, Applicative and Functor *)
module ResourceMonad : Monad with type 'a f = 'a resource = MakeMonad (struct
  type 'a f = 'a resource

  let return a = Allocate (Suspend (fun _ -> a), fun _ -> Pure ())

  let bind f res = RBind (f, res)
end)

module ResourceApplicative : Applicative with type 'a f = 'a resource =
  MakeApplicative (ResourceMonad)

module ResourceFunctor : Functor with type 'a f = 'a resource =
  MakeFunctor (ResourceApplicative)

(* Gather all Resource instances for convenient import *)
module ResourceInstances = struct
  include ResourceFunctor
  include ResourceApplicative
  include ResourceMonad
end
