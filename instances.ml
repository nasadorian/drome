(*
 * typeclass instances for io
 *)

open Typeclasses
open Util
open Io

module IOMonad : Monad = struct
  type 'a f = 'a io

  let return a = suspend (fun _ -> a)

  let bind = bind

  let ( >>= ) fa f = bind f fa
end

module IOApplicative = MakeApplicative (IOMonad)
module IOFunctor = MakeFunctor (IOApplicative)

module IOApplicativeError : ApplicativeError with type 'a f = 'a io = struct
  type 'a f = 'a io

  let raise_error (e : exn) : 'a io = suspend (fun _ -> raise e)

  (*let handle_error_with : (exn -> 'a io) -> 'a io -> 'a io = failwith ""*)
  let handle_error_with handler = failwith ""

  (*let handle_error : (exn -> 'a) -> 'a io -> 'a io = failwith ""*)
  let handle_error = failwith ""

  (*let attempt : 'a io -> ('a, exn) result io = failwith ""*)
  let attempt = failwith ""
end
