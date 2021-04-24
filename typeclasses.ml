open Util

(*         
                  ApplicativeError ----- MonadError
                       /                /   
                      /                / 
                     /                /
     Functor ---- Applicative ----- Monad 

 *)

module type Functor = sig
  type _ f

  val map : ('a -> 'b) -> 'a f -> 'b f

  val ( <$> ) : 'a f -> ('a -> 'b) -> 'b f
end

module type Applicative = sig
  type _ f

  val pure : 'a -> 'a f

  val ap : ('a -> 'b) f -> 'a f -> 'b f

  val ( <*> ) : ('a -> 'b) f -> 'a f -> 'b f
end

module type Monad = sig
  type _ f

  val return : 'a -> 'a f

  val bind : ('a -> 'b f) -> 'a f -> 'b f

  val ( >>= ) : 'a f -> ('a -> 'b f) -> 'b f
end

module type ApplicativeError = sig
  type _ f

  val raise_error : exn -> 'a f

  (*val handle_error_with : (exn -> 'a f) -> 'a f -> 'a f*)

  (*val handle_error : (exn -> 'a) -> 'a f -> 'a f*)

  val attempt : 'a f -> (exn, 'a) result f
end

module type MonadError = sig
  type _ f

  val rethrow : exn -> (exn, 'a) result f -> 'a f

  val ensure : ('a -> bool) -> exn thunk -> 'a f -> 'a f
end

(*
 * Typeclass deriving "functors"
 *)

module MakeApplicative (M : Monad) : Applicative with type 'a f = 'a M.f =
struct
  open M

  type 'a f = 'a M.f

  let pure = return

  let ap (ff : ('a -> 'b) M.f) (fa : 'a M.f) : 'b M.f =
    ff >>= fun f ->
    fa >>= fun a -> return (f a)

  let ( <*> ) = ap
end

module MakeFunctor (A : Applicative) : Functor with type 'a f = 'a A.f = struct
  open A

  type 'a f = 'a A.f

  let map f fa = pure f <*> fa

  let ( <$> ) fa f = map f fa
end

module MakeApplicativeError (A : Applicative) :
  ApplicativeError with type 'a f = 'a A.f = struct
  type 'a f = 'a A.f

  let raise_error (e : exn) : 'a A.f = A.pure (raise e)

  let attempt fa = failwith ""
end
