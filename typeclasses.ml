open Util

(*         
           The typeclass hierarchy used in `drome`
 
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

  val ( >=> ) : ('a -> 'b f) -> ('b -> 'c f) -> 'a -> 'c f

  val productR : 'a f -> 'b f -> 'b f

  val ( *> ) : 'a f -> 'b f -> 'b f

  val productL : 'a f -> 'b f -> 'a f

  val ( <* ) : 'a f -> 'b f -> 'a f
end

module type ApplicativeError = sig
  type _ f

  val raise_error : exn -> 'a f

  val handle_error_with : (exn -> 'a f) -> 'a f -> 'a f

  val handle_error : (exn -> 'a) -> 'a f -> 'a f

  val attempt : 'a f -> ('a, exn) result f

  val adapt_error : 'a f -> (exn -> exn) -> 'a f
end

module type MonadError = sig
  type _ f

  val rethrow : ('a, exn) result f -> 'a f

  val ensure : ('a -> bool) -> exn -> 'a f -> 'a f
end

(* Derive Applicative from Monad *)
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

(* Derive Functor via Applicative *)
module MakeFunctor (A : Applicative) : Functor with type 'a f = 'a A.f = struct
  open A

  type 'a f = 'a A.f

  let map f fa = pure f <*> fa

  let ( <$> ) fa f = map f fa
end

(* Derive MonadError from ApplicativeError and Monad *)
module MakeMonadError (A : ApplicativeError) (M : Monad with type 'a f = 'a A.f) :
  MonadError with type 'a f = 'a M.f = struct
  type 'a f = 'a M.f

  let rethrow ioa = M.bind (Result.fold ~ok:M.return ~error:A.raise_error) ioa

  let ensure p e io =
    M.bind (fun a -> if p a then M.return a else A.raise_error e) io
end
