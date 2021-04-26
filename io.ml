open Util

type _ io =
  | Pure : 'a -> 'a io
  | Suspend : 'a thunk -> 'a io
  | Bind : (('a -> 'b io) * 'a io) -> 'b io
  | Map : (('a -> 'b) * 'a io) -> 'b io
  | Error : (exn * 'a io) -> 'a io
  | Attempt : 'a io -> ('a, exn) result io

let pure (a : 'a) : 'a io = Pure a

let suspend (a : 'a thunk) : 'a io = Suspend a

(* Monadic bind -- deferred to support trampolined infinite recursion *)
let bind (f : 'a -> 'b io) (aio : 'a io) : 'b io = Bind (f, aio)

(* Fused map operation -- composes f << g on nested maps *)
let map (f : 'a -> 'b) (aio : 'a io) : 'b io =
  match aio with Map (g, bio) -> Map (f << g, bio) | _ -> Map (f, aio)

let attempt : 'a io -> ('a, exn) result io = fun io -> Attempt io

let ap (fio : ('a -> 'b) io) : 'a io -> 'b io =
  bind (fun a -> map (fun f -> f a) fio)

let ( <$> ) (aio : 'a io) (f : 'a -> 'b) = map f aio

let ( >>= ) (aio : 'a io) (f : 'a -> 'b io) = bind f aio

let ( <*> ) (f : ('a -> 'b) io) (aio : 'a io) : 'b io = ap f aio

(* left-to-right composition of Kleisli arrows aka "fish operator" *)
let kleisli (f : 'a -> 'b io) (g : 'b -> 'c io) : 'a -> 'c io =
 fun a -> f a >>= g

let ( >=> ) = kleisli

let productR (aio : 'a io) (bio : 'b io) : 'b io = aio >>= fun _ -> bio

let ( *> ) = productR

let productL (aio : 'a io) (bio : 'b io) : 'a io = bio >>= fun _ -> aio

let ( <* ) = productL

let raise_error (io : 'a io) (e : exn) : 'a io = Error (e, io)

let result_traverse :
    type a b. (a -> b io) -> (a, exn) result -> (b, exn) result io =
 fun f r ->
  match Result.map f r with
  | Result.Ok io -> io <$> Result.ok
  | Result.Error e -> pure (Result.Error e)

let rec unsafe_run_sync : type a. a io -> a = function
  | Pure a -> a
  | Suspend a -> a ()
  | Bind (f, io) -> (
      match io with
      | Pure a -> unsafe_run_sync (f a)
      | Suspend ta -> unsafe_run_sync (f (ta ()))
      (* Trampolining occurs here via monad associativity *)
      | Bind (g, io') -> unsafe_run_sync (Bind (g >=> f, io'))
      | Map (g, io') -> unsafe_run_sync (io' >>= fun a -> f (g a))
      | Error (e, _io) -> raise e
      | Attempt io -> unsafe_run_sync (Bind (f, lift_attempt io)))
  | Map (f, io) -> (
      match io with
      | Pure a -> f a
      | Suspend ta -> f (ta ())
      | Bind (g, io') -> unsafe_run_sync (io' >>= fun a -> g a <$> f)
      (* Map fusion via functor composition law *)
      | Map (g, io') -> unsafe_run_sync (Map (f << g, io'))
      | Error (e, _io) -> raise e
      | Attempt io -> unsafe_run_sync (Map (f, lift_attempt io)))
  | Error (e, _io) -> raise e
  | Attempt io -> unsafe_run_sync (lift_attempt io)

(* Suspends an unsafe action, catching any exceptions in a `result` *)
and lift_attempt : type a. a io -> (a, exn) result io =
 fun io ->
  suspend (fun _ ->
      try Result.ok (unsafe_run_sync io) with e -> Result.error e)
