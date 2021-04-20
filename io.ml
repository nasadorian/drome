open Util

module IO = struct
  type _ io =
    | Pure : 'a -> 'a io
    | Suspend : 'a thunk -> 'a io
    | Defer : 'a io thunk -> 'a io
    | Bind : (('a -> 'b io) * 'a io) -> 'b io
    | Map : (('a -> 'b) * 'a io) -> 'b io

  let pure (a : 'a) : 'a io = Pure a

  let suspend (a : 'a thunk) : 'a io = Suspend a

  let bind (f : 'a -> 'b io) (aio : 'a io) : 'b io =
    Defer (fun _ -> Bind (f, aio))

  let map (f : 'a -> 'b) (aio : 'a io) : 'b io =
    match aio with Map (g, bio) -> Map (f << g, bio) | _ -> Map (f, aio)

  let ap (fio : ('a -> 'b) io) : 'a io -> 'b io =
    bind (fun a -> map (fun f -> f a) fio)

  let ( <$> ) (aio : 'a io) (f : 'a -> 'b) = map f aio

  let ( >>= ) (aio : 'a io) (f : 'a -> 'b io) = bind f aio

  let ( <*> ) (f : ('a -> 'b) io) (aio : 'a io) : 'b io = ap f aio

  let productR (aio : 'a io) (bio : 'b io) : 'b io = aio >>= fun _ -> bio

  let ( *> ) = productR

  let productL (aio : 'a io) (bio : 'b io) : 'a io = bio >>= fun _ -> aio

  let ( <* ) = productL

  let rec unsafe_run_sync : type a. a io -> a = function
    | Pure a -> a
    | Suspend a -> a ()
    | Defer io -> unsafe_run_sync (io ())
    | Bind (f, aio) -> (
        match aio with
        | Pure a -> unsafe_run_sync (f a)
        | Suspend ta -> unsafe_run_sync (f (ta ()))
        | Defer io -> unsafe_run_sync (io () >>= f)
        | Bind (g, aio') -> unsafe_run_sync (aio' >>= fun a -> g a >>= f)
        | Map (g, aio') -> unsafe_run_sync (aio' >>= fun a -> f (g a)))
    | Map (f, aio) -> (
        match aio with
        | Pure a -> f a
        | Suspend ta -> f (ta ())
        | Defer io -> unsafe_run_sync (io () <$> f)
        | Bind (g, aio') -> unsafe_run_sync (aio' >>= fun a -> g a <$> f)
        | Map (g, aio') -> unsafe_run_sync (Map (f << g, aio')))

  (* TODO: rewrite this as part of AST *)
  let attempt (aio : 'a io) : (exn, 'a) result io =
    suspend @@ fun _ ->
    try Result.ok (unsafe_run_sync aio) with e -> Result.error e
end
