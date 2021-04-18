module IO = struct
  type 'a thunk = unit -> 'a

  type _ io =
    | Pure : 'a -> 'a io
    | Suspend : 'a thunk -> 'a io
    | Bind : (('a -> 'b io) * 'a io) -> 'b io
    | Map : (('a -> 'b) * 'a io) -> 'b io

  let pure (a : 'a) : 'a io = Pure a

  let suspend (a : 'a thunk) : 'a io = Suspend a

  let bind (f : 'a -> 'b io) (aio : 'a io) : 'b io = Bind (f, aio)

  let map (f : 'a -> 'b) (aio : 'a io) : 'b io = Map (f, aio)

  let ap (fio : ('a -> 'b) io) (aio : 'a io) : 'b io =
    bind (fun a -> map (fun f -> f a) fio) aio

  let ( <$> ) (aio : 'a io) (f : 'a -> 'b) = map f aio

  let ( >>= ) (aio : 'a io) (f : 'a -> 'b io) = bind f aio

  let ( <*> ) (f : ('a -> 'b) io) (aio : 'a io) : 'b io = ap f aio

  let x = if true then () else ()

  let rec unsafe_run_sync : type a. a io -> a = function
    | Pure a -> a
    | Suspend a -> a ()
    | Bind (f, aio) -> f (unsafe_run_sync aio) |> unsafe_run_sync
    | Map (f, aio) -> f (unsafe_run_sync aio)
end
