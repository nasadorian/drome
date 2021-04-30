open Util

type _ io =
  | Pure : 'a -> 'a io
  | Suspend : 'a thunk -> 'a io
  | Bind : (('a -> 'b io) * 'a io) -> 'b io
  | Map : (('a -> 'b) * 'a io) -> 'b io
  | RaiseError : exn -> 'a io
  | Attempt : 'a io -> ('a, exn) result io
  | HandleErrorWith : ((exn -> 'a io) * 'a io) -> 'a io
