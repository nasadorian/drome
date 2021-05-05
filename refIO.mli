(*
  refIO.mli -- API for purely functional, thread-safe references
*)

open Dsl

type 'a f

val make : 'a -> 'a f io

val set : 'a -> 'a f -> unit io

val get : 'a f -> 'a io

val update : ('a -> 'a) -> 'a f -> unit io

val modify : ('a -> 'a * 'b) -> 'a f -> 'b io
