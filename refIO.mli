(*
  refIO.mli -- API for purely functional, thread-safe references
*)

open Dsl

type 'a f

(* make a -- lift a value into a thread-safe mutable reference *)
val make : 'a -> 'a f io

(* set a r -- set value into ref; internally equivalent to `r := a` *)
val set : 'a -> 'a f -> unit io

(* get r -- extract value from ref *)
val get : 'a f -> 'a io

(* update f r -- atomically update the ref value with `f` *)
val update : ('a -> 'a) -> 'a f -> unit io

(* modify f r -- compute a result `b` from the value in the ref, state monad
 * style *)
val modify : ('a -> 'a * 'b) -> 'a f -> 'b io
