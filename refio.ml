(*
  ioref.ml -- purely functional, thread-safe references
*)

open Dsl
open Drome

module type RefIOAPI = sig
  type 'a f

  val make : 'a -> 'a f io

  val set : 'a -> 'a f -> unit io

  val get : 'a f -> 'a io

  val update : ('a -> 'a) -> 'a f -> unit io

  val modify : ('a -> 'a * 'b) -> 'a f -> 'b io
end

module RefIO : RefIOAPI = struct
  open Dsl

  type 'a ioref = Ref of 'a ref

  type 'a f = 'a ioref

  let make (a : 'a) : 'a ioref io = IO.make (fun _ -> Ref (ref a))

  let set (a : 'a) (Ref r : 'a ioref) : unit io = IO.make (fun _ -> r := a)

  let get (Ref r : 'a ioref) : 'a io = IO.make (fun _ -> !r)

  let update (f : 'a -> 'a) (Ref r : 'a ioref) : unit io =
    (* compare and set loop; allows lock-free atomicity *)
    let rec cas a ar = if a = !ar then ar := f a else cas !ar ar in
    IO.make (fun _ -> cas !r r)

  let modify (f : 'a -> 'a * 'b) (Ref r : 'a ioref) : 'b io =
    let rec cas a ar = if a = !ar then snd (f a) else cas !ar ar in
    IO.make (fun _ -> cas !r r)
end
