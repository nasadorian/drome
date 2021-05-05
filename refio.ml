(*
  refio.ml -- a purely functional, thread-safe reference type
*)

open Dsl

type 'a ioref = Ref of 'a ref

type 'a f = 'a ioref

(* make a -- lift a value into a thread-safe mutable reference *)
let make (a : 'a) : 'a ioref io = IO.make (fun _ -> Ref (ref a))

(* set a r -- set value into ref; internally equivalent to `r := a` *)
let set (a : 'a) (Ref r : 'a ioref) : unit io = IO.make (fun _ -> r := a)

(* get r -- extract value from ref *)
let get (Ref r : 'a ioref) : 'a io = IO.make (fun _ -> !r)

(* update f r -- atomically update the ref value with `f` *)
let update (f : 'a -> 'a) (Ref r : 'a ioref) : unit io =
  (* compare and set loop; allows lock-free atomicity *)
  let rec cas a ar = if a = !ar then ar := f a else cas !ar ar in
  IO.make (fun _ -> cas !r r)

(* modify f r -- thread-safe transformation of the value in the ref;
 * this is similar to the State monad's modify function *)
let modify (f : 'a -> 'a * 'b) (Ref r : 'a ioref) : 'b io =
  let rec cas a ar =
    if a = !ar then (
      let next, out = f a in
      r := next;
      out)
    else cas !ar ar
  in
  IO.make (fun _ -> cas !r r)
