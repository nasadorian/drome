(*
  resource.ml -- closeable and composable resource management
*)

open Dsl
include Instances.ResourceInstances
module IO = Io

(* make a r -- acquire resource `a` in IO, and encode how to release it
 * with function `r` *)
let make (acq : 'a io) (rel : 'a -> unit io) : 'a resource = Allocate (rel, acq)

(* use u r -- apply `u` a resource then release it *)
let rec use : type a b. (a -> b io) -> a resource -> b io =
 fun u r ->
  match r with
  | Allocate (release, acquire) ->
      IO.(
        acquire >>= fun a ->
        (* handle failure during use and ensure resource release *)
        let action =
          handle_error_with (fun e -> release a *> raise_error e) (u a)
        in
        action <* release a)
  | RBind (f, res) -> (
      match res with
      | Allocate (release, acquire) ->
          IO.(
            acquire >>= fun a ->
            let res' = f a in
            use u res' <* release a)
      | RBind (g, res') -> use u (RBind (g >=> f, res'))
      | RPure a -> use u (f a))
  | RPure a -> u a

(* use' r -- acquire and release resource with a no-op `u` action *)
let use' : type a. a resource -> a io = fun r -> use IO.pure r
