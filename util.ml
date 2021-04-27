type 'a thunk = unit -> 'a

let compose (g : 'b -> 'c) (f : 'a -> 'b) (a : 'a) : 'c = g (f a)

let ( << ) = compose

let ( >> ) f g : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = g << f

let flip (f : 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b
