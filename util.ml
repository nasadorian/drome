let compose (g : 'b -> 'c) (f : 'a -> 'b) (a : 'a) : 'c = g (f a)

let ( << ) = compose

let ( >> ) f g : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = g << f
