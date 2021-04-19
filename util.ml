let compose (g : 'b -> 'c) (f : 'a -> 'b) (a : 'a) : 'c = g (f a)

let ( << ) g f = compose g f

let ( >> ) = Fun.flip ( << )
