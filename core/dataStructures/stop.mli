type ('a, 'b) stop

val success : 'a -> ('a, 'b) stop
val stop : 'b -> ('a, 'b) stop
val success_or_stop : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) stop -> 'c
