type 'a working_list

val make : int -> 'a working_list
val indice : 'a working_list -> int
val clean : 'a working_list -> unit
val list : 'a working_list -> 'a list
val pop : 'a working_list -> 'a option
val push : 'a -> 'a working_list -> unit
val exists : ('a -> bool) -> 'a working_list -> bool
val not_empty : 'a working_list -> bool
val copy : 'a working_list -> 'a working_list
val member : 'a -> 'a working_list -> bool
