type t 
type id = int  
type update = {id: id ; previous_size: int ; current_size: int} 
type updates = update list 

val init: t 

val create: t -> id -> t 
val bind: t -> id -> id -> t
val unbind: t -> id -> id -> t 
val degrade: neightbor:(id -> id list) -> t -> id -> t 

val flush: neightbor:(id -> id list) -> t -> t * updates 

