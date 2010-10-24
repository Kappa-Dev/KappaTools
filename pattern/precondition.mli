type t 

val empty : unit -> t

val add : Mixture.t -> t -> t
(**[find_all ag_nme site_num int_view lnk_view prec] returns {(mix_id,cc_id,ag_id),...}*)
val find_all : int -> int -> int option -> (int * int) option -> bool -> t -> Mods.Int2Set.t
(*val to_string : t -> string*)

