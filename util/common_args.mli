type t = { mutable backtrace           : bool ;
	   mutable debug               : bool;
	   mutable timeIndependent     : bool }

val default : t
(* return options *)
val options : t -> (string * Arg.spec * string) list
