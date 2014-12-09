(* Shell-like expansion, using the wordexp standard library call.
   Copyright (C) Antoine Mine' 2006
*)

external wordexp: string -> string list = "ml_wordexp"
(** May raise a [Failure] or an [Invalid_argument] *)
