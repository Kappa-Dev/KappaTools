type size_sig = {
  threshold_sig_name: string Loc.annoted;
  threshold_sig_value: string option Loc.annoted list;
  threshold: int;
}

type t = size_sig option array array
type previous_threshold = int array

val name_of_size_predicate : int -> string
val get_size_predicate_site : int -> int -> Signature.s -> int
val get_internal_state_true : int -> int -> Signature.s -> int
val print_size_predicate : t -> int -> int -> Format.formatter -> unit

val print_kappa :
  noSizepredicates:bool -> Signature.s -> Format.formatter -> t -> unit

val to_yojson : filenames:int Mods.StringMap.t -> t -> Yojson.Basic.t
val of_yojson : filenames:string array -> Yojson.Basic.t -> t

val get_size_predicate_sites_sig :
  ?except:exn -> Signature.s -> t -> int -> int -> size_sig
