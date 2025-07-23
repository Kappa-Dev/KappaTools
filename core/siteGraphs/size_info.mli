type size_sig = {
  threshold_sig_name: string Loc.annoted;
  threshold_sig_value: string option Loc.annoted list;
  threshold_sig_agent_name: string option; 
  threshold: int;
}

type t = size_sig option array array
type previous_threshold_thread = int array
type previous_threshold = previous_threshold_thread * previous_threshold_thread array 

val previous_threshold_init: unit -> previous_threshold 
val copy_previous_threshold: previous_threshold -> previous_threshold 
val name_of_size_predicate_before_compil : string option -> int -> string
val name_of_size_predicate : Signature.s -> int option -> int -> string
val get_size_predicate_site : int -> int option -> int -> Signature.s -> int
val get_internal_state_true : int -> int option -> int -> Signature.s -> int
val get_internal_state_false : int -> int option -> int -> Signature.s -> int
val print_size_predicate : t -> int -> int -> Format.formatter -> unit

val print_kappa :
  noSizepredicates:bool -> Signature.s -> Format.formatter -> t -> unit

val to_yojson : filenames:int Mods.StringMap.t -> t -> Yojson.Basic.t
val of_yojson : filenames:string array -> Yojson.Basic.t -> t

val get_size_predicate_sites_sig :
  ?except:exn -> Signature.s -> t -> int -> int -> size_sig

val compute_threshold: (Operator.compare_op * bool) * int -> int 