open Mods

(** Stores a bunch of stuff the user gave a name to *)

type 'a t = private
    { decls : (string Term.with_pos *'a) array;
      (** the name of the stuff * the stuff *)
      finder : int StringMap.t;
    (** [fst (fst d.decls.(StringMap.find s d.finder))] MUST be equal to [s] *)
 }

val create : (string Term.with_pos *'a) array -> 'a t
val size : 'a t -> int
val elt_name : 'a t -> int -> string
val elt_id : ?kind:string -> 'a t -> string Term.with_pos -> int

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
