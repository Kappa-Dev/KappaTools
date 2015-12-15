open Mods

(** Stores a bunch of stuff the user gave a name to *)

type 'a t = private
    { decls : (string Location.annot *'a) array;
      (** the name of the stuff * the stuff *)
      finder : int StringMap.t;
    (** [fst (fst d.decls.(StringMap.find s d.finder))] MUST be equal to [s] *)
 }

val create : (string Location.annot *'a) array -> 'a t
val size : 'a t -> int
val elt_name : 'a t -> int -> string
val elt_id : ?kind:string -> 'a t -> string Location.annot -> int

val fold : (int -> string -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val print :
  sep:(Format.formatter -> unit) ->
  (int -> string -> Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit

val debug_print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
