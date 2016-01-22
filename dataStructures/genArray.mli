(** Signature of array only limited by max_int *)

module type GenArray = sig
    type 'a t
    val create: int -> 'a -> 'a t
    val length: 'a t -> int
    val get: 'a t -> int -> 'a
    val set: 'a t -> int -> 'a -> unit
    val init: int -> (int -> 'a) -> 'a t
    val make: int -> 'a -> 'a t
    val append: 'a t -> 'a t -> 'a t
    val make_matrix: int -> int -> 'a -> 'a t t
    val concat: 'a t list -> 'a t
    val sub: 'a t -> int -> int -> 'a t
    val copy: 'a t -> 'a t
    val fill: 'a t -> int -> int -> 'a -> unit
    val of_list: 'a list -> 'a t
    val iter: ('a -> unit) -> 'a t -> unit
    val iteri: (int -> 'a -> unit) -> 'a t -> unit
    val blit: 'a t -> int -> 'a t -> int -> int -> unit
    val fold_lefti: (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val fold_righti: (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val print:
      ?trailing:(Format.formatter -> unit) -> (Format.formatter -> unit) ->
      (int -> Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
