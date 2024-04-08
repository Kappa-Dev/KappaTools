(** Hooks with React-like syntax to avoid 'GC' issues *)

module S : sig
  type 'a t

  val create : ?debug:string -> ?eq:('a -> 'a -> bool) -> 'a -> 'a t
  val register : 'a t -> ('a -> unit) -> unit
  val v : 'a t -> 'a
  val set : ?debug:string -> 'a t -> 'a -> unit

  val bind :
    ?debug:string -> ?eq:('a -> 'a -> bool) -> 'b t -> ('b -> 'a) -> 'a t

  val to_react_signal : 'a t -> 'a React.signal
end

module E : sig
  type 'a t

  val create : ?debug:string -> unit -> 'a t
  val register : 'a t -> ('a -> unit) -> unit
  val send : 'a t -> 'a -> unit
  val map : ?debug:string -> 'a t -> ('a -> 'b) -> 'b t
  val to_react_event : 'a t -> 'a React.event
end

(* TODO : Add lwt to it? *)
