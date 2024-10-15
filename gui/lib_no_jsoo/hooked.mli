(** Hooks with React-like syntax to avoid 'GC' issues *)

module type DebugPrint = sig
  val debug_print : string -> unit
end

module type S = sig
  type 'a t

  val create :
    ?debug:string ->
    ?eq:('a -> 'a -> bool) ->
    'a ->
    'a t * (?debug:string -> 'a -> unit)

  val register : 'a t -> ('a -> unit) -> unit
  val register_lwt : 'a t -> ('a -> unit Lwt.t) -> unit
  val value : 'a t -> 'a
  val set : ?debug:string -> 'a t -> 'a -> unit

  val map :
    ?debug:string -> ?eq:('a -> 'a -> bool) -> ('b -> 'a) -> 'b t -> 'a t

  val fmap :
    ?debug:string ->
    ?eq:('a -> 'a -> bool) ->
    ('b -> 'a option) ->
    'a ->
    'b t ->
    'a t

  val on : ?eq:('a -> 'a -> bool) -> bool t -> 'a -> 'a t -> 'a t
  val l2 : ?eq:('a -> 'a -> bool) -> ('b -> 'c -> 'a) -> 'b t -> 'c t -> 'a t

  (* `eq` need to be provided as it can't be extracted from React.signal *)
  val of_react_signal :
    ?debug:string -> ?eq:('a -> 'a -> bool) -> 'a React.signal -> 'a t

  val to_react_signal : 'a t -> 'a React.signal
  val const : 'a -> 'a t
end

module type E = sig
  type 'a t

  val create : ?debug:string -> unit -> 'a t * ('a -> unit)
  val register : 'a t -> ('a -> unit) -> unit
  val send : 'a t -> 'a -> unit
  val map : ?debug:string -> 'a t -> ('a -> 'b) -> 'b t
  val to_react_event : 'a t -> 'a React.event
end

module MakeS : functor (_ : DebugPrint) -> S
module MakeE : functor (_ : DebugPrint) -> E

(* TODO : Add lwt to it? *)
