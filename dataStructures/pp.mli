val list : (out_channel -> unit) ->
(out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
val set : ('b -> 'a list) -> (out_channel -> unit) ->
(out_channel -> 'a -> unit) -> out_channel -> 'b -> unit
val string : out_channel -> string -> unit
val int : out_channel -> int -> unit
val comma : out_channel -> unit
val colon : out_channel -> unit
val space : out_channel -> unit
