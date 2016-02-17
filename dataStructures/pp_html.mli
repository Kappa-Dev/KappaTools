val graph_page :
  (Format.formatter -> unit) -> ?subtitle:(Format.formatter -> unit) ->
  string list -> (Format.formatter -> unit) -> (Format.formatter -> unit) ->
  Format.formatter -> unit
    (** [graph_page title deps header core f] *)
