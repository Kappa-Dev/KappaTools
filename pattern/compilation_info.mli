type place = Connected_component.ContentAgent.t * int

type t = {
  agents_in_syntactic_rule : place list;
  (** id of cc in connected_components *)
  sites_of_side_effect : (place * int) list;
  (** id of cc in ..., id_of site *)
  sites_of_internal_state_side_effect : (place * int) list;
  (** id of cc in ..., id_of site *)
}

val of_empty_rule : t
val add_agent : t -> place -> (bool array * bool array) option -> t
val rename :
  Connected_component.work -> int -> Connected_component.cc ->
  Renaming.t -> t -> t
