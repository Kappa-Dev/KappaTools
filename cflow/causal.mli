type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (** the agents *)
  | PERT of string (** the rule *)

type quark_lists = {
  site_tested : (int * int) list;
  site_modified : (int * int) list;
  internal_state_tested : (int * int) list;
  internal_state_modified : (int * int) list;
}

type atom =
    {
      causal_impact : int ; (*(1) tested (2) modified, (3) tested + modified*)
      eid:int ; (*event identifier*)
      kind:event_kind ;
(*      observation: string list*)
    }

type attribute = atom list (*vertical sequence of atoms*)
type grid =
    {
      flow: (int*int*int,attribute) Hashtbl.t ;
      (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i:
      link (1) or internal state (0) *)
      pid_to_init: (int*int*int,int) Hashtbl.t ;
      obs: int list ;
      init_tbl: (int,Mods.IntSet.t) Hashtbl.t;(*decreasing*)
      init_to_eidmax: (int,int) Hashtbl.t;
    }
type config =
  {
    events_kind: event_kind Mods.IntMap.t ;
    prec_1: Mods.IntSet.t Mods.IntMap.t ;
    conflict : Mods.IntSet.t Mods.IntMap.t ;
  }
type enriched_grid =
    {
      config:config;
      depth:int;
      prec_star: (int list array * Graph_closure.order) ; 
      depth_of_event: int Mods.IntMap.t ;
      size:int;
    }

val label : ?env:Environment.t -> event_kind -> string
val empty_grid : unit -> grid

val record :
  (event_kind *
     Instantiation.concrete Instantiation.event) -> int -> Environment.t -> grid -> grid
val record_obs :
  (event_kind *
     Instantiation.concrete Instantiation.test list
   * unit Mods.simulation_info) ->
  Instantiation.concrete Instantiation.site list -> int -> grid -> grid
val record_init :
  int list * Instantiation.concrete Instantiation.action list ->
  int -> Environment.t -> grid -> grid

val cut : (int * int * int) list -> grid -> config
val enrich_grid :
  Format.formatter -> Graph_closure.config -> grid -> enriched_grid

val debug_print_grid : Format.formatter -> grid -> unit
val pretty_print :
  Format.formatter -> Environment.t -> Graph_closure.config -> string ->
  string -> (grid * 'a Mods.simulation_info list) list -> unit
(** [pretty_print err_fmt env config_closure compression_type label story_list
                  state env] *)
val print_stat :
  Format.formatter -> 'a -> 'b -> enriched_grid -> unit
