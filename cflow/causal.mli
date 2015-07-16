type event_kind = OBS of int | RULE of int | INIT | PERT of int

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
      weak_list: int list ;
      init_tbl: (int,Mods.IntSet.t) Hashtbl.t;(*decreasing*)
      init_to_eidmax: (int,int) Hashtbl.t;
    }
type config =
    {
      events: atom Mods.IntMap.t ;
      prec_1: Mods.IntSet.t Mods.IntMap.t ;
      conflict : Mods.IntSet.t Mods.IntMap.t ;
      top : Mods.IntSet.t}
type enriched_grid =
    {
      config:config;
      ids:(int * int * int) list ;
      depth:int;
      prec_star: int list array ; (*decreasing*)
      depth_of_event: int Mods.IntMap.t ;
      size:int;
    }

val label : Environment.t -> event_kind -> string
val empty_grid : unit -> grid

val record :
  (int * Primitives.Instantiation.concrete Primitives.Instantiation.event) ->
  bool -> int -> Environment.t -> grid -> grid
val record_obs :
  (int * Primitives.Instantiation.concrete Primitives.Instantiation.test list
   * unit Mods.simulation_info) ->
  Primitives.Instantiation.concrete Primitives.Instantiation.site list ->
  bool -> int -> grid -> grid
val record_init :
  Primitives.Instantiation.concrete Primitives.Instantiation.action list ->
  bool -> int -> Environment.t -> grid -> grid

val cut : (int * int * int) list -> grid -> config
val enrich_grid :
  Format.formatter -> Graph_closure.config -> grid -> enriched_grid

val pretty_print :
  Format.formatter -> Environment.t -> Graph_closure.config -> string ->
  string -> (grid * 'a Mods.simulation_info option list) list -> unit
(** [pretty_print err_fmt env config_closure compression_type label story_list
                  state env] *)
val print_stat :
  Format.formatter -> 'a -> 'b -> enriched_grid -> unit
