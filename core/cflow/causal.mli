type quark_lists = {
  site_tested: (int * int) list;
  site_modified: (int * int) list;
  internal_state_tested: (int * int) list;
  internal_state_modified: (int * int) list;
}

type event_kind = OBS of string | EVENT of Trace.event_kind

type atom = {
  causal_impact: int; (*(1) tested (2) modified, (3) tested + modified*)
  eid: int; (*event identifier*)
  kind: event_kind; (*      observation: string list*)
}

type attribute = atom list (*vertical sequence of atoms*)

type grid = {
  flow: (int * int * int, attribute) Hashtbl.t;
  (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i:
    link (1) or internal state (0) *)
  pid_to_init: (int * int * int, int) Hashtbl.t;
  obs: int list;
  init_tbl: (int, Mods.IntSet.t) Hashtbl.t; (*decreasing*)
  init_to_eidmax: (int, int) Hashtbl.t;
}

type config = {
  events_kind: event_kind Mods.IntMap.t;
  prec_1: Mods.IntSet.t Mods.IntMap.t;
  conflict: Mods.IntSet.t Mods.IntMap.t;
}

type enriched_grid = {
  config: config;
  depth: int;
  prec_star: int list array * Graph_closure.order;
  depth_of_event: int Mods.IntMap.t;
  size: int;
}

type formatCflow = Dot | Html | Json

val empty_grid : unit -> grid

val record :
  Trace.event_kind
  * Instantiation.concrete Instantiation.event
  * unit Trace.Simulation_info.t ->
  int ->
  Model.t ->
  grid ->
  grid

val record_obs :
  string
  * Instantiation.concrete Instantiation.test list list
  * unit Trace.Simulation_info.t ->
  Instantiation.concrete Instantiation.site list ->
  int ->
  grid ->
  grid

val record_init :
  int list * Instantiation.concrete Instantiation.action list ->
  int ->
  Model.t ->
  grid ->
  grid

val cut :
  ?with_reduction:bool ->
  Remanent_parameters_sig.parameters ->
  'a ->
  StoryProfiling.StoryStats.log_info ->
  Exception.method_handler ->
  (int * int * int) list ->
  grid ->
  Exception.method_handler * StoryProfiling.StoryStats.log_info * config

val enrich_grid :
  Remanent_parameters_sig.parameters ->
  'a ->
  StoryProfiling.StoryStats.log_info ->
  Exception.method_handler ->
  Graph_closure.config ->
  grid ->
  Exception.method_handler * StoryProfiling.StoryStats.log_info * enriched_grid

val fold_over_causal_past_of_obs :
  Remanent_parameters_sig.parameters ->
  'a ->
  StoryProfiling.StoryStats.log_info ->
  Exception.method_handler ->
  Graph_closure.config ->
  grid ->
  (Remanent_parameters_sig.parameters ->
  'a ->
  StoryProfiling.StoryStats.log_info ->
  Exception.method_handler ->
  Graph_closure.M.elt ->
  int list ->
  'b ->
  Exception.method_handler
  * StoryProfiling.StoryStats.log_info
  * ('b, 'c) Stop.stop) ->
  'b ->
  ( Exception.method_handler * StoryProfiling.StoryStats.log_info * 'b,
    Exception.method_handler * StoryProfiling.StoryStats.log_info * 'c )
  Stop.stop

val debug_print_grid : Format.formatter -> grid -> unit

val pretty_print :
  dotFormat:formatCflow ->
  Remanent_parameters_sig.parameters ->
  'a ->
  StoryProfiling.StoryStats.log_info ->
  Exception.method_handler ->
  Model.t ->
  Graph_closure.config ->
  string ->
  string ->
  (Trace.t * grid * 'b Trace.Simulation_info.t list) list ->
  Exception.method_handler * StoryProfiling.StoryStats.log_info
(** [pretty_print err_fmt env config_closure compression_type label story_list
                  state env] *)

val print_stat : Format.formatter -> 'a -> 'b -> enriched_grid -> unit
