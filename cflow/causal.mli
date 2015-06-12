type event_kind = OBS of int | RULE of int | INIT of int | PERT of int
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

val empty_grid : unit -> grid

val record :
  ?decorate_with:(int * 'a) list -> Primitives.rule -> Mods.Int2Set.t ->
  int Mods.IntMap.t * int Mods.IntMap.t -> bool -> int -> grid ->
  Environment.t -> grid
(** [record ?decorate_with rule side_effects (embedding,fresh_map)
	   is_weak event_number grid env] *)
val record_init :
  ((int * int) * (int * ('a option * 'b)) list) * 'c list ->
  bool -> int -> grid -> Environment.t -> grid
(** [record_init init is_weak event_number grid env] *)
val record_obs :
  Mods.Int2Set.t -> (int * Mixture.t * int Mods.IntMap.t * 'a) * 'b ->
  bool -> int -> grid -> Environment.t -> grid
(** [record_obs side_effects ((r_id,state,embedding,_),test)
	       is_weak event_number grid env] *)

val label : Environment.t -> State.t -> event_kind -> string
val cut : (int * int * int) list -> grid -> config
val enrich_grid :
  Format.formatter -> Graph_closure.config -> grid -> enriched_grid

val pretty_print :
  Format.formatter -> Graph_closure.config -> string -> string ->
  (grid * 'a Mods.simulation_info option list) list -> State.t ->
  Environment.t -> unit
(** [pretty_print err_fmt config_closure compression_type label story_list
                  state env] *)
val print_stat :
  Format.formatter -> 'a -> 'b -> enriched_grid -> unit
