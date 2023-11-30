(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type compression_modes = { causal: bool; weak: bool; strong: bool }
type 'ok result = ('ok, Result_util.message list) Result_util.t
type project_id = string

class type manager_environment = object
  method environment_info : unit -> Api_types_t.environment_info result Lwt.t
end

class type virtual manager_model = object
  method virtual is_running : bool
  method secret_project_parse : Ast.parsing_compil result Lwt.t
  method project_overwrite : string -> Ast.parsing_compil -> unit result Lwt.t
  method file_catalog : Kfiles.catalog_item list result Lwt.t
  method file_create : int -> string -> string -> unit result Lwt.t
  method file_get : string -> (string * int) result Lwt.t
  method file_update : string -> string -> unit result Lwt.t
  method file_move : int -> string -> unit result Lwt.t
  method file_delete : string -> unit result Lwt.t
end

class type manager_file_line = object
  method simulation_catalog_file_line :
    Api_types_t.file_line_catalog result Lwt.t

  method simulation_detail_file_line : string -> string list result Lwt.t
end

class type manager_din = object
  method simulation_catalog_din : Api_types_t.din_catalog result Lwt.t

  method simulation_detail_din :
    Api_types_t.din_id -> Api_types_t.din result Lwt.t
end

class type manager_log_message = object
  method simulation_detail_log_message : Api_types_t.log_message result Lwt.t
end

class type manager_plot = object
  method simulation_detail_plot :
    Api_types_t.plot_parameter -> Api_types_t.plot result Lwt.t
end

class type manager_snapshot = object
  method simulation_catalog_snapshot : Api_types_t.snapshot_catalog result Lwt.t

  method simulation_detail_snapshot :
    Api_types_t.snapshot_id -> Api_types_t.snapshot result Lwt.t
end

class type manager_simulation = object
  method secret_simulation_load :
    Pattern.sharing_level ->
    Ast.parsing_compil ->
    (string * Nbr.t) list ->
    unit result Lwt.t

  method simulation_delete : unit result Lwt.t

  method simulation_start :
    Api_types_t.simulation_parameter ->
    Api_types_t.simulation_artifact result Lwt.t

  method simulation_pause : unit result Lwt.t

  method simulation_intervention :
    Api_types_t.simulation_intervention -> string result Lwt.t

  method simulation_continue : string -> unit result Lwt.t
  method simulation_info : Api_types_t.simulation_info result Lwt.t
  method simulation_efficiency : Counter.Efficiency.t result Lwt.t
  method simulation_parameter : Api_types_t.simulation_parameter result Lwt.t
  method simulation_raw_trace : string result Lwt.t
  method simulation_outputs_zip : Bigbuffer.bigstring result Lwt.t
  inherit manager_file_line
  inherit manager_din
  inherit manager_log_message
  inherit manager_plot
  inherit manager_snapshot
end

type 'a kasa_reply =
  ('a, Exception_without_parameter.method_handler) Lwt_result.t

class type manager_static_analysis = object
  method init_static_analyser : Ast.parsing_compil -> unit kasa_reply

  method init_static_analyser_raw : string -> unit kasa_reply
  (** The string has to be the json corresponding to an [Ast.parsing_compil] *)

  method get_contact_map :
    Public_data.accuracy_level option -> Yojson.Basic.t kasa_reply

  method get_pos_of_rules_and_vars :
    Public_data.pos_of_rules_and_vars kasa_reply

  method get_influence_map_raw :
    Public_data.accuracy_level option -> string kasa_reply

  method get_local_influence_map :
    ?fwd:int ->
    ?bwd:int ->
    ?origin:(int, int) Public_data.influence_node ->
    total:int ->
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level
    * int
    * int option
    * int option
    * (Public_data.rule, Public_data.var) Public_data.influence_node option
    * Public_data.influence_map)
    kasa_reply

  method get_initial_node :
    (Public_data.rule, Public_data.var) Public_data.influence_node option
    kasa_reply

  method get_next_node :
    (int, int) Public_data.influence_node option ->
    (Public_data.rule, Public_data.var) Public_data.influence_node option
    kasa_reply

  method get_previous_node :
    (int, int) Public_data.influence_node option ->
    (Public_data.rule, Public_data.var) Public_data.influence_node option
    kasa_reply

  method get_nodes_of_influence_map :
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level
    * (Public_data.rule, Public_data.var) Public_data.influence_node list)
    kasa_reply

  method get_dead_rules : Public_data.dead_rules kasa_reply
  method get_dead_agents : Public_data.dead_agents kasa_reply

  method get_non_weakly_reversible_transitions :
    Public_data.separating_transitions kasa_reply

  method get_constraints_list :
    (string * Public_data.agent list Public_data.lemma list) list kasa_reply

  method get_potential_polymers :
    Public_data.accuracy_level option ->
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level * Public_data.accuracy_level * Public_data.scc)
    kasa_reply
end

class type uniform_manager_static_analysis = object
  method init_static_analyser : Ast.parsing_compil -> unit result Lwt.t

  method init_static_analyser_raw : string -> unit result Lwt.t
  (** The string has to be the json corresponding to an [Ast.parsing_compil] *)

  method get_contact_map :
    Public_data.accuracy_level option -> Yojson.Basic.t result Lwt.t

  method secret_get_pos_of_rules_and_vars :
    Public_data.pos_of_rules_and_vars result Lwt.t

  method get_influence_map_raw :
    Public_data.accuracy_level option -> string result Lwt.t

  method get_local_influence_map :
    ?fwd:int ->
    ?bwd:int ->
    ?origin:(int, int) Public_data.influence_node ->
    total:int ->
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level
    * int
    * int option
    * int option
    * (Public_data.rule, Public_data.var) Public_data.influence_node option
    * Public_data.influence_map)
    result
    Lwt.t

  method get_initial_node :
    (Public_data.rule, Public_data.var) Public_data.influence_node option result
    Lwt.t

  method get_next_node :
    (int, int) Public_data.influence_node option ->
    (Public_data.rule, Public_data.var) Public_data.influence_node option result
    Lwt.t

  method get_previous_node :
    (int, int) Public_data.influence_node option ->
    (Public_data.rule, Public_data.var) Public_data.influence_node option result
    Lwt.t

  method get_nodes_of_influence_map :
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level
    * (Public_data.rule, Public_data.var) Public_data.influence_node list)
    result
    Lwt.t

  method get_dead_rules : Public_data.dead_rules result Lwt.t
  method get_dead_agents : Public_data.dead_agents result Lwt.t

  method get_non_weakly_reversible_transitions :
    Public_data.separating_transitions result Lwt.t

  method get_constraints_list :
    (string * Public_data.agent list Public_data.lemma list) list result Lwt.t

  method get_potential_polymers :
    Public_data.accuracy_level option ->
    Public_data.accuracy_level option ->
    (Public_data.accuracy_level * Public_data.accuracy_level * Public_data.scc)
    result
    Lwt.t
end

class type virtual manager_stories = object
  method virtual is_running : bool

  method config_story_computation :
    compression_modes -> (unit, string) Lwt_result.t

  method raw_launch_story_computation : string -> (unit, string) Lwt_result.t
  method story_log : string list
  method story_is_computing : bool
  method story_progress : Story_json.progress_bar option

  method story_list :
    (compression_modes
    * unit Trace.Simulation_info.t list list
    * Graph_loggers_sig.graph)
    Mods.IntMap.t
end

class type concrete_manager = object
  inherit manager_model
  inherit manager_simulation
  inherit uniform_manager_static_analysis
  inherit manager_stories

  method project_parse :
    patternSharing:Pattern.sharing_level ->
    (string * Nbr.t) list ->
    unit result Lwt.t

  method get_influence_map_node_at :
    filename:string ->
    Loc.position ->
    (int, int) Public_data.influence_node option result Lwt.t

  method is_running : bool
  method terminate : unit
  method is_computing : bool
end

class type rest_manager = object
  inherit manager_environment
  inherit concrete_manager
  method project_catalog : string list result Lwt.t
  method project_create : Api_types_t.project_parameter -> unit result Lwt.t
  method project_delete : project_id -> unit result Lwt.t
end
