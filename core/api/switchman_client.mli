type box
type 'a handle

class virtual new_client :
  is_running:(unit -> bool) ->
  post:(string -> unit) ->
  (int, box) Hashtbl.t ->
object
  val mutable id : int

  method file_catalog :
    ( Kappa_grammar.Kfiles.catalog_item list,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method file_create :
    int ->
    string ->
    string ->
    (unit, Result_util.message list) Result_util.t Lwt.t

  method file_delete :
    string -> (unit, Result_util.message list) Result_util.t Lwt.t

  method file_get :
    string -> (string * int, Result_util.message list) Result_util.t Lwt.t

  method file_move :
    int -> string -> (unit, Result_util.message list) Result_util.t Lwt.t

  method file_update :
    string -> string -> (unit, Result_util.message list) Result_util.t Lwt.t

  method get_constraints_list :
    ( (string * Public_data.agent list Public_data.lemma list) list,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_contact_map :
    Public_data.accuracy_level option ->
    (Yojson.Basic.t, Result_util.message list) Result_util.t Lwt.t

  method get_dead_agents :
    (Public_data.dead_agents, Result_util.message list) Result_util.t Lwt.t

  method get_dead_rules :
    (Public_data.rule list, Result_util.message list) Result_util.t Lwt.t

  method get_influence_map_node_at :
    filename:string ->
    Loc.position ->
    ( Public_data.refined_influence_node option,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_influence_map_raw :
    Public_data.accuracy_level option ->
    (string, Result_util.message list) Result_util.t Lwt.t

  method get_initial_node :
    ( Public_data.refined_influence_node option,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_local_influence_map :
    ?fwd:int ->
    ?bwd:int ->
    ?origin:Public_data.short_influence_node ->
    total:int ->
    Public_data.accuracy_level option ->
    ( Public_data.accuracy_level
      * int
      * int option
      * int option
      * Public_data.refined_influence_node option
      * Public_data.influence_map,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_next_node :
    Public_data.short_influence_node option ->
    ( Public_data.refined_influence_node option,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_nodes_of_influence_map :
    Public_data.accuracy_level option ->
    ( Public_data.accuracy_level * Public_data.refined_influence_node list,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_non_weakly_reversible_transitions :
    ( (Public_data.rule * (string * string) list) list,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_potential_polymers :
    Public_data.accuracy_level option ->
    Public_data.accuracy_level option ->
    ( Public_data.accuracy_level * Public_data.accuracy_level * Public_data.scc,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method get_previous_node :
    Public_data.short_influence_node option ->
    ( Public_data.refined_influence_node option,
      Result_util.message list )
    Result_util.t
    Lwt.t

  method init_static_analyser :
    Kappa_grammar.Ast.parsing_compil ->
    (unit, Result_util.message list) Result_util.t Lwt.t

  method init_static_analyser_raw :
    string -> (unit, Result_util.message list) Result_util.t Lwt.t

  method private message :
    'a.
    'a handle ->
    (Buffer.t -> unit) ->
    ('a, Result_util.message list) Result_util.t Lwt.t

  method project_overwrite :
    string ->
    Kappa_grammar.Ast.parsing_compil ->
    (unit, Result_util.message list) Result_util.t Lwt.t

  method project_parse :
    patternSharing:Kappa_terms.Pattern.sharing_level ->
    (string * Nbr.t) list ->
    (unit, Result_util.message list) Result_util.t Lwt.t

  method secret_get_pos_of_rules_and_vars :
    Public_data.pos_of_rules_and_vars Api.lwt_result

  method secret_project_parse : Ast.parsing_compil Api.lwt_result

  method secret_simulation_load :
    Kappa_terms.Pattern.sharing_level ->
    Kappa_grammar.Ast.parsing_compil ->
    (string * Nbr.t) list ->
    unit Api.lwt_result

  method simulation_catalog_din :
    (string list, Result_util.message list) Result_util.t Lwt.t

  method simulation_catalog_file_line :
    (string list, Result_util.message list) Result_util.t Lwt.t

  method simulation_catalog_snapshot :
    (string list, Result_util.message list) Result_util.t Lwt.t

  method simulation_continue :
    string -> (unit, Result_util.message list) Result_util.t Lwt.t

  method simulation_delete :
    (unit, Result_util.message list) Result_util.t Lwt.t

  method simulation_detail_din :
    string ->
    (Kappa_runtime.Data.din, Result_util.message list) Result_util.t Lwt.t

  method simulation_detail_file_line :
    string -> (string list, Result_util.message list) Result_util.t Lwt.t

  method simulation_detail_log_message :
    (string, Result_util.message list) Result_util.t Lwt.t

  method simulation_detail_plot :
    Api_types_j.plot_parameter ->
    (Kappa_runtime.Data.plot, Result_util.message list) Result_util.t Lwt.t

  method simulation_detail_snapshot :
    string ->
    (Kappa_runtime.Data.snapshot, Result_util.message list) Result_util.t Lwt.t

  method simulation_efficiency :
    (Kappa_runtime.Counter.Efficiency.t, Result_util.message list) Result_util.t
    Lwt.t

  method simulation_info :
    (Api_types_t.simulation_info, Result_util.message list) Result_util.t Lwt.t

  method simulation_intervention :
    string -> (string, Result_util.message list) Result_util.t Lwt.t

  method simulation_outputs_zip :
    (Bigbuffer.bigstring, Result_util.message list) Result_util.t Lwt.t

  method simulation_parameter :
    (Api_types_t.simulation_parameter, Result_util.message list) Result_util.t
    Lwt.t

  method simulation_pause : (unit, Result_util.message list) Result_util.t Lwt.t

  method simulation_raw_trace :
    (string, Result_util.message list) Result_util.t Lwt.t

  method simulation_start :
    Api_types_j.simulation_parameter ->
    (Api_types_t.simulation_artifact, Result_util.message list) Result_util.t
    Lwt.t
end

type mailbox = (int, box) Hashtbl.t

val new_mailbox : unit -> mailbox
val receive : mailbox -> string -> unit
val is_computing : mailbox -> bool
