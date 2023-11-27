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
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method file_create :
    int ->
    string ->
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method file_delete :
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method file_get :
    string ->
    ( string * int,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method file_move :
    int ->
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method file_update :
    string ->
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_constraints_list :
    ( (string
      * Kappa_kasa_type_interface.Public_data.agent list
        Kappa_kasa_type_interface.Public_data.lemma
        list)
      list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_contact_map :
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    ( Yojson.Basic.t,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_dead_agents :
    ( Kappa_kasa_type_interface.Public_data.dead_agents,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_dead_rules :
    ( Kappa_kasa_type_interface.Public_data.rule list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_influence_map_node_at :
    filename:string ->
    Kappa_generic_toolset.Locality.position ->
    ( (int, int) Kappa_kasa_type_interface.Public_data.influence_node option,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_influence_map_raw :
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    ( string,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_initial_node :
    ( ( Kappa_kasa_type_interface.Public_data.rule,
        Kappa_kasa_type_interface.Public_data.var )
      Kappa_kasa_type_interface.Public_data.influence_node
      option,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_local_influence_map :
    ?fwd:int ->
    ?bwd:int ->
    ?origin:(int, int) Kappa_kasa_type_interface.Public_data.influence_node ->
    total:int ->
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    ( Kappa_kasa_type_interface.Public_data.accuracy_level
      * int
      * int option
      * int option
      * ( Kappa_kasa_type_interface.Public_data.rule,
          Kappa_kasa_type_interface.Public_data.var )
        Kappa_kasa_type_interface.Public_data.influence_node
        option
      * Kappa_kasa_type_interface.Public_data.influence_map,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_next_node :
    (int, int) Kappa_kasa_type_interface.Public_data.influence_node option ->
    ( ( Kappa_kasa_type_interface.Public_data.rule,
        Kappa_kasa_type_interface.Public_data.var )
      Kappa_kasa_type_interface.Public_data.influence_node
      option,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_nodes_of_influence_map :
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    ( Kappa_kasa_type_interface.Public_data.accuracy_level
      * ( Kappa_kasa_type_interface.Public_data.rule,
          Kappa_kasa_type_interface.Public_data.var )
        Kappa_kasa_type_interface.Public_data.influence_node
        list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_non_weakly_reversible_transitions :
    ( (Kappa_kasa_type_interface.Public_data.rule * (string * string) list) list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_potential_polymers :
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    Kappa_kasa_type_interface.Public_data.accuracy_level option ->
    ( Kappa_kasa_type_interface.Public_data.accuracy_level
      * Kappa_kasa_type_interface.Public_data.accuracy_level
      * Kappa_kasa_type_interface.Public_data.scc,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method get_previous_node :
    (int, int) Kappa_kasa_type_interface.Public_data.influence_node option ->
    ( ( Kappa_kasa_type_interface.Public_data.rule,
        Kappa_kasa_type_interface.Public_data.var )
      Kappa_kasa_type_interface.Public_data.influence_node
      option,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method init_static_analyser :
    Kappa_grammar.Ast.parsing_compil ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method init_static_analyser_raw :
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method private message :
    'a.
    'a handle ->
    (Buffer.t -> unit) ->
    ( 'a,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method project_overwrite :
    string ->
    Kappa_grammar.Ast.parsing_compil ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method project_parse :
    patternSharing:Kappa_terms.Pattern.sharing_level ->
    (string * Kappa_generic_toolset.Nbr.t) list ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method secret_get_pos_of_rules_and_vars :
    Public_data.pos_of_rules_and_vars Api.result Lwt.t

  method secret_project_parse : Ast.parsing_compil Api.result Lwt.t

  method secret_simulation_load :
    Kappa_terms.Pattern.sharing_level ->
    Kappa_grammar.Ast.parsing_compil ->
    (string * Kappa_generic_toolset.Nbr.t) list ->
    unit Api.result Lwt.t

  method simulation_catalog_din :
    ( string list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_catalog_file_line :
    ( string list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_catalog_snapshot :
    ( string list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_continue :
    string ->
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_delete :
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_detail_din :
    string ->
    ( Kappa_runtime.Data.din,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_detail_file_line :
    string ->
    ( string list,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_detail_log_message :
    ( string,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_detail_plot :
    Api_types_j.plot_parameter ->
    ( Kappa_runtime.Data.plot,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_detail_snapshot :
    string ->
    ( Kappa_runtime.Data.snapshot,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_efficiency :
    ( Kappa_runtime.Counter.Efficiency.t,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_info :
    ( Api_types_t.simulation_info,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_intervention :
    string ->
    ( string,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_outputs_zip :
    ( Kappa_generic_toolset.Bigbuffer.bigstring,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_parameter :
    ( Api_types_t.simulation_parameter,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_pause :
    ( unit,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_raw_trace :
    ( string,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t

  method simulation_start :
    Api_types_j.simulation_parameter ->
    ( Api_types_t.simulation_artifact,
      Kappa_generic_toolset.Result_util.message list )
    Kappa_generic_toolset.Result_util.t
    Lwt.t
end

type mailbox = (int, box) Hashtbl.t

val new_mailbox : unit -> mailbox
val receive : mailbox -> string -> unit
val is_computing : mailbox -> bool
