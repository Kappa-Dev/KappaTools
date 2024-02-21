module type Site_graph = sig
  type t
  type agent_id
  type bond_index

  type binding_state =
    | Free
    | Wildcard
    | Bound_to_unknown
    | Binding_type of string * string
    | Bound_to of bond_index

  val binding_state_to_json : binding_state -> Yojson.Basic.t
  val binding_state_of_json : Yojson.Basic.t -> binding_state
  val int_of_bond_index : bond_index -> int
  val bond_index_of_int : int -> bond_index
  val empty : t

  val get_string_version :
    t ->
    (string
    * (string option * binding_state option * (int option * int option) option)
      Wrapped_modules.LoggedStringMap.t)
    Ckappa_sig.Agent_id_map_and_set.Map.t

  val set_string_version :
    (*FIXME*)
    (string
    * (string option * binding_state option * (int option * int option) option)
      Wrapped_modules.LoggedStringMap.t)
    Ckappa_sig.Agent_id_map_and_set.Map.t ->
    t ->
    t

  val add_agent :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    Ckappa_sig.c_agent_name ->
    t ->
    Exception.method_handler * agent_id * t

  val add_site :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_state :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_state ->
    t ->
    Exception.method_handler * t

  val add_bound_to_unknown :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_bond :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_bond_type :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_counter_range :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    ?inf:int ->
    ?sup:int ->
    t ->
    Exception.method_handler * t

  val to_string :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    Exception.method_handler * string

  val print :
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    Exception.method_handler

  val print_agent :
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    string ->
    (string option * binding_state option * (int option * int option) option)
    Wrapped_modules.LoggedStringMap.t ->
    bool ->
    Exception.method_handler

  val print_list :
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t list ->
    Exception.method_handler

  val has_a_counter :
    Remanent_parameters_sig.parameters ->
    Exception_without_parameter.method_handler ->
    Cckappa_sig.kappa_handler ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    Exception_without_parameter.method_handler * bool

  val has_a_binding_state :
    Remanent_parameters_sig.parameters ->
    Exception_without_parameter.method_handler ->
    Cckappa_sig.kappa_handler ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    Exception_without_parameter.method_handler * bool
end
