module type Site_graph =
sig
  type t
  type agent_id
  type bond_index


  val empty: t
  val add_agent:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler  ->
    Cckappa_sig.kappa_handler ->
    Ckappa_sig.c_agent_name -> t ->
    Exception.method_handler * agent_id * t

  val add_state:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_state -> t ->
    Exception.method_handler * t

  val add_bound_to_unknown:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name -> t ->
    Exception.method_handler * t

  val add_bond:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_bond_type:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val print:
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t -> Exception.method_handler

    val print_list:
      Loggers.t ->
      Remanent_parameters_sig.parameters ->
      Exception.method_handler ->
      Cckappa_sig.kappa_handler ->
      t list -> Exception.method_handler

  val to_json:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t -> Exception.method_handler * Yojson.Basic.json
end
