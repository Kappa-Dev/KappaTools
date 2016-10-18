module type Site_graph =
sig
  type t
  type agent_id
  type bond_index

  type binding_state =
    | Free | Wildcard | Bound_to_unknown
    | Binding_type of Ckappa_sig.agent_name * Ckappa_sig.site_name
    | Bound_to of bond_index

(*TODO*)
  type internal_list = (Ckappa_sig.agent_name *
                        Wrapped_modules.LoggedStringMap.elt *
                        Ckappa_sig.internal_state) list

  type bound_to_list =
    (Ckappa_sig.agent_name * Wrapped_modules.LoggedStringMap.elt * bond_index)
      list

  type binding_list = (Ckappa_sig.agent_name *
                       Wrapped_modules.LoggedStringMap.elt *
                       (Ckappa_sig.agent_name * Ckappa_sig.site_name)) list

  type triple_pair_list =
    internal_list * bound_to_list * binding_list


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

  val print_store_views:
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t -> Exception.method_handler * triple_pair_list

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
