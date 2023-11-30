(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Nov 05 2018>
*)

module type Interface = sig
  type ast
  type preprocessed_ast
  type compil
  type cache
  type mixture (* not necessarily connected, fully specified *)
  type chemical_species (* connected, fully specified *)
  type canonic_species (* chemical species in canonic form *)
  type pattern (* not necessarity connected, maybe partially specified *)
  type connected_component (* connected, maybe partially specified *)
  type rule
  type init = ((connected_component array list, int) Alg_expr.e * rule) list

  val debug_mode : compil -> bool
  val do_we_allow_empty_lhs : compil -> bool
  val to_dotnet : compil -> compil
  val with_dot_and_plus : compil -> compil
  val dont_allow_empty_lhs : compil -> compil
  val empty_cache : compil -> cache
  val get_init : compil -> init
  val mixture_of_init : compil -> rule (*hidden_init*) -> mixture
  val dummy_chemical_species : compil -> chemical_species

  val compare_connected_component :
    connected_component -> connected_component -> int

  val print_connected_component :
    ?compil:compil -> Format.formatter -> connected_component -> unit

  val print_token : ?compil:compil -> Format.formatter -> int -> unit

  val print_chemical_species :
    ?compil:compil -> Format.formatter -> chemical_species -> unit

  val print_canonic_species :
    ?compil:compil -> Format.formatter -> canonic_species -> unit

  val rule_rate_convention : compil -> Remanent_parameters_sig.rate_convention

  val reaction_rate_convention :
    compil -> Remanent_parameters_sig.rate_convention option

  val what_do_we_count : compil -> Ode_args.count
  val do_we_count_in_embeddings : compil -> bool
  val internal_meaning_is_nembeddings : compil -> bool
  val do_we_prompt_reactions : compil -> bool
  val symbol_table : compil -> Symbol_table.symbol_table

  val nbr_automorphisms_in_chemical_species :
    debug_mode:bool -> chemical_species -> int

  val canonic_form : chemical_species -> canonic_species
  val connected_components_of_patterns : pattern -> connected_component list

  val connected_components_of_mixture :
    compil -> cache -> mixture -> cache * chemical_species list

  type embedding (* the domain is connected *)
  type embedding_forest (* the domain may be not connected *)

  val lift_embedding : embedding -> embedding_forest

  val find_embeddings :
    compil -> connected_component -> chemical_species -> embedding list

  val find_all_embeddings :
    compil -> chemical_species -> (connected_component * embedding) list

  (*val find_embeddings_unary_binary:
    compil -> pattern -> chemical_species -> embedding_forest list * mixture*)

  val compose_embeddings_unary_binary :
    compil ->
    pattern ->
    embedding list list ->
    chemical_species ->
    embedding_forest list * mixture

  module type ObsMap = sig
    type 'a t

    val empty : 'a -> 'a t
    val add : connected_component -> 'a -> 'a list t -> 'a list t
    val get : connected_component -> 'a list t -> 'a list
    val reset : connected_component -> 'a list t -> 'a list t
  end

  module ObsMap : ObsMap

  val disjoint_union :
    compil ->
    (connected_component * embedding * chemical_species) list ->
    pattern * embedding_forest * mixture

  (*type rule*)

  type rule_name = string
  type rule_id = int
  type rule_id_with_mode = rule_id * Rule_modes.arity * Rule_modes.direction

  val valid_modes : cache -> compil -> rule -> cache * rule_id_with_mode list
  val lhs : compil -> rule_id_with_mode -> rule -> pattern

  val token_vector :
    rule ->
    ((connected_component array list, int) Alg_expr.e Loc.annoted * int) list

  val token_vector_of_init :
    rule ->
    ((connected_component array list, int) Alg_expr.e Loc.annoted * int) list

  val print_rule_id : Format.formatter -> rule_id -> unit
  val print_rule : ?compil:compil -> Format.formatter -> rule -> unit
  val print_rule_name : ?compil:compil -> Format.formatter -> rule -> unit

  val string_of_var_id :
    ?compil:compil -> ?init_mode:bool -> Loggers.t -> rule_id -> string

  val string_of_var_id_jac : ?compil:compil -> rule_id -> rule_id -> string

  (*  module SyntacticRuleSetMap:SetMap.SetMap*)

  val rate :
    compil ->
    rule ->
    rule_id_with_mode ->
    (connected_component array list, int) Alg_expr.e Loc.annoted option

  val rate_name : compil -> rule -> rule_id_with_mode -> rule_name
  val apply : compil -> rule -> embedding_forest -> mixture -> mixture
  val get_preprocessed_ast : Run_cli_args.t -> preprocessed_ast
  val to_preprocessed_ast : preprocessed_ast -> Cli_init.preprocessed_ast
  val get_ast : Run_cli_args.t -> ast
  val to_ast : ast -> Ast.parsing_compil
  val preprocess : Run_cli_args.t -> ast -> preprocessed_ast

  val get_compil :
    debug_mode:bool ->
    dotnet:bool ->
    ?bwd_bisim:LKappa_group_action.bwd_bisim_info ->
    rule_rate_convention:Remanent_parameters_sig.rate_convention ->
    ?reaction_rate_convention:Remanent_parameters_sig.rate_convention ->
    show_reactions:bool ->
    count:Ode_args.count ->
    internal_meaning:Ode_args.count ->
    compute_jacobian:bool ->
    Run_cli_args.t ->
    preprocessed_ast ->
    compil

  val get_rules : compil -> rule list

  val get_variables :
    compil ->
    (string * (connected_component array list, int) Alg_expr.e Loc.annoted)
    array

  val get_obs : compil -> (connected_component array list, int) Alg_expr.e list
  val get_obs_titles : compil -> string list
  val nb_tokens : compil -> int

  (*symmetries for initial states*)

  val divide_rule_rate_by : cache -> compil -> rule -> cache * int

  val species_of_initial_state_env :
    debug_mode:bool ->
    Model.t ->
    Contact_map.t ->
    Pattern.PreEnv.t ->
    ('b * Primitives.elementary_rule) list ->
    Pattern.PreEnv.t * Pattern.cc list

  val species_of_initial_state :
    compil ->
    cache ->
    ('b * Primitives.elementary_rule) list ->
    cache * Pattern.cc list

  val detect_symmetries :
    Remanent_parameters_sig.parameters ->
    compil ->
    cache ->
    chemical_species list ->
    Public_data.contact_map ->
    cache * Symmetries.symmetries

  val print_symmetries :
    Remanent_parameters_sig.parameters ->
    compil ->
    Symmetries.symmetries ->
    unit

  val get_rule_cache : cache -> LKappa_auto.cache
  val set_rule_cache : LKappa_auto.cache -> cache -> cache

  val get_representative :
    Remanent_parameters_sig.parameters ->
    compil ->
    cache ->
    Symmetries.reduction ->
    chemical_species ->
    cache * chemical_species

  val equiv_class_of_pattern :
    Remanent_parameters_sig.parameters ->
    compil ->
    cache ->
    Symmetries.reduction ->
    connected_component ->
    cache * (int * (connected_component * int) list)

  val valid_mixture :
    compil -> cache -> ?max_size:int -> mixture -> cache * bool

  val init_bwd_bisim_info :
    Symmetries.equivalence_classes -> LKappa_group_action.bwd_bisim_info
end
