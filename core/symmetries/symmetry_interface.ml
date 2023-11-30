(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jan 07 2020>
*)

type rule = Primitives.elementary_rule
type preprocessed_ast = Cli_init.preprocessed_ast
type ast = Ast.parsing_compil
type init = (Primitives.alg_expr * rule) list

type compil = {
  debug_mode: bool;
  contact_map: Contact_map.t;
  environment: Model.t;
  init: init;
  rule_rate_convention: Remanent_parameters_sig.rate_convention;
  reaction_rate_convention: Remanent_parameters_sig.rate_convention option;
  show_reactions: bool;
  count: Ode_args.count;
  internal_meaning: Ode_args.count;
  compute_jacobian: bool;
  symbol_table: Symbol_table.symbol_table;
  allow_empty_lhs: bool;
}

let debug_mode compil = compil.debug_mode
let do_we_allow_empty_lhs compil = compil.allow_empty_lhs

let to_dotnet compil =
  { compil with symbol_table = Symbol_table.unbreakable_symbol_table_dotnet }

let with_dot_and_plus compil =
  {
    compil with
    symbol_table = Symbol_table.with_dot_and_plus compil.symbol_table;
  }

let dont_allow_empty_lhs compil = { compil with allow_empty_lhs = false }

type cc_cache = Pattern.PreEnv.t
type nauto_in_rules_cache = LKappa_auto.cache
type sym_cache = Symmetries.cache
type seen_cache = bool Mods.DynArray.t
type rule_weight = int option Mods.DynArray.t

type cache = {
  cc_cache: cc_cache;
  rule_cache: nauto_in_rules_cache;
  representative_cache: sym_cache;
  seen_rule: seen_cache;
  seen_pattern: seen_cache;
  seen_species: seen_cache;
  correcting_coef: rule_weight;
  n_cc: rule_weight;
}

let get_cc_cache cache = cache.cc_cache
let set_cc_cache cc_cache cache = { cache with cc_cache }
let get_rule_cache cache = cache.rule_cache
let set_rule_cache rule_cache cache = { cache with rule_cache }
let get_sym_cache cache = cache.representative_cache

let set_sym_cache sym_cache cache =
  { cache with representative_cache = sym_cache }

let hash_rule_weight get set f cache compil rule =
  match get cache rule with
  | Some n -> cache, n
  | None ->
    let cache, n = f cache compil rule in
    let cache = set cache rule n in
    cache, n

let get_representative parameters compil cache symmetries species =
  let sigs = Model.signatures compil.environment in
  let rep_cache, rule_cache, cc_cache, species =
    Symmetries.representative ~parameters ~sigs (get_sym_cache cache)
      (get_rule_cache cache) (get_cc_cache cache) symmetries species
  in
  let cache =
    set_rule_cache rule_cache
      (set_cc_cache cc_cache (set_sym_cache rep_cache cache))
  in
  cache, species

let equiv_class_of_pattern parameters compil cache symmetries pattern =
  let env = compil.environment in
  let rep_cache, rule_cache, cc_cache, seen_pattern, equiv_class =
    Symmetries.equiv_class ~parameters env cache.seen_pattern
      (get_sym_cache cache) (get_rule_cache cache) (get_cc_cache cache)
      symmetries pattern
  in
  let cache =
    set_rule_cache rule_cache
      (set_cc_cache cc_cache (set_sym_cache rep_cache cache))
  in
  { cache with seen_pattern }, equiv_class

let get_init compil = compil.init

let lift_opt f compil_opt =
  match compil_opt with
  | None -> None
  | Some a -> Some (f a)

let contact_map compil = compil.contact_map
let environment compil = compil.environment
let domain compil = Model.domain (environment compil)
let domain_opt = lift_opt domain
let environment_opt = lift_opt environment
let symbol_table compil = compil.symbol_table

let symbol_table_opt a =
  match a with
  | None -> Symbol_table.symbol_table_V4
  | Some compil -> symbol_table compil

type mixture = Edges.t (* not necessarily connected, fully specified *)
type chemical_species = Pattern.cc
(* connected, fully specified *)

type canonic_species = chemical_species (* chemical species in canonic form *)
type pattern = Pattern.id array
(* not necessarity connected, maybe partially specified *)

type connected_component = Pattern.id
(* connected, maybe partially specified *)

let dummy_chemical_species compil =
  Pattern.empty_cc (Pattern.Env.signatures (domain compil))

let rule_rate_convention compil = compil.rule_rate_convention
let reaction_rate_convention compil = compil.reaction_rate_convention
let what_do_we_count compil = compil.count

let do_we_count_in_embeddings compil =
  match what_do_we_count compil with
  | Ode_args.Occurrences -> false
  | Ode_args.Embeddings -> true

let internal_meaning_is_nembeddings compil =
  match compil.internal_meaning with
  | Ode_args.Occurrences -> false
  | Ode_args.Embeddings -> true

let do_we_prompt_reactions compil = compil.show_reactions

let print_chemical_species ?compil f =
  Format.fprintf f "@[<h>%a@]"
    (Kade_backend.Pattern.print_cc
       ~noCounters:
         (match compil with
         | None -> false
         | Some c -> c.debug_mode)
       ~full_species:true
       ?sigs:(Option_util.map Model.signatures (environment_opt compil))
       ?cc_id:None ~symbol_table:(symbol_table_opt compil) ~with_id:false)

let print_token ?compil fmt k =
  Format.fprintf fmt "%a" (Model.print_token ?env:(environment_opt compil)) k

let print_canonic_species = print_chemical_species

let nbr_automorphisms_in_chemical_species ~debug_mode x =
  List.length (Pattern.automorphisms ~debug_mode x)

let compare_connected_component = Pattern.compare_canonicals

let print_connected_component ?compil =
  Kade_backend.Pattern.print
    ~noCounters:
      (match compil with
      | None -> false
      | Some c -> c.debug_mode)
    ?domain:(domain_opt compil) ~symbol_table:(symbol_table_opt compil)
    ~with_id:false

let canonic_form x = x
let connected_components_of_patterns = Array.to_list

let connected_components_of_mixture_sigs ~debug_mode sigs cache contact_map_int
    e =
  let cache, acc =
    Pattern_decompiler.patterns_of_mixture ~debug_mode contact_map_int sigs
      cache e
  in
  cache, acc

let connected_components_of_mixture compil cache e =
  let cc_cache = get_cc_cache cache in
  let contact_map = contact_map compil in
  let sigs = Pattern.Env.signatures (domain compil) in
  let cc_cache, acc =
    Pattern_decompiler.patterns_of_mixture ~debug_mode:compil.debug_mode
      contact_map sigs cc_cache e
  in
  set_cc_cache cc_cache cache, acc

type embedding = Renaming.t (* the domain is connected *)
type embedding_forest = Matching.t
(* the domain may be not connected *)

let lift_embedding x =
  Option_util.unsome Matching.empty (Matching.add_cc Matching.empty 0 x)

let find_all_embeddings compil cc =
  let tr = Primitives.fully_specified_pattern_to_positive_transformations cc in
  let env = environment compil in
  let l = Evaluator.find_all_embeddings ~debug_mode:true env tr in
  Tools.remove_double_elements l

let add_fully_specified_to_graph ~debug_mode sigs graph cc =
  let e, g =
    Pattern.fold_by_type
      (fun ~pos ~agent_type intf (emb, g) ->
        let a, g' = Edges.add_agent sigs agent_type g in
        let ag = a, agent_type in
        let emb' = Mods.IntMap.add pos ag emb in
        ( emb',
          Tools.array_fold_lefti
            (fun site acc (l, i) ->
              let acc' =
                if i <> -1 then
                  Edges.add_internal a site i acc
                else
                  acc
              in
              match l with
              | Pattern.UnSpec | Pattern.Free -> Edges.add_free a site acc'
              | Pattern.Link (x', s') ->
                (match Mods.IntMap.find_option x' emb' with
                | None -> acc'
                | Some ag' -> fst @@ Edges.add_link ag site ag' s' acc'))
            g' intf ))
      cc (Mods.IntMap.empty, graph)
  in
  let r = Renaming.empty () in
  let out =
    Mods.IntMap.fold
      (fun i (a, _) acc -> acc && Renaming.imperative_add ~debug_mode i a r)
      e true
  in
  let () = assert out in
  g, r

let find_embeddings compil =
  Pattern.embeddings_to_fully_specified ~debug_mode:compil.debug_mode
    (domain compil)

let f ~debug_mode ren acc (i, _cc) em =
  List_util.map_flatten
    (fun m ->
      List_util.map_option
        (fun r -> Matching.add_cc m i (Renaming.compose ~debug_mode true r ren))
        em)
    acc

(*let find_embeddings_unary_binary compil p x =
  let mix,ren =
    add_fully_specified_to_graph
      (Model.signatures compil.environment)
      (Edges.empty ~with_connected_components:false) x in
  let matc =
    Tools.array_fold_lefti
      (fun i acc cc ->
         let em = find_embeddings compil cc x in
         f ren acc (i,cc) em)
      [Matching.empty]
      p in
  (matc,mix)*)

let compose_embeddings_unary_binary compil p emb_list x =
  let mix, ren =
    add_fully_specified_to_graph ~debug_mode:compil.debug_mode
      (Model.signatures compil.environment)
      (Edges.empty ~with_connected_components:false)
      x
  in
  let cc_list = Tools.array_fold_lefti (fun i acc cc -> (i, cc) :: acc) [] p in
  let matc =
    List.fold_left2
      (f ~debug_mode:compil.debug_mode ren)
      [ Matching.empty ] cc_list emb_list
  in
  matc, mix

let disjoint_union_sigs ~debug_mode sigs l =
  let pat = Tools.array_map_of_list (fun (x, _, _) -> x) l in
  let _, em, mix =
    List.fold_left
      (fun (i, em, mix) (_, r, cc) ->
        let i = pred i in
        let mix', r' = add_fully_specified_to_graph ~debug_mode sigs mix cc in
        let r'' = Renaming.compose ~debug_mode false r r' in
        i, Option_util.unsome Matching.empty (Matching.add_cc em i r''), mix')
      ( List.length l,
        Matching.empty,
        Edges.empty ~with_connected_components:false )
      l
  in
  pat, em, mix

let disjoint_union compil l =
  let sigs = Model.signatures compil.environment in
  disjoint_union_sigs ~debug_mode:compil.debug_mode sigs l

type rule_id = int
type arity = Rule_modes.arity
type direction = Rule_modes.direction
type rule_name = string
type rule_id_with_mode = rule_id * arity * direction

let lhs _compil _rule_id r = r.Primitives.connected_components

let is_zero expr =
  match expr with
  | Alg_expr.CONST a, _ -> Nbr.is_zero a
  | ( ( Alg_expr.BIN_ALG_OP (_, _, _)
      | Alg_expr.UN_ALG_OP (_, _)
      | Alg_expr.IF (_, _, _)
      | Alg_expr.DIFF_TOKEN _ | Alg_expr.DIFF_KAPPA_INSTANCE _
      | Alg_expr.STATE_ALG_OP _ | Alg_expr.ALG_VAR _ | Alg_expr.KAPPA_INSTANCE _
      | Alg_expr.TOKEN_ID _ ),
      _ ) ->
    false

let add_not_none_not_zero x y list =
  match y with
  | None -> list
  | Some x when is_zero (fst x) -> list
  | Some _ -> x :: list

let add_not_zero x y list =
  if is_zero y then
    list
  else
    x :: list

let mode_of_rule _compil _rule = Rule_modes.Direct

let n_cc cache compil rule =
  let id = rule.Primitives.syntactic_rule in
  let rule_cache = get_rule_cache cache in
  let lkappa_rule = Model.get_ast_rule compil.environment id in
  let rule_cache, arity = LKappa_auto.n_cc rule_cache lkappa_rule in
  let cache = set_rule_cache rule_cache cache in
  cache, arity

let n_cc cache compil rule =
  hash_rule_weight
    (fun cache rule ->
      let id = rule.Primitives.syntactic_rule in
      Mods.DynArray.get cache.n_cc id)
    (fun cache rule n ->
      let id = rule.Primitives.syntactic_rule in
      let () = Mods.DynArray.set cache.n_cc id (Some n) in
      cache)
    n_cc cache compil rule

let valid_modes cache compil rule =
  let id = rule.Primitives.syntactic_rule in
  let cache, arity = n_cc cache compil rule in
  let arity' = Array.length rule.Primitives.connected_components in
  let mode = mode_of_rule compil rule in
  let () = assert (arity' <= arity) in
  let mode_list =
    if arity = arity' then
      add_not_zero Rule_modes.Usual rule.Primitives.rate
        (add_not_none_not_zero Rule_modes.Unary rule.Primitives.unary_rate [])
    else if arity' = 1 && arity = 2 then (
      let lkappa_rule = Model.get_ast_rule compil.environment id in
      add_not_none_not_zero Rule_modes.Unary_refinement
        lkappa_rule.LKappa.r_un_rate []
    ) else
      []
  in
  cache, List.rev_map (fun x -> id, x, mode) (List.rev mode_list)

let rate _compil rule (_, arity, _) =
  match arity with
  | Rule_modes.Usual -> Some rule.Primitives.rate
  | Rule_modes.Unary | Rule_modes.Unary_refinement ->
    Option_util.map fst rule.Primitives.unary_rate

let token_vector a = a.Primitives.delta_tokens
let token_vector_of_init = token_vector
let print_rule_id log = Format.fprintf log "%i"

let print_rule ?compil =
  match compil with
  | None ->
    Kade_backend.Kappa_printer.elementary_rule ~noCounters:true ?env:None
      ?symbol_table:None
  | Some compil ->
    Kade_backend.Kappa_printer.decompiled_rule ~noCounters:compil.debug_mode
      ~full:true (environment compil) ~symbol_table:(symbol_table compil)

let print_rule_name ?compil f r =
  let env = environment_opt compil in
  let id = r.Primitives.syntactic_rule in
  Kade_backend.Model.print_ast_rule
    ~noCounters:
      (match compil with
      | None -> false
      | Some c -> c.debug_mode)
    ?env ~symbol_table:(symbol_table_opt compil) f id

let string_of_var_id ?compil ?init_mode logger r =
  let f logger r =
    match Loggers.get_encoding_format logger with
    | Loggers.Mathematica | Loggers.Maple ->
      "var" ^ string_of_int r
      ^
      (match init_mode with
      | Some true -> ""
      | Some _ | None -> "(t)")
    | Loggers.Octave | Loggers.Matlab -> "var(" ^ string_of_int r ^ ")"
    | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS | Loggers.SBML
    | Loggers.DOTNET | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
    | Loggers.Js_Graph | Loggers.HTML_Tabular | Loggers.GEPHI | Loggers.Json
    | Loggers.Matrix ->
      ""
  in
  let env = environment_opt compil in
  match env with
  | None -> f logger r
  | Some env ->
    (try
       let array = Model.get_algs env in
       fst array.(r - 1)
     with _ -> f logger r)

let string_of_var_id_jac ?compil r dt =
  let _ = compil in
  "jacvar(" ^ string_of_int r ^ "," ^ string_of_int dt ^ ")"

let rate_name compil rule rule_id =
  let _kade_id, arity, direction = rule_id in
  let arity_tag =
    match arity with
    | Rule_modes.Usual -> ""
    | Rule_modes.Unary | Rule_modes.Unary_refinement -> " (unary context)"
  in
  let direction_tag =
    match direction with
    | Rule_modes.Direct -> ""
    | Rule_modes.Op -> " (op)"
  in
  Format.asprintf "%a%s%s" (print_rule_name ~compil) rule arity_tag
    direction_tag

let apply_sigs ~debug_mode env rule inj_nodes mix =
  let concrete_removed =
    List.map
      (Primitives.Transformation.concretize ~debug_mode
         (inj_nodes, Mods.IntMap.empty))
      rule.Primitives.removed
  in
  let dummy_instances = Instances.empty env in
  let side_effects, edges_after_neg =
    List.fold_left
      (Rule_interpreter.apply_negative_transformation
         ?mod_connectivity_store:None dummy_instances)
      ([], mix) concrete_removed
  in
  let (_, remaining_side_effects, edges'), concrete_inserted =
    List.fold_left
      (fun (x, p) h ->
        let x', h' =
          Rule_interpreter.apply_positive_transformation ~debug_mode
            (Model.signatures env) dummy_instances x h
        in
        x', h' :: p)
      (((inj_nodes, Mods.IntMap.empty), side_effects, edges_after_neg), [])
      rule.Primitives.inserted
  in
  let edges'', _ =
    List.fold_left
      (fun (e, i) (((id, _) as nc), s) ->
        Edges.add_free id s e, Primitives.Transformation.Freed (nc, s) :: i)
      (edges', concrete_inserted)
      remaining_side_effects
  in
  edges''

let apply compil rule inj_nodes mix =
  apply_sigs ~debug_mode:compil.debug_mode compil.environment rule inj_nodes mix

let get_rules compil =
  Model.fold_rules (fun _ acc r -> r :: acc) [] (environment compil)

let get_variables compil = Model.get_algs (environment compil)

let get_obs compil =
  Array.to_list (Model.map_observables (fun r -> r) (environment compil))

let remove_escape_char =
  (* I do not know anything about it be single quote are not allowed
     in Octave, please correct this function if you are more
     knowledgeable *)
  String.map (function
    | '\'' -> '|'
    | x -> x)

let get_obs_titles compil =
  let env = environment compil in
  Array.to_list
  @@ Model.map_observables
       (fun x ->
         remove_escape_char
           (Format.asprintf "%a"
              (Kade_backend.Kappa_printer.alg_expr ~noCounters:compil.debug_mode
                 ~env ~symbol_table:(symbol_table compil))
              x))
       env

let get_preprocessed_ast cli_args =
  let warning ~pos msg = Data.print_warning ~pos Format.err_formatter msg in
  Cli_init.get_preprocessed_ast_from_cli_args ~warning ~debug_mode:false
    cli_args

let to_preprocessed_ast x = x
let get_ast cli_args = Cli_init.get_ast_from_cli_args cli_args
let to_ast x = x

let preprocess cli_args ast =
  let warning ~pos msg = Data.print_warning ~pos Format.err_formatter msg in
  Cli_init.preprocess_ast ~warning ~debug_mode:false cli_args ast

let saturate_domain_with_symmetric_patterns ~debug_mode bwd_bisim_info env =
  let contact_map = Model.contact_map env in
  let preenv' =
    Model.fold_mixture_in_expr
      (fun domain ccs ->
        LKappa_group_action.saturate_domain_with_symmetric_patterns ~debug_mode
          ~compile_mode_on:false env bwd_bisim_info ccs domain)
      (Pattern.PreEnv.of_env (Model.domain env))
      env
  in
  let domain, _ =
    Pattern.finalize ~debug_mode ~sharing:Pattern.No_sharing preenv' contact_map
  in
  Model.new_domain domain env

let get_compil ~debug_mode ~dotnet ?bwd_bisim ~rule_rate_convention
    ?reaction_rate_convention ~show_reactions ~count ~internal_meaning
    ~compute_jacobian cli_args preprocessed_ast =
  let warning ~pos msg = Data.print_warning ~pos Format.err_formatter msg in
  let compilation_result : Cli_init.compilation_result =
    Cli_init.get_compilation_from_preprocessed_ast ~warning cli_args
      preprocessed_ast
  in
  let env =
    match bwd_bisim with
    | None -> compilation_result.env
    | Some bsi ->
      saturate_domain_with_symmetric_patterns ~debug_mode bsi
        compilation_result.env
  in
  let compil =
    {
      debug_mode;
      environment = env;
      contact_map = compilation_result.contact_map;
      init = compilation_result.init_l;
      rule_rate_convention;
      reaction_rate_convention;
      show_reactions;
      count;
      internal_meaning;
      compute_jacobian;
      allow_empty_lhs = true;
      symbol_table =
        (match cli_args.Run_cli_args.syntaxVersion with
        | Ast.V3 -> Symbol_table.unbreakable_symbol_table_V3
        | Ast.V4 -> Symbol_table.unbreakable_symbol_table_V4);
    }
  in
  if dotnet then
    to_dotnet (dont_allow_empty_lhs compil)
  else
    compil

let empty_cc_cache compil =
  Pattern.PreEnv.of_env (Model.domain compil.environment)

let empty_lkappa_cache () = LKappa_auto.init_cache ()
let empty_sym_cache () = Symmetries.empty_cache ()
let empty_seen_cache () = Mods.DynArray.create 1 false
let empty_rule_weight () = Mods.DynArray.create 1 None

let empty_cache compil =
  {
    cc_cache = empty_cc_cache compil;
    rule_cache = empty_lkappa_cache ();
    representative_cache = empty_sym_cache ();
    seen_pattern = empty_seen_cache ();
    seen_rule = empty_seen_cache ();
    seen_species = empty_seen_cache ();
    n_cc = empty_rule_weight ();
    correcting_coef = empty_rule_weight ();
  }

let mixture_of_init compil c =
  let _, emb, m = disjoint_union compil [] in
  let m = apply compil c emb m in
  m

let mixture_of_init_sigs ~debug_mode env c =
  let _, emb, m = disjoint_union_sigs ~debug_mode (Model.signatures env) [] in
  let m = apply_sigs ~debug_mode env c emb m in
  m

let species_of_initial_state_env ~debug_mode env contact_map_int cache list =
  let sigs = Model.signatures env in
  let cache, list =
    List.fold_left
      (fun (cache, list) (_, r) ->
        let b = mixture_of_init_sigs ~debug_mode env r in
        let cache', acc =
          connected_components_of_mixture_sigs ~debug_mode sigs cache
            contact_map_int b
        in
        cache', List.rev_append acc list)
      (cache, []) list
  in
  cache, list

let species_of_initial_state compil cache list =
  let cc_cache, list =
    species_of_initial_state_env ~debug_mode:compil.debug_mode
      compil.environment (contact_map compil) (get_cc_cache cache) list
  in
  set_cc_cache cc_cache cache, list

let nb_tokens compil = Model.nb_tokens (environment compil)

let divide_rule_rate_by cache compil rule =
  match compil.rule_rate_convention with
  | Remanent_parameters_sig.Common -> assert false
  (* this is not a valid parameterization *)
  (* Common can be used only to compute normal forms *)
  | Remanent_parameters_sig.No_correction -> cache, 1
  | Remanent_parameters_sig.Biochemist
  | Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs ->
    let rule_id = rule.Primitives.syntactic_rule in
    let lkappa_rule = Model.get_ast_rule compil.environment rule_id in
    let rule_cache, output =
      LKappa_auto.nauto compil.rule_rate_convention (get_rule_cache cache)
        lkappa_rule
    in
    set_rule_cache rule_cache cache, output

let divide_rule_rate_by cache compil rule =
  hash_rule_weight
    (fun cache rule ->
      let rule_id = rule.Primitives.syntactic_rule in
      Mods.DynArray.get cache.correcting_coef rule_id)
    (fun cache rule n ->
      let rule_id = rule.Primitives.syntactic_rule in
      let () = Mods.DynArray.set cache.correcting_coef rule_id (Some n) in
      cache)
    divide_rule_rate_by cache compil rule

let detect_symmetries parameters compil cache chemical_species contact_map =
  let rule_cache = get_rule_cache cache in
  let rule_cache, symmetries =
    Symmetries.detect_symmetries parameters compil.environment rule_cache
      compil.rule_rate_convention chemical_species (get_rules compil)
      contact_map
  in
  set_rule_cache rule_cache cache, symmetries

let print_symmetries parameters compil symmetries =
  let env = compil.environment in
  Symmetries.print_symmetries parameters env symmetries

let valid_mixture compil cc_cache ?max_size mixture =
  match max_size with
  | None -> cc_cache, true
  | Some n ->
    let cc_cache, cc_list =
      connected_components_of_mixture compil cc_cache mixture
    in
    cc_cache, List.for_all (fun cc -> Pattern.size_of_cc cc <= n) cc_list

let init_bwd_bisim_info red =
  red, Mods.DynArray.create 1 false, ref (LKappa_auto.init_cache ())

module type ObsMap = sig
  type 'a t

  val empty : 'a -> 'a t
  val add : connected_component -> 'a -> 'a list t -> 'a list t
  val get : connected_component -> 'a list t -> 'a list
  val reset : connected_component -> 'a list t -> 'a list t
end

module ObsMap : ObsMap = struct
  type 'a t = 'a Pattern.ObsMap.t

  let empty a = Pattern.ObsMap.dummy a
  let get cc map = Pattern.ObsMap.get map cc

  let add cc data map =
    let old = get cc map in
    let () = Pattern.ObsMap.set map cc (data :: old) in
    map

  let reset cc map =
    let () = Pattern.ObsMap.set map cc [] in
    map
end
