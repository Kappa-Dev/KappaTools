(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Jan 08 2020>
*)

let local_trace = false

let debug ~debug_mode s =
  if local_trace || debug_mode then
    Format.kfprintf
      (fun f -> Format.pp_print_break f 0 0)
      Format.err_formatter s
  else
    Format.ifprintf Format.err_formatter s

module Make (I : Symmetry_interface_sig.Interface) = struct
  type connected_component_id = int

  let fst_cc_id = 1
  let next_cc_id = succ

  module Store = SetMap.Make (struct
    type t =
      I.rule_id_with_mode * connected_component_id * I.connected_component

    let compare (a, b, c) (a', b', c') =
      let x = compare (a, b) (a', b') in
      if x <> 0 then
        x
      else
        I.compare_connected_component c c'

    let print a ((r, ar, dir), cc_id, cc) =
      let () =
        Format.fprintf a "Component_wise:(%a,%s,%s,%i,%a)" I.print_rule_id r
          (match ar with
          | Rule_modes.Usual -> "@"
          | Rule_modes.Unary | Rule_modes.Unary_refinement -> "(1)")
          (match dir with
          | Rule_modes.Direct -> "->"
          | Rule_modes.Op -> "<-")
          cc_id
          (I.print_connected_component ?compil:None)
          cc
      in
      let () = I.print_rule_id a r in
      let () = Format.fprintf a "cc_id: %i \n" cc_id in
      I.print_connected_component ?compil:None a cc
  end)

  module StoreMap = Store.Map

  type id = int
  type ode_var_id = id
  type var_id = id
  type obs_id = id
  type rule_id = id

  let fst_id = 1
  let next_id id = id + 1

  type ode_var =
    | Noccurrences of I.canonic_species
    | Nembed of I.canonic_species
    | Token of int
    | Dummy

  type lhs_decl = Init_decl | Var_decl of string | Init_value of ode_var

  module VarSetMap = SetMap.Make (struct
    type t = ode_var

    let compare = compare

    let print log x =
      match x with
      | Nembed x | Noccurrences x -> I.print_canonic_species log x
      | Token x -> Format.fprintf log "%i" x
      | Dummy -> ()
  end)

  module VarSet = VarSetMap.Set
  module VarMap = VarSetMap.Map

  type 'a decl =
    | Var of var_id * string option * ('a, int) Alg_expr.e Loc.annoted
    | Init_expr of var_id * ('a, int) Alg_expr.e Loc.annoted * ode_var_id list
    | Dummy_decl

  let var_id_of_decl decl =
    match decl with
    | Var (a, _, _) -> a
    | Init_expr (a, _, _) -> a
    | Dummy_decl -> fst_id

  type enriched_rule = {
    comment: string;
    rule_id_with_mode: rule_id * Rule_modes.arity * Rule_modes.direction;
    rule: I.rule;
    lhs: I.pattern;
    lhs_cc: (connected_component_id * I.connected_component) list;
    divide_rate_by: int;
  }

  let get_comment e = e.comment
  let get_rule_id_with_mode e = e.rule_id_with_mode
  let get_rule e = e.rule
  let get_lhs e = e.lhs
  let get_lhs_cc e = e.lhs_cc
  let get_divide_rate_by e = e.divide_rate_by

  let rule_id_of e =
    let a, _, _ = e.rule_id_with_mode in
    a

  let arity_of e =
    let _, a, _ = e.rule_id_with_mode in
    a

  let direction_of e =
    let _, _, a = e.rule_id_with_mode in
    a

  let var_of_rate (rule_id, arity, direction) =
    match arity, direction with
    | Rule_modes.Usual, Rule_modes.Direct -> Ode_loggers_sig.Rate rule_id
    | (Rule_modes.Unary | Rule_modes.Unary_refinement), Rule_modes.Direct ->
      Ode_loggers_sig.Rateun rule_id
    | Rule_modes.Usual, Rule_modes.Op -> Ode_loggers_sig.Rated rule_id
    | (Rule_modes.Unary | Rule_modes.Unary_refinement), Rule_modes.Op ->
      Ode_loggers_sig.Rateund rule_id

  let var_of_rule rule = var_of_rate rule.rule_id_with_mode

  let var_of_stoch rule n =
    let rule_id, _, _ = rule.rule_id_with_mode in
    Ode_loggers_sig.Stochiometric_coef (rule_id, n)

  type ('a, 'b) network = {
    rules: enriched_rule list;
    cc_to_rules: (enriched_rule * int) list I.ObsMap.t;
    cc_to_embedding_to_current_species: I.embedding list I.ObsMap.t;
    updated_cc_to_embedding_to_current_species: I.connected_component list;
    ode_variables: VarSet.t;
    reactions:
      ((id list * id list * id Loc.annoted list * enriched_rule) * int) list;
    ode_vars_tab: ode_var Mods.DynArray.t;
    id_of_ode_var: ode_var_id VarMap.t;
    fresh_ode_var_id: ode_var_id;
    species_tab: (I.chemical_species * int) Mods.DynArray.t;
    cache: I.cache;
    varmap: var_id Mods.IntMap.t;
    tokenmap: ode_var_id Mods.IntMap.t;
    fresh_var_id: var_id;
    var_declaration: 'a decl list;
    n_rules: int;
    obs: (obs_id * ('a, 'b) Alg_expr.e Loc.annoted) list;
    n_obs: int;
    time_homogeneous_obs: bool option;
    time_homogeneous_vars: bool option;
    time_homogeneous_rates: bool option;
    symmetries: Symmetries.symmetries option;
    sym_reduction: Symmetries.reduction;
    max_stoch_coef: int;
    fictitious_species: id option;
    has_empty_lhs: bool option;
    has_time_reaction: bool option;
  }

  let get_data network =
    ( network.n_rules,
      (List.length network.reactions
      -
      match network.has_time_reaction with
      | Some true -> 1
      | None | Some false -> 0),
      VarSet.fold
        (fun a n ->
          match a with
          | Nembed _ | Noccurrences _ -> n + 1
          | Token _ | Dummy -> n)
        network.ode_variables 0
      -
      match network.has_empty_lhs with
      | None | Some false -> 0
      | Some true -> 1 )

  let may_be_time_homogeneous_gen a =
    match a with
    | Some false -> false
    | Some true | None -> true

  let may_be_not_time_homogeneous_gen a =
    match a with
    | Some false | None -> true
    | Some true -> false

  let var_may_be_not_time_homogeneous network =
    may_be_not_time_homogeneous_gen network.time_homogeneous_vars

  let obs_may_be_not_time_homogeneous network =
    var_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_obs

  let rate_may_be_not_time_homogeneous network =
    var_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_rates

  let may_be_not_time_homogeneous network =
    rate_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_obs

  let get_fresh_var_id network = network.fresh_var_id
  let get_last_var_id network = network.fresh_var_id - 1

  let inc_fresh_var_id network =
    { network with fresh_var_id = next_id network.fresh_var_id }

  let get_fresh_ode_var_id network = network.fresh_ode_var_id
  let get_last_ode_var_id network = network.fresh_ode_var_id - 1

  let inc_fresh_ode_var_id network =
    { network with fresh_ode_var_id = next_id network.fresh_ode_var_id }

  let get_fresh_obs_id network = network.n_obs
  let last_fresh_obs_id network = network.n_obs - 1
  let inc_fresh_obs_id network = { network with n_obs = next_id network.n_obs }
  let fold_left_swap f a b = List.fold_left (fun a b -> f b a) b a
  let get_compil = I.get_compil
  let get_preprocessed_ast = I.get_preprocessed_ast
  let get_ast = I.get_ast
  let to_ast = I.to_ast
  let preprocess = I.preprocess

  let reset compil network =
    {
      network with
      ode_variables = VarSet.empty;
      ode_vars_tab = Mods.DynArray.create 0 Dummy;
      id_of_ode_var = VarMap.empty;
      species_tab = Mods.DynArray.create 0 (I.dummy_chemical_species compil, 1);
      cache = I.empty_cache compil;
      fresh_ode_var_id = fst_id;
      fresh_var_id = fst_id;
      varmap = Mods.IntMap.empty;
      tokenmap = Mods.IntMap.empty;
      var_declaration = [];
      obs = [];
      n_obs = 1;
      time_homogeneous_vars = None;
      time_homogeneous_obs = None;
      time_homogeneous_rates = None;
      (* symmetries = None ;
         sym_reduction = Symmetries.Ground ;*)
    }

  let add_embed_to_current_species cc embed network =
    let _old, network =
      match I.ObsMap.get cc network.cc_to_embedding_to_current_species with
      | [] ->
        ( [],
          {
            network with
            updated_cc_to_embedding_to_current_species =
              cc :: network.updated_cc_to_embedding_to_current_species;
          } )
      | _ :: _ as old -> old, network
    in
    let cc_to_embedding_to_current_species =
      I.ObsMap.add cc embed network.cc_to_embedding_to_current_species
    in
    { network with cc_to_embedding_to_current_species }

  let get_embed_to_current_species cc network =
    I.ObsMap.get cc network.cc_to_embedding_to_current_species

  let clean_embed_to_current_species network =
    let l = network.updated_cc_to_embedding_to_current_species in
    let updated_cc_to_embedding_to_current_species = [] in
    let cc_to_embedding_to_current_species =
      List.fold_left
        (fun cc_to_embedding_to_current_species k ->
          I.ObsMap.reset k cc_to_embedding_to_current_species)
        network.cc_to_embedding_to_current_species l
    in
    {
      network with
      updated_cc_to_embedding_to_current_species;
      cc_to_embedding_to_current_species;
    }

  let init compil =
    {
      rules = [];
      cc_to_rules = I.ObsMap.empty [];
      cc_to_embedding_to_current_species = I.ObsMap.empty [];
      updated_cc_to_embedding_to_current_species = [];
      reactions = [];
      ode_variables = VarSet.empty;
      ode_vars_tab = Mods.DynArray.create 0 Dummy;
      id_of_ode_var = VarMap.empty;
      species_tab = Mods.DynArray.create 0 (I.dummy_chemical_species compil, 1);
      cache = I.empty_cache compil;
      fresh_ode_var_id = fst_id;
      fresh_var_id = fst_id;
      varmap = Mods.IntMap.empty;
      tokenmap = Mods.IntMap.empty;
      var_declaration = [];
      n_rules = 0;
      obs = [];
      n_obs = 1;
      time_homogeneous_vars = None;
      time_homogeneous_obs = None;
      time_homogeneous_rates = None;
      symmetries = None;
      sym_reduction = Symmetries.Ground;
      max_stoch_coef = 0;
      fictitious_species = None;
      has_empty_lhs = None;
      has_time_reaction = None;
    }

  let from_nembed_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Nil
    else
      Ode_loggers.Div nauto

  let from_nocc_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Mul nauto
    else
      Ode_loggers.Nil

  let to_nembed_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Nil
    else
      Ode_loggers.Mul nauto

  let to_nocc_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Div nauto
    else
      Ode_loggers.Nil

  let lift f compil expr nauto =
    match f compil nauto with
    | Ode_loggers.Nil -> expr
    | Ode_loggers.Div n -> Alg_expr.div expr (Alg_expr.int n)
    | Ode_loggers.Mul n -> Alg_expr.mult (Alg_expr.int n) expr

  let to_nembed = lift to_nembed_correct
  let to_nocc = lift to_nocc_correct

  let to_var compil expr auto =
    if I.internal_meaning_is_nembeddings compil then
      to_nembed compil expr auto
    else
      to_nocc compil expr auto

  let species_to_var compil species =
    if I.internal_meaning_is_nembeddings compil then
      Nembed species
    else
      Noccurrences species

  let from_nembed = lift from_nembed_correct
  let from_nocc = lift from_nocc_correct

  let is_known_variable variable network =
    VarSet.mem variable network.ode_variables

  let add_new_var var network =
    let () =
      Mods.DynArray.set network.ode_vars_tab (get_fresh_ode_var_id network) var
    in
    let network =
      {
        network with
        ode_variables = VarSet.add var network.ode_variables;
        id_of_ode_var =
          VarMap.add var network.fresh_ode_var_id network.id_of_ode_var;
      }
    in
    inc_fresh_ode_var_id network, get_fresh_ode_var_id network

  let add_ficitious_species network =
    let network =
      { network with fictitious_species = Some (get_fresh_ode_var_id network) }
    in
    let network = inc_fresh_ode_var_id network in
    network

  let add_new_canonic_species ~debug_mode compil canonic species network =
    let () =
      Mods.DynArray.set network.species_tab
        (get_fresh_ode_var_id network)
        (species, I.nbr_automorphisms_in_chemical_species ~debug_mode species)
    in
    let var = species_to_var compil canonic in
    add_new_var var network

  let add_new_token token network =
    let network, id = add_new_var (Token token) network in
    { network with tokenmap = Mods.IntMap.add token id network.tokenmap }, id

  let enrich_rule cache compil rule rule_id_with_mode =
    let lhs = I.lhs compil rule_id_with_mode rule in
    let _succ_last_cc_id, lhs_cc =
      List.fold_left
        (fun (counter, list) cc -> next_cc_id counter, (counter, cc) :: list)
        (fst_cc_id, [])
        (List.rev (I.connected_components_of_patterns lhs))
    in
    let cache, divide_rate_by =
      I.divide_rule_rate_by cache compil rule
      (* to do, check if we do not compute it many times *)
    in
    ( cache,
      {
        comment = I.rate_name compil rule rule_id_with_mode;
        rule_id_with_mode;
        rule;
        lhs;
        lhs_cc;
        divide_rate_by;
      } )

  let add_embedding_list key lembed store =
    let old_list = StoreMap.find_default [] key store in
    let new_list = fold_left_swap (fun a b -> a :: b) lembed old_list in
    StoreMap.add key new_list store

  let translate_canonic_species compil canonic species remanent =
    let debug_mode = I.debug_mode compil in
    let var = species_to_var compil canonic in
    let id_opt = VarMap.find_option var (snd remanent).id_of_ode_var in
    match id_opt with
    | None ->
      let () = debug ~debug_mode "A NEW SPECIES IS DISCOVERED @." in
      let () =
        debug ~debug_mode "canonic form: %a@."
          (fun x -> I.print_canonic_species ~compil x)
          canonic
      in
      let () =
        debug ~debug_mode "species: %a@.@."
          (fun x -> I.print_chemical_species ~compil x)
          species
      in
      let to_be_visited, network = remanent in
      let network, id =
        add_new_canonic_species ~debug_mode compil canonic species network
      in
      (species :: to_be_visited, network), id
    | Some i ->
      let () = debug ~debug_mode "ALREADY SEEN SPECIES @." in
      let () =
        debug ~debug_mode "canonic form: %a@."
          (fun x -> I.print_canonic_species ~compil x)
          canonic
      in
      let () =
        debug ~debug_mode "species: %a@.@."
          (fun x -> I.print_chemical_species ~compil x)
          species
      in
      remanent, i

  let representative parameters compil network species =
    match network.sym_reduction with
    | Symmetries.Ground -> network, species
    | Symmetries.Forward _ | Symmetries.Backward _ ->
      let cache = network.cache in
      let cache, species =
        I.get_representative parameters compil cache network.sym_reduction
          species
      in
      { network with cache }, species

  let translate_species parameters compil species remanent =
    let network, species =
      representative parameters compil (snd remanent) species
    in
    let remanent = fst remanent, network in
    translate_canonic_species compil (I.canonic_form species) species remanent

  let translate_token token remanent =
    let id_opt =
      VarMap.find_option (Token token) (snd remanent).id_of_ode_var
    in
    match id_opt with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_token token network in
      (to_be_visited, network), id
    | Some i -> remanent, i

  let petrify_species parameters compil species remanent =
    let network, species =
      representative parameters compil (snd remanent) species
    in
    let remanent = fst remanent, network in
    translate_canonic_species compil (I.canonic_form species) species remanent

  let petrify_species_list parameters compil l remanent =
    fold_left_swap
      (fun species (remanent, l) ->
        let remanent, i = petrify_species parameters compil species remanent in
        remanent, i :: l)
      l (remanent, [])

  let petrify_mixture parameters compil mixture (acc, network) =
    let cache, species =
      I.connected_components_of_mixture compil network.cache mixture
    in
    petrify_species_list parameters compil species (acc, { network with cache })

  let add_to_prefix_list connected_component key prefix_list store acc =
    let list_embeddings = StoreMap.find_default [] key store in
    List.fold_left
      (fun new_list prefix ->
        List.fold_left
          (fun new_list (embedding, chemical_species) ->
            ((connected_component, embedding, chemical_species) :: prefix)
            :: new_list)
          new_list list_embeddings)
      acc prefix_list

  let count_in_connected_component_fwd target compil network connected_component
      =
    ( network,
      VarMap.fold
        (fun vars id alg ->
          match vars with
          | Token _ | Dummy -> alg
          | Nembed _ | Noccurrences _ ->
            let from =
              match vars with
              | Token _ | Dummy -> assert false
              | Nembed _ -> from_nembed compil
              | Noccurrences _ -> from_nocc compil
            in
            let species, nauto = Mods.DynArray.get network.species_tab id in
            let n_embs =
              List.length (I.find_embeddings compil connected_component species)
            in
            if n_embs = 0 then
              alg
            else (
              let species = Loc.annot_with_dummy (Alg_expr.KAPPA_INSTANCE id) in
              let term =
                target compil
                  (from (Alg_expr.mult (Alg_expr.int n_embs) species) nauto)
                  nauto
              in
              if fst alg = Alg_expr.CONST Nbr.zero then
                term
              else
                Alg_expr.add alg term
            ))
        network.id_of_ode_var (Alg_expr.const Nbr.zero) )

  let count_in_connected_component_bwd target parameters compil network
      connected_component =
    let cache = network.cache in
    let cache, (w, l) =
      I.equiv_class_of_pattern parameters compil cache network.sym_reduction
        connected_component
    in
    let network = { network with cache } in
    let term network h wh =
      let network, expr =
        count_in_connected_component_fwd target compil network h
      in
      ( network,
        Alg_expr.div (Alg_expr.mult (Alg_expr.int wh) expr) (Alg_expr.int w) )
    in
    let rec aux list (network, expr) =
      match list with
      | [] -> network, expr
      | (h, w) :: tail ->
        let network, term = term network h w in
        aux tail (network, Alg_expr.add term expr)
    in
    match l with
    | [] -> network, Alg_expr.int 0
    | (h, w) :: tail -> aux tail (term network h w)

  let nembed_of_connected_component parameters compil network
      connected_component =
    match network.sym_reduction with
    | Symmetries.Ground | Symmetries.Forward _ ->
      count_in_connected_component_fwd to_nembed compil network
        connected_component
    | Symmetries.Backward _ ->
      count_in_connected_component_bwd to_nembed parameters compil network
        connected_component

  let nocc_of_connected_component parameters compil network connected_component
      =
    match network.sym_reduction with
    | Symmetries.Ground | Symmetries.Forward _ ->
      count_in_connected_component_fwd to_nocc compil network
        connected_component
    | Symmetries.Backward _ ->
      count_in_connected_component_bwd to_nocc parameters compil network
        connected_component

  let rec convert_alg_expr parameter compil network alg =
    match alg with
    | Alg_expr.BIN_ALG_OP (op, arg1, arg2), loc ->
      let network, output1 = convert_alg_expr parameter compil network arg1 in
      let network, output2 = convert_alg_expr parameter compil network arg2 in
      network, (Alg_expr.BIN_ALG_OP (op, output1, output2), loc)
    | Alg_expr.UN_ALG_OP (op, arg), loc ->
      let network, output = convert_alg_expr parameter compil network arg in
      network, (Alg_expr.UN_ALG_OP (op, output), loc)
    | Alg_expr.KAPPA_INSTANCE cc, _loc ->
      let f x network =
        Array.fold_left
          (fun (network, expr) h ->
            let network, expr' =
              nocc_of_connected_component parameter compil network h
            in
            network, Alg_expr.mult expr expr')
          (network, Alg_expr.const Nbr.one)
          x
      in
      (match cc with
      | [] -> network, Alg_expr.const Nbr.zero
      | head :: tail ->
        List.fold_left
          (fun (network, acc) l ->
            let network, expr = f l network in
            network, Alg_expr.add acc expr)
          (f head network) tail)
    | Alg_expr.TOKEN_ID id, loc ->
      let id = snd (translate_token id ([], network)) in
      network, (Alg_expr.TOKEN_ID id, loc)
    | ((Alg_expr.ALG_VAR _ | Alg_expr.CONST _ | Alg_expr.STATE_ALG_OP _), _) as
      a ->
      network, a
    | Alg_expr.IF (cond, yes, no), pos ->
      let network, outputb = convert_bool_expr parameter compil network cond in
      let network, outputyes = convert_alg_expr parameter compil network yes in
      let network, outputno = convert_alg_expr parameter compil network no in
      network, (Alg_expr.IF (outputb, outputyes, outputno), pos)
    | Alg_expr.DIFF_TOKEN (expr, dt), pos ->
      let network, output = convert_alg_expr parameter compil network expr in
      network, (Alg_expr.DIFF_TOKEN (output, dt), pos)
    | Alg_expr.DIFF_KAPPA_INSTANCE (_expr, _dt), pos ->
      raise
        (ExceptionDefn.Internal_Error
           ("Cannot translate partial derivative", pos))

  and convert_bool_expr parameter compil network = function
    | ((Alg_expr.TRUE | Alg_expr.FALSE), _) as a -> network, a
    | Alg_expr.COMPARE_OP (op, a, b), pos ->
      let network, outputa = convert_alg_expr parameter compil network a in
      let network, outputb = convert_alg_expr parameter compil network b in
      network, (Alg_expr.COMPARE_OP (op, outputa, outputb), pos)
    | Alg_expr.BIN_BOOL_OP (op, a, b), pos ->
      let network, outputa = convert_bool_expr parameter compil network a in
      let network, outputb = convert_bool_expr parameter compil network b in
      network, (Alg_expr.BIN_BOOL_OP (op, outputa, outputb), pos)
    | Alg_expr.UN_BOOL_OP (op, a), pos ->
      let network, outputa = convert_bool_expr parameter compil network a in
      network, (Alg_expr.UN_BOOL_OP (op, outputa), pos)

  let add_reaction ?max_size parameters compil enriched_rule embedding_forest
      mixture remanent =
    let debug_mode = I.debug_mode compil in
    let rule = enriched_rule.rule in
    let _ = debug ~debug_mode "REACTANTS\n" in
    let remanent, reactants =
      petrify_mixture parameters compil mixture remanent
    in
    let _ = debug ~debug_mode "PRODUCT\n" in
    let products = I.apply compil rule embedding_forest mixture in
    let list, network = remanent in
    let cache, bool = I.valid_mixture compil network.cache ?max_size products in
    let network = { network with cache } in
    let remanent = list, network in
    if bool then (
      let tokens = I.token_vector rule in
      let remanent, products =
        petrify_mixture parameters compil products remanent
      in
      let remanent, tokens =
        List.fold_left
          (fun (remanent, tokens) (_, b) ->
            let remanent, id = translate_token b remanent in
            remanent, Loc.annot_with_dummy id :: tokens)
          (remanent, []) tokens
      in
      let to_be_visited, network = remanent in
      let network =
        {
          network with
          reactions =
            ( ( List.rev reactants,
                List.rev products,
                List.rev tokens,
                enriched_rule ),
              1 )
            :: network.reactions;
        }
      in
      to_be_visited, network
    ) else
      remanent

  let initial_network ?max_size parameters compil network initial_states rules =
    let network = { network with has_empty_lhs = Some false } in
    let debug_mode = I.debug_mode compil in
    let l, network =
      List.fold_left
        (fun remanent enriched_rule ->
          match enriched_rule.lhs_cc with
          | [] ->
            let _, embed, mixture = I.disjoint_union compil [] in
            let () = debug ~debug_mode "add new reaction" in
            let l, network = remanent in
            let remanent = l, { network with has_empty_lhs = Some true } in
            add_reaction ?max_size parameters compil enriched_rule embed mixture
              remanent
          | _ :: _ -> remanent)
        (List.fold_left
           (fun remanent species ->
             fst (translate_species parameters compil species remanent))
           ([], network) initial_states)
        rules
    in
    if not (I.do_we_allow_empty_lhs compil) then (
      match network.has_empty_lhs with
      | None -> failwith "Internal error"
      | Some true -> l, add_ficitious_species network
      | Some false -> l, network
    ) else
      l, network

  let compare_reaction (react, prod, token, enriched_rule)
      (react', prod', token', enriched_rule') =
    let cmp = compare (rule_id_of enriched_rule) (rule_id_of enriched_rule') in
    if cmp = 0 then (
      let cmp = compare react react' in
      if cmp = 0 then (
        let cmp = compare prod prod' in
        if cmp = 0 then
          compare token token'
        else
          cmp
      ) else
        cmp
    ) else
      cmp

  let compare_extended_reaction a b = compare_reaction (fst a) (fst b)

  let compute_reactions ?max_size ~smash_reactions parameters compil network
      rules initial_states =
    (* Let us annotate the rules with cc decomposition *)
    let debug_mode = I.debug_mode compil in
    let n_rules = List.length rules in
    let cache = network.cache in
    let cache, max_coef, rules_rev =
      List.fold_left
        (fun (cache, max_coef, list) rule ->
          let cache, modes = I.valid_modes cache compil rule in
          let max_coef = max max_coef (List.length (I.token_vector rule)) in
          let a, b =
            List.fold_left
              (fun (cache, list) mode ->
                let cache, elt = enrich_rule cache compil rule mode in
                cache, elt :: list)
              (cache, list) modes
          in
          a, max_coef, b)
        ((cache : I.cache), network.max_stoch_coef, [])
        (List.rev rules)
    in
    let obsmap = network.cc_to_rules in
    let obsmap =
      List.fold_left
        (fun obsmap ext_rule ->
          let cc_list = ext_rule.lhs_cc in
          List.fold_left
            (fun obs_map (cc_id, cc) ->
              I.ObsMap.add cc (ext_rule, cc_id) obs_map)
            obsmap cc_list)
        obsmap rules_rev
    in
    let network = { network with cache; max_stoch_coef = max_coef } in
    let rules = List.rev rules_rev in
    let to_be_visited, network =
      initial_network ?max_size parameters compil network initial_states rules
    in
    let network = { network with n_rules; rules; cc_to_rules = obsmap } in
    let store = StoreMap.empty in
    (* store maps each cc in the lhs of a rule to the list of
       embedding between this cc and a pattern in set\to_be_visited
    *)
    let rec aux to_be_visited network store =
      match to_be_visited with
      | [] -> network
      | new_species :: to_be_visited ->
        let network = clean_embed_to_current_species network in
        let () =
          debug ~debug_mode "@[<v 2>@[test for the new species:@ %a@]"
            (fun x -> I.print_chemical_species ~compil x)
            new_species
        in
        (* add in store the embeddings from cc of lhs to
           new_species,
           for unary application of binary rule, the dictionary of
           species is updated, and the reaction entered directly *)
        let store, to_be_visited, network =
          let all_ccs = I.find_all_embeddings compil new_species in
          List.fold_left
            (fun (store_old_embeddings, to_be_visited, network) (cc, embed) ->
              let pairs_rule_pos = I.ObsMap.get cc network.cc_to_rules in
              List.fold_left
                (fun (store_old_embeddings, to_be_visited, network)
                     (enriched_rule, pos) ->
                  (* regular application of tules, we store the
                     embeddings*)
                  let () =
                    debug ~debug_mode
                      "@[<v 2>test for rule %i at pos %i (Aut:%i)@[%a@]"
                      (rule_id_of enriched_rule) pos
                      enriched_rule.divide_rate_by (I.print_rule ~compil)
                      enriched_rule.rule
                  in
                  match arity_of enriched_rule with
                  | Rule_modes.Usual | Rule_modes.Unary_refinement ->
                    let () = debug ~debug_mode "regular case" in
                    let store_new_embeddings =
                      (*List.fold_left
                        (fun store (cc_id,cc) ->
                           let () = debug ~debug_mode "find embeddings" in
                           let lembed =
                             I.find_embeddings compil cc new_species
                           in
                           add_embedding_list
                             (enriched_rule.rule_id_with_mode,cc_id,cc)
                             (List.rev_map (fun a -> a,new_species)
                                (List.rev lembed))
                             store
                        ) StoreMap.empty enriched_rule.lhs_cc*)
                      add_embedding_list
                        (enriched_rule.rule_id_with_mode, pos, cc)
                        [ embed, new_species ]
                        StoreMap.empty
                    in
                    let (), store_all_embeddings =
                      StoreMap.map2_with_logs
                        (fun _ a _ _ _ -> a)
                        () ()
                        (fun _ _ b -> (), b)
                        (fun _ _ b -> (), b)
                        (fun _ _ b c ->
                          (), List.fold_left (fun list elt -> elt :: list) b c)
                        store_old_embeddings store_new_embeddings
                    in
                    (* compute the embedding betwen lhs and tuple of
                       species that contain at least one occurence of
                       new_species *)
                    let dump_store store =
                      if local_trace || debug_mode then
                        StoreMap.iter
                          (fun ((a, ar, dir), id, b) c ->
                            let () =
                              debug ~debug_mode
                                "@[<v 2>* rule:%i %s %s  cc:%i:@[%a@]:" a
                                (match ar with
                                | Rule_modes.Usual -> "@"
                                | Rule_modes.Unary_refinement | Rule_modes.Unary
                                  ->
                                  "(1)")
                                (match dir with
                                | Rule_modes.Direct -> "->"
                                | Rule_modes.Op -> "<-")
                                id
                                (I.print_connected_component ~compil)
                                b
                            in
                            let () =
                              List.iter
                                (fun (_, b) ->
                                  debug ~debug_mode "%a"
                                    (fun x ->
                                      I.print_chemical_species ~compil x)
                                    b)
                                c
                            in
                            let () = debug ~debug_mode "@]" in
                            ())
                          store
                    in
                    let () = debug ~debug_mode "new embeddings" in
                    let () = dump_store store_new_embeddings in
                    let _, new_embedding_list =
                      List.fold_left
                        (fun ( partial_emb_list,
                               partial_emb_list_with_new_species ) (cc_id, cc) ->
                          (* First case, we complete with an
                             embedding towards the new_species *)
                          let label =
                            enriched_rule.rule_id_with_mode, cc_id, cc
                          in
                          let partial_emb_list_with_new_species =
                            add_to_prefix_list cc label partial_emb_list
                              store_new_embeddings
                              (add_to_prefix_list cc label
                                 partial_emb_list_with_new_species
                                 store_all_embeddings [])
                          in
                          let partial_emb_list =
                            add_to_prefix_list cc label partial_emb_list
                              store_old_embeddings []
                          in
                          partial_emb_list, partial_emb_list_with_new_species)
                        ([ [] ], []) enriched_rule.lhs_cc
                    in
                    (* compute the corresponding rhs, and put the new
                       species in the working list, and store the
                       corrsponding reactions *)
                    let to_be_visited, network =
                      List.fold_left
                        (fun remanent list ->
                          let () = debug ~debug_mode "compute one refinement" in
                          let () =
                            debug ~debug_mode "disjoint union @[<v>%a@]"
                              (Pp.list Pp.space (fun f (_, _, s) ->
                                   I.print_chemical_species ~compil f s))
                              list
                          in
                          let _, embed, mixture =
                            I.disjoint_union compil list
                          in
                          let () = debug ~debug_mode "add new reaction" in
                          add_reaction ?max_size parameters compil enriched_rule
                            embed mixture remanent)
                        (to_be_visited, network) new_embedding_list
                    in
                    let () = debug ~debug_mode "@]" in
                    store_all_embeddings, to_be_visited, network
                  | Rule_modes.Unary ->
                    (* unary application of binary rules *)
                    let () = debug ~debug_mode "unary case" in
                    let network =
                      add_embed_to_current_species cc embed network
                    in
                    let emb_list_list_opt =
                      let rec aux list output =
                        match list with
                        | [] -> Some output
                        | (cc_id, cc) :: tail ->
                          let emb_list =
                            if cc_id = pos then
                              [ embed ]
                            else
                              get_embed_to_current_species cc network
                          in
                          (match emb_list with
                          | [] -> None
                          | _ :: _ -> aux tail (emb_list :: output))
                      in
                      aux enriched_rule.lhs_cc []
                    in
                    let to_be_visited, network =
                      match emb_list_list_opt with
                      | Some l ->
                        let lembed, mix =
                          I.compose_embeddings_unary_binary compil
                            enriched_rule.lhs l new_species
                        in
                        fold_left_swap
                          (fun embed remanent ->
                            let () =
                              debug ~debug_mode "add new reaction (unary)"
                            in
                            let embed =
                              add_reaction ?max_size parameters compil
                                enriched_rule embed mix remanent
                            in
                            let () =
                              debug ~debug_mode "end new reaction (unary)"
                            in
                            embed)
                          lembed (to_be_visited, network)
                      | None -> to_be_visited, network
                    in
                    let () = debug ~debug_mode "@]" in
                    store_old_embeddings, to_be_visited, network)
                (store_old_embeddings, to_be_visited, network)
                pairs_rule_pos)
            (store, to_be_visited, network)
            all_ccs
        in
        let () = debug ~debug_mode "@]" in
        aux to_be_visited network store
    in
    let network = aux to_be_visited network store in
    let network =
      if smash_reactions then (
        let normalised_reactions_list =
          List.fold_left
            (fun store_list ((react, products, token_vector, enrich_rule), nocc) ->
              let sorted_reactants = List.sort compare react in
              let sorted_products = List.sort compare products in
              let sorted_token_vector = List.sort compare token_vector in
              ( ( sorted_reactants,
                  sorted_products,
                  sorted_token_vector,
                  enrich_rule ),
                nocc )
              :: store_list)
            [] network.reactions
        in
        let sorted_reactions_list =
          List.sort compare_extended_reaction normalised_reactions_list
        in
        let smashed_list =
          Tools.smash_duplicate_in_ordered_list compare_reaction
            sorted_reactions_list
        in
        { network with reactions = smashed_list }
      ) else
        network
    in
    let network =
      match I.reaction_rate_convention compil with
      | None | Some Remanent_parameters_sig.No_correction -> network
      | Some Remanent_parameters_sig.Biochemist
      | Some Remanent_parameters_sig.Common ->
        let reactions = network.reactions in
        let reactions =
          List.rev_map
            (fun (reaction, nocc) ->
              let reactants, products, _, _ = reaction in
              let correct =
                Tools.get_product_image_occ_2 1
                  (fun i j -> i * j)
                  (fun i j -> Tools.fact (min i j))
                  reactants products
              in
              reaction, nocc * correct)
            (List.rev reactions)
        in
        { network with reactions }
      | Some Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs ->
        let reactions = network.reactions in
        let reactions =
          List.rev_map
            (fun (reaction, nocc) ->
              let reactants, _products, _, _ = reaction in
              let correct =
                Tools.get_product_image_occ 1
                  (fun i j -> i * j)
                  (fun i -> Tools.fact i)
                  reactants
              in
              reaction, nocc * correct)
            (List.rev reactions)
        in
        { network with reactions }
    in
    let () = debug ~debug_mode "@]@." in
    network

  let convert_tokens compil network =
    Tools.recti
      (fun network a -> snd (fst (translate_token a ([], network))))
      network (I.nb_tokens compil)

  let species_of_species_id network i = Mods.DynArray.get network.species_tab i
  let get_reactions network = network.reactions

  let convert_initial_state parameters compil intro network =
    let b, c = intro in
    let network, expr_init =
      convert_alg_expr parameters compil network (Loc.annot_with_dummy b)
    in
    ( expr_init,
      match I.token_vector_of_init c with
      | [] ->
        let m = I.mixture_of_init compil c in
        let cache', cc =
          I.connected_components_of_mixture compil network.cache m
        in
        let network = { network with cache = cache' } in
        List.fold_left
          (fun (network, acc) x ->
            let (_, n'), v =
              translate_species parameters compil x ([], network)
            in
            n', v :: acc)
          (network, []) (List.rev cc)
      | l ->
        List.fold_right
          (fun (_, token) (network, acc) ->
            let (_, n'), v = translate_token token ([], network) in
            n', v :: acc)
          l (network, []) )

  let translate_token token network = snd (translate_token token ([], network))

  let convert_var_def parameters compil network variable_def =
    let a, b = variable_def in
    a, convert_alg_expr parameters compil network b

  let convert_var_defs parameters compil network =
    let list_var = I.get_variables compil in
    let init = I.get_init compil in
    let list, network =
      Tools.array_fold_lefti
        (fun i (list, network) def ->
          let a, (network, b) = convert_var_def parameters compil network def in
          ( Var (get_fresh_var_id network, Some a, b) :: list,
            inc_fresh_var_id
              {
                network with
                varmap =
                  Mods.IntMap.add i (get_fresh_var_id network) network.varmap;
              } ))
        ([], network) list_var
    in
    let init_tab = Mods.DynArray.make (get_fresh_ode_var_id network) [] in
    let add i j =
      Mods.DynArray.set init_tab i (j :: Mods.DynArray.get init_tab i)
    in
    let list, network =
      List.fold_left
        (fun (list, network) def ->
          let b, (network, c) =
            convert_initial_state parameters compil def network
          in
          let () = List.iter (fun id -> add id (get_fresh_var_id network)) c in
          ( Init_expr (network.fresh_var_id, b, c) :: list,
            inc_fresh_var_id network ))
        (list, network) init
    in
    let size = List.length list in
    let npred = Mods.DynArray.create (get_fresh_var_id network) 0 in
    let lsucc = Mods.DynArray.create (get_fresh_var_id network) [] in
    let dec_tab =
      Mods.DynArray.create network.fresh_var_id
        (Dummy_decl, None, Alg_expr.const Nbr.zero)
    in
    let add_succ i j =
      let () = Mods.DynArray.set npred j (1 + Mods.DynArray.get npred j) in
      let () = Mods.DynArray.set lsucc i (j :: Mods.DynArray.get lsucc i) in
      ()
    in
    let rec aux_alg id expr =
      match fst expr with
      | Alg_expr.CONST _ -> ()
      | Alg_expr.BIN_ALG_OP (_, a, b) ->
        aux_alg id a;
        aux_alg id b
      | Alg_expr.UN_ALG_OP (_, a)
      | Alg_expr.DIFF_KAPPA_INSTANCE (a, _)
      | Alg_expr.DIFF_TOKEN (a, _) ->
        aux_alg id a
      | Alg_expr.STATE_ALG_OP _ -> ()
      | Alg_expr.IF (cond, yes, no) ->
        aux_bool id cond;
        aux_alg id yes;
        aux_alg id no
      | Alg_expr.TOKEN_ID id' ->
        let list = Mods.DynArray.get init_tab id' in
        List.iter (fun id'' -> add_succ id id'') list
      | Alg_expr.KAPPA_INSTANCE (id' : ode_var_id) ->
        let list = Mods.DynArray.get init_tab id' in
        List.iter (fun id'' -> add_succ id id'') list
      | Alg_expr.ALG_VAR id' ->
        let id_opt = Mods.IntMap.find_option id' network.varmap in
        (match id_opt with
        | Some id'' -> add_succ id id''
        | None -> ())
    and aux_bool id = function
      | (Alg_expr.TRUE | Alg_expr.FALSE), _ -> ()
      | Alg_expr.COMPARE_OP (_, a, b), _ ->
        aux_alg id a;
        aux_alg id b
      | Alg_expr.UN_BOOL_OP (_, a), _ -> aux_bool id a
      | Alg_expr.BIN_BOOL_OP (_, a, b), _ ->
        aux_bool id a;
        aux_bool id b
    in
    let () =
      List.iter
        (fun decl ->
          match decl with
          | Dummy_decl -> ()
          | Init_expr (id, b, _) ->
            let () = Mods.DynArray.set dec_tab id (decl, None, b) in
            aux_alg id b
          | Var (id, a, b) ->
            let () = Mods.DynArray.set dec_tab id (decl, a, b) in
            aux_alg id b)
        list
    in
    let top_sort =
      let clean k to_be_visited =
        let l = Mods.DynArray.get lsucc k in
        List.fold_left
          (fun to_be_visited j ->
            let old = Mods.DynArray.get npred j in
            let () = Mods.DynArray.set npred j (old - 1) in
            if old = 1 then
              j :: to_be_visited
            else
              to_be_visited)
          to_be_visited l
      in
      let to_be_visited =
        let rec aux k l =
          if k < fst_id then
            l
          else if Mods.DynArray.get npred k = 0 then
            aux (k - 1) (k :: l)
          else
            aux (k - 1) l
        in
        aux (network.fresh_var_id - 1) []
      in
      let rec aux to_be_visited l =
        match to_be_visited with
        | [] -> List.rev l
        | h :: t -> aux (clean h t) (h :: l)
      in
      let l = aux to_be_visited [] in
      let l =
        List.rev_map
          (fun x ->
            let decl, _, _ = Mods.DynArray.get dec_tab x in
            decl)
          l
      in
      l
    in
    let size' = List.length top_sort in
    if size' = size then
      { network with var_declaration = top_sort }
    else (
      let () = Format.fprintf Format.std_formatter "Circular dependencies\n" in
      assert false
    )

  let convert_one_obs parameters obs network =
    let a, b = obs in
    a, convert_alg_expr parameters b network

  let convert_obs parameters compil network =
    let list_obs = I.get_obs compil in
    let network =
      List.fold_left
        (fun network obs ->
          let network, expr_obs =
            convert_alg_expr parameters compil network
              (Loc.annot_with_dummy obs)
          in
          inc_fresh_obs_id
            {
              network with
              obs = (get_fresh_obs_id network, expr_obs) :: network.obs;
            })
        network list_obs
    in
    { network with obs = List.rev network.obs; n_obs = network.n_obs - 1 }

  let build_time_var network =
    if may_be_not_time_homogeneous network then
      Some (get_last_ode_var_id network)
    else
      None

  let species_of_initial_state compil network list =
    let cache = network.cache in
    let cache, list =
      List.fold_left
        (fun (cache, list) (_, r) ->
          let b = I.mixture_of_init compil r in
          let cache', acc = I.connected_components_of_mixture compil cache b in
          cache', List.rev_append acc list)
        (cache, []) list
    in
    { network with cache }, list

  type ('a, 'b) rate = ('a, 'b) Alg_expr.e Loc.annoted
  type ('a, 'b) stoc = int * ('a, 'b) Alg_expr.e Loc.annoted
  type ('a, 'b) coef = R of ('a, 'b) rate | S of ('a, 'b) stoc

  type ('a, 'b) sort_rules_and_decl = {
    const_decl_set: Mods.StringSet.t;
    const_decl: 'a decl list;
    var_decl: 'a decl list;
    init: 'a decl list;
    const_rate: (enriched_rule * ('a, 'b) coef list) list;
    var_rate: (enriched_rule * ('a, 'b) coef list) list;
  }

  let init_sort_rules_and_decl =
    {
      const_decl_set = Mods.StringSet.empty;
      const_decl = [];
      var_decl = [];
      const_rate = [];
      var_rate = [];
      init = [];
    }

  let split_var_declaration network sort_rules_and_decls =
    let decl =
      List.fold_left
        (fun sort_decls decl ->
          match decl with
          | Dummy_decl | Var (_, None, _) | Init_expr _ ->
            { sort_decls with init = decl :: sort_decls.init }
          | Var (_id, Some a, b) ->
            if Alg_expr.is_constant b then
              {
                sort_decls with
                const_decl_set = Mods.StringSet.add a sort_decls.const_decl_set;
                const_decl = decl :: sort_decls.const_decl;
              }
            else
              { sort_decls with var_decl = decl :: sort_decls.var_decl })
        sort_rules_and_decls network.var_declaration
    in
    {
      decl with
      const_decl = List.rev decl.const_decl;
      var_decl = List.rev decl.var_decl;
      init = List.rev decl.init;
    }

  let flatten_rate (id, mode, direct) =
    match mode with
    | Rule_modes.Unary | Rule_modes.Usual -> id, mode, direct
    | Rule_modes.Unary_refinement -> id, Rule_modes.Unary, direct

  let flatten_coef (id, _mode, direct) = id, Rule_modes.Usual, direct

  let split_rules parameters compil network sort_rules_and_decls =
    let rate_set = Rule_modes.RuleModeIdSet.empty in
    let coef_set = Rule_modes.RuleModeIdSet.empty in
    let network, sort, _ =
      List.fold_left
        (fun (network, sort_rules, (rate_set, coef_set)) enriched_rule ->
          if
            Rule_modes.RuleModeIdSet.mem
              (flatten_rate enriched_rule.rule_id_with_mode)
              rate_set
          then
            network, sort_rules, (rate_set, coef_set)
          else (
            let rate =
              I.rate compil enriched_rule.rule enriched_rule.rule_id_with_mode
            in
            match rate with
            | None -> network, sort_rules, (rate_set, coef_set)
            | Some rate ->
              let rate_set =
                Rule_modes.RuleModeIdSet.add
                  (flatten_rate enriched_rule.rule_id_with_mode)
                  rate_set
              in
              let const_list = [] in
              let var_list = [] in
              let network, rate =
                convert_alg_expr parameters compil network rate
              in
              let network, const_list, var_list =
                if Alg_expr.is_constant rate then
                  network, R rate :: const_list, var_list
                else
                  network, const_list, R rate :: var_list
              in
              let network, const_list, var_list, coef_set =
                if
                  Rule_modes.RuleModeIdSet.mem
                    (flatten_coef enriched_rule.rule_id_with_mode)
                    coef_set
                then
                  network, const_list, var_list, coef_set
                else (
                  let coef_set =
                    Rule_modes.RuleModeIdSet.add
                      (flatten_coef enriched_rule.rule_id_with_mode)
                      coef_set
                  in
                  let vector = I.token_vector enriched_rule.rule in
                  let network, const_list, var_list, _ =
                    List.fold_left
                      (fun (network, const_list, var_list, n) (expr, _) ->
                        let network, rate =
                          convert_alg_expr parameters compil network expr
                        in
                        if Alg_expr.is_constant rate then
                          network, S (n, rate) :: const_list, var_list, n + 1
                        else
                          network, const_list, S (n, rate) :: var_list, n + 1)
                      (network, const_list, var_list, 1)
                      vector
                  in
                  network, const_list, var_list, coef_set
                )
              in
              let sort_rules =
                match const_list with
                | [] -> sort_rules
                | _ :: _ ->
                  {
                    sort_rules with
                    const_rate =
                      (enriched_rule, const_list) :: sort_rules.const_rate;
                  }
              in
              let sort_rules =
                match var_list with
                | [] -> sort_rules
                | _ :: _ ->
                  {
                    sort_rules with
                    var_rate = (enriched_rule, var_list) :: sort_rules.var_rate;
                  }
              in
              network, sort_rules, (rate_set, coef_set)
          ))
        (network, sort_rules_and_decls, (rate_set, coef_set))
        network.rules
    in
    ( network,
      {
        sort with
        const_rate = List.rev sort.const_rate;
        var_rate = List.rev sort.var_rate;
      } )

  let split_rules_and_decl parameters compil network =
    split_rules parameters compil network
      (split_var_declaration network init_sort_rules_and_decl)

  let time_homogeneity_of_rates compil network =
    let rules = network.rules in
    List.for_all
      (fun rule ->
        let rate_opt = I.rate compil rule.rule rule.rule_id_with_mode in
        match rate_opt with
        | None -> true
        | Some rate -> Alg_expr.is_time_homogeneous rate)
      rules

  let time_homogeneity_of_vars network =
    let vars_decl = network.var_declaration in
    List.for_all
      (fun decl ->
        match decl with
        | Dummy_decl | Init_expr _ -> true
        | Var (_, _, expr) -> Alg_expr.is_time_homogeneous expr)
      vars_decl

  let time_homogeneity_of_obs network =
    let obs = network.obs in
    List.for_all (fun (_, expr) -> Alg_expr.is_time_homogeneous expr) obs

  let check_time_homogeneity ~ignore_obs compil network =
    {
      network with
      time_homogeneous_vars = Some (time_homogeneity_of_vars network);
      time_homogeneous_obs =
        Some
          (if ignore_obs then
             true
           else
             time_homogeneity_of_obs network);
      time_homogeneous_rates = Some (time_homogeneity_of_rates compil network);
    }

  let network_from_compil ?max_size ~smash_reactions ~ignore_obs parameters
      compil network =
    let () = Format.printf "+ generate the network... @." in
    let rules = I.get_rules compil in
    let () = Format.printf "\t -initial states @." in
    let network, initial_state =
      species_of_initial_state compil network (I.get_init compil)
    in
    let () = Format.printf "\t -saturating the set of molecular species @." in
    let network =
      compute_reactions ?max_size ~smash_reactions parameters compil network
        rules initial_state
    in
    let () = Format.printf "\t -tokens @." in
    let network = convert_tokens compil network in
    let () = Format.printf "\t -variables @." in
    let network = convert_var_defs parameters compil network in
    let () = Format.printf "\t -observables @." in
    let network = convert_obs parameters compil network in
    let () = Format.printf "\t -check time homogeneity @." in
    let network = check_time_homogeneity ~ignore_obs compil network in
    network

  let handler_init =
    {
      Network_handler.int_of_obs = (fun i -> i);
      Network_handler.int_of_kappa_instance = (fun i -> i);
      Network_handler.int_of_token_id = (fun i -> i);
    }

  let handler_expr network =
    {
      Network_handler.int_of_obs =
        (fun s -> Mods.IntMap.find_default s s network.varmap);
      Network_handler.int_of_kappa_instance = (fun i -> i);
      Network_handler.int_of_token_id = (fun i -> i);
    }

  let string_of_var_id ?compil ?init_mode t id =
    I.string_of_var_id ?compil ?init_mode (Ode_loggers_sig.lift t) id

  let increment ~propagate_constants is_zero ?(init_mode = false)
      ?(comment = "") string_of_var logger logger_buffer logger_err x =
    if is_zero x then
      Ode_loggers.associate ~propagate_constants ~init_mode ~comment
        string_of_var logger logger_buffer logger_err (Ode_loggers_sig.Init x)
    else
      Ode_loggers.increment ~init_mode ~comment string_of_var logger logger_err
        (Ode_loggers_sig.Init x)

  let affect_var ~propagate_constants is_zero ?(init_mode = false) logger
      logger_buffer logger_err compil network decl =
    let handler_expr = handler_expr network in
    match decl with
    | Dummy_decl -> ()
    | Init_expr (id', expr, list) ->
      (match list with
      | [] -> ()
      | [ a ] ->
        let species, n = species_of_species_id network a in
        let expr = to_var compil (from_nocc compil expr n) n in
        let comment =
          Format.asprintf "%a"
            (fun log -> I.print_chemical_species ~compil log)
            species
        in
        increment ~propagate_constants is_zero ~init_mode ~comment
          (string_of_var_id ~compil ~init_mode logger)
          logger logger_buffer logger_err a expr handler_expr
      | _ ->
        let () =
          Ode_loggers.associate ~propagate_constants ~init_mode
            (string_of_var_id ~compil ~init_mode logger)
            logger logger_buffer logger_err (Ode_loggers_sig.Expr id') expr
            handler_expr
        in
        List.iter
          (fun id ->
            let n = snd (species_of_species_id network id) in
            let expr =
              to_var compil
                (from_nocc compil
                   (Loc.annot_with_dummy (Alg_expr.ALG_VAR id'))
                   n)
                n
            in
            increment ~propagate_constants is_zero
              (string_of_var_id ~compil ~init_mode logger)
              logger logger_buffer logger_err ~init_mode id expr handler_init)
          list)
    | Var (id, comment, expr) ->
      let expr =
        if Sbml_backend.is_dotnet logger then
          Sbml_backend.propagate_dangerous_var_names_in_alg_expr logger
            handler_expr expr
        else
          expr
      in
      Ode_loggers.associate ?comment ~propagate_constants ~init_mode
        (string_of_var_id ~compil ~init_mode logger)
        logger logger_buffer logger_err (Ode_loggers_sig.Expr id) expr
        handler_expr

  let get_dep ?time_var dep_map expr _network =
    Alg_expr_extra.dep ?time_var
      (Mods.IntSet.empty, Mods.IntSet.empty)
      (fun a (b, c) -> Mods.IntSet.add a b, c)
      (fun a (b, c) -> b, Mods.IntSet.add a c)
      (fun (a, b) (c, d) -> Mods.IntSet.union a c, Mods.IntSet.union b d)
      (fun id ->
        match Mods.IntMap.find_option (succ id) dep_map with
        | Some set -> set
        | None -> Mods.IntSet.empty, Mods.IntSet.empty)
      expr

  module V = struct
    type t = Ode_loggers_sig.variable

    let compare = compare
    let print _ _ = ()
  end

  module VSetMap = SetMap.Make (V)
  module VMAP = VSetMap.Map

  let affect_deriv_var ~propagate_constants _is_zero logger logger_buffer
      logger_err compil network decl dep =
    let time_var = build_time_var network in
    let handler_expr = handler_expr network in
    match decl with
    | Dummy_decl | Init_expr _ -> dep
    | Var (id, _comment, expr) ->
      let dep_var = get_dep ?time_var dep expr network in
      let dep = Mods.IntMap.add id dep_var dep in
      let () =
        Mods.IntSet.iter
          (fun dt ->
            let expr =
              Alg_expr_extra.simplify
                (Alg_expr_extra.diff_mixture ?time_var expr dt)
            in
            Ode_loggers.associate ~propagate_constants ~init_mode:false
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err
              (Ode_loggers_sig.variable_of_derived_variable
                 (Ode_loggers_sig.Expr id) dt)
              expr handler_expr)
          (fst dep_var)
      in
      let () =
        Mods.IntSet.iter
          (fun dt ->
            let expr =
              Alg_expr_extra.simplify (Alg_expr_extra.diff_token expr dt)
            in
            Ode_loggers.associate ~propagate_constants ~init_mode:false
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err
              (Ode_loggers_sig.variable_of_derived_variable
                 (Ode_loggers_sig.Expr id) dt)
              expr handler_expr)
          (snd dep_var)
      in
      dep

  let affect_deriv_gen ~propagate_constants dep_var logger logger_buffer
      logger_err compil var rate network dep =
    let time_var = build_time_var network in
    let handler_expr = handler_expr network in
    let dep_set = get_dep ?time_var dep_var rate network in
    let dep = VMAP.add var dep_set dep in
    let () =
      Mods.IntSet.iter
        (fun dt ->
          let expr =
            Alg_expr_extra.simplify
              (Alg_expr_extra.diff_mixture ?time_var rate dt)
          in
          Ode_loggers.associate ~propagate_constants ~init_mode:false
            (string_of_var_id ~compil logger)
            logger logger_buffer logger_err
            (Ode_loggers_sig.variable_of_derived_variable var dt)
            expr handler_expr)
        (fst dep_set)
    in
    let () =
      Mods.IntSet.iter
        (fun dt ->
          let expr =
            Alg_expr_extra.simplify (Alg_expr_extra.diff_token rate dt)
          in
          Ode_loggers.associate ~propagate_constants ~init_mode:false
            (string_of_var_id ~compil logger)
            logger logger_buffer logger_err
            (Ode_loggers_sig.variable_of_derived_variable var dt)
            expr handler_expr)
        (snd dep_set)
    in
    dep

  let affect_deriv_rate dep_var logger logger_buffer logger_err compil rule rate
      network dep =
    affect_deriv_gen dep_var logger logger_buffer logger_err compil
      (var_of_rule rule) rate network dep

  let affect_deriv_stoch dep_var logger logger_buffer logger_err compil rule n
      coef network dep =
    affect_deriv_gen dep_var logger logger_buffer logger_err compil
      (var_of_stoch rule n) coef network dep

  let fresh_is_zero network =
    let is_zero = Mods.DynArray.create (get_fresh_ode_var_id network) true in
    let is_zero x =
      if Mods.DynArray.get is_zero x then (
        let () = Mods.DynArray.set is_zero x false in
        true
      ) else
        false
    in
    is_zero

  let declare_rates_global_gen g logger network =
    let do_it f = Ode_loggers.declare_global logger (g (f network.n_rules) 1) in
    let () = do_it (fun x -> Ode_loggers_sig.Rate x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rated x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rateun x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rateund x) in
    let () =
      do_it (fun x ->
          Ode_loggers_sig.Stochiometric_coef (x, network.max_stoch_coef))
    in
    let () =
      match Ode_loggers_sig.get_encoding_format logger with
      | Loggers.Octave | Loggers.Matlab -> Ode_loggers.print_newline logger
      | Loggers.Mathematica | Loggers.GEPHI | Loggers.Maple | Loggers.SBML
      | Loggers.DOTNET | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS
      | Loggers.Matrix | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
      | Loggers.Js_Graph | Loggers.HTML_Tabular | Loggers.Json ->
        ()
    in
    ()

  let declare_rates_global logger network =
    declare_rates_global_gen (fun a _ -> a) logger network

  let declare_jacobian_rates_global logger network =
    declare_rates_global_gen Ode_loggers_sig.variable_of_derived_variable logger
      network

  let breakline = true

  let good_step ~step logger =
    match Ode_loggers_sig.get_encoding_format logger with
    | Loggers.Mathematica | Loggers.Maple -> step = 2
    | Loggers.Matlab | Loggers.Octave | Loggers.Matrix | Loggers.HTML_Graph
    | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT
    | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS | Loggers.SBML
    | Loggers.DOTNET | Loggers.GEPHI | Loggers.Json ->
      step = 1

  let is_step_one ~step = step = 1
  let is_step_two ~step = step = 2

  let export_main_gen ~propagate_constants ~initial_step ~max_step ~reltol
      ~abstol ~nonnegative ~step ~compute_jacobian ~command_line
      ~command_line_quotes ?(data_file = "data.csv") ?(init_t = 0.) ~max_t
      ?(plot_period = 1.) logger logger_buffer (logger_err : Loggers.t) compil
      network split =
    let is_zero = fresh_is_zero network in
    let nodevar = get_last_ode_var_id network in
    let nobs = get_fresh_obs_id network in
    let handler_expr = handler_expr network in
    let () =
      if good_step ~step logger then
        Ode_loggers.open_procedure logger "main" "main" []
    in
    let command_line_closure logger =
      let () = Ode_loggers.print_comment ~breakline logger "command line: " in
      let () =
        Ode_loggers.print_comment ~breakline logger
          ("     " ^ command_line_quotes)
      in
      ()
    in
    let count = I.what_do_we_count compil in
    let rule_rate_convention = I.rule_rate_convention compil in
    let reaction_rate_convention = I.reaction_rate_convention compil in
    let may_be_not_time_homogeneous = may_be_not_time_homogeneous network in
    (*---------------------------------------------------------------*)
    let () =
      if is_step_one ~step then (
        let () =
          Ode_loggers.print_ode_preamble ~may_be_not_time_homogeneous ~count
            ~rule_rate_convention ?reaction_rate_convention logger
            command_line_closure ()
        in
        let () = Ode_loggers.print_newline logger in
        (*---------------------------------------------------------------*)
        let () =
          Sbml_backend.open_box logger_buffer logger_err "listOfParameters"
        in
        let () = Sbml_backend.line_dotnet_or_sbml logger_buffer logger_err in
        let () =
          let () =
            if propagate_constants then
              ()
            else
              Sbml_backend.open_box_dotnet logger_buffer logger_err
                "begin parameters"
          in
          let () = Sbml_backend.line_dotnet logger_buffer logger_err in
          (*---------------------------------------------------------------*)
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.Tinit
              (Alg_expr.float init_t) handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.Tend
              (Alg_expr.float max_t) handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.InitialStep
              (Alg_expr.float initial_step)
              handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.MaxStep
              (Alg_expr.float max_step) handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.RelTol
              (Alg_expr.float reltol) handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.AbsTol
              (Alg_expr.float abstol) handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.Period_t_points
              (Alg_expr.float plot_period)
              handler_expr
          in
          let () =
            Ode_loggers.associate_nonnegative logger_buffer nonnegative
          in
          let () = Ode_loggers.print_newline logger_buffer in
          let () =
            Ode_loggers.declare_global logger_buffer Ode_loggers_sig.N_ode_var
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.N_ode_var
              (Alg_expr.int nodevar) handler_expr
          in
          let () =
            Ode_loggers.declare_global logger_buffer
              Ode_loggers_sig.N_max_stoc_coef
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.N_max_stoc_coef
              (Alg_expr.int network.max_stoch_coef)
              handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.N_var
              (Alg_expr.int (get_last_var_id network))
              handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.N_obs
              (Alg_expr.int network.n_obs)
              handler_expr
          in
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger_buffer logger_err Ode_loggers_sig.N_rules
              (Alg_expr.int network.n_rules)
              handler_expr
          in
          let () = Ode_loggers.print_newline logger_buffer in
          let () =
            Ode_loggers.declare_global logger_buffer
              (Ode_loggers_sig.Expr network.fresh_var_id)
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Expr network.fresh_var_id)
          in
          let () =
            Ode_loggers.declare_global logger_buffer
              (Ode_loggers_sig.Init network.fresh_ode_var_id)
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Init network.fresh_ode_var_id)
          in
          let () = declare_rates_global logger_buffer network in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Stochiometric_coef
                 (network.n_rules, network.max_stoch_coef))
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Rate network.n_rules)
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Rated network.n_rules)
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Rateun network.n_rules)
          in
          let () =
            Ode_loggers.initialize ~nodevar logger_buffer
              (Ode_loggers_sig.Rateund network.n_rules)
          in
          (*---------------------------------------------------------------*)
          let () =
            if compute_jacobian then (
              let () =
                Ode_loggers.declare_global logger_buffer
                  (Ode_loggers_sig.Jacobian_var
                     (network.fresh_ode_var_id, network.fresh_ode_var_id))
              in
              let () =
                Ode_loggers.initialize ~nodevar logger_buffer
                  (Ode_loggers_sig.Jacobian_var
                     (network.fresh_ode_var_id, network.fresh_ode_var_id))
              in
              let () = declare_jacobian_rates_global logger_buffer network in
              let () =
                Ode_loggers.initialize ~nodevar logger_buffer
                  (Ode_loggers_sig.Jacobian_rate
                     (network.n_rules, network.fresh_ode_var_id))
              in
              let () =
                Ode_loggers.initialize ~nodevar logger_buffer
                  (Ode_loggers_sig.Jacobian_rated
                     (network.n_rules, network.fresh_ode_var_id))
              in
              let () =
                Ode_loggers.initialize ~nodevar logger_buffer
                  (Ode_loggers_sig.Jacobian_rateun
                     (network.n_rules, network.fresh_ode_var_id))
              in
              let () =
                Ode_loggers.initialize ~nodevar logger_buffer
                  (Ode_loggers_sig.Jacobian_rateund
                     (network.n_rules, network.fresh_ode_var_id))
              in
              ()
            )
          in
          (*---------------------------------------------------------------*)
          let () = Ode_loggers.print_newline logger_buffer in
          let () = Ode_loggers.start_time logger_buffer init_t in
          let () = Ode_loggers.print_newline logger_buffer in
          (*---------------------------------------------------------------*)
          let () =
            if may_be_not_time_homogeneous then (
              match Ode_loggers_sig.get_encoding_format logger with
              | Loggers.SBML | Loggers.Octave | Loggers.Matlab | Loggers.DOTNET
                ->
                let () =
                  Ode_loggers.associate ~propagate_constants
                    (string_of_var_id ~compil logger)
                    logger logger_buffer logger_err
                    (Ode_loggers_sig.Init (get_last_ode_var_id network))
                    (Loc.annot_with_dummy
                       (Alg_expr.STATE_ALG_OP Operator.TIME_VAR))
                    handler_init
                in
                Sbml_backend.print_parameters
                  (fun x -> string_of_var_id x)
                  logger logger_buffer logger_err
                  Ode_loggers_sig.Time_scale_factor Nbr.one;
                Sbml_backend.line_dotnet logger logger_err
              | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
              | Loggers.HTML | Loggers.GEPHI | Loggers.HTML_Tabular
              | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS
              | Loggers.Maple | Loggers.Mathematica | Loggers.Json ->
                ()
            ) else
              ()
          in
          (*---------------------------------------------------------------*)
          let () =
            List.iter
              (affect_var ~propagate_constants is_zero logger logger_buffer
                 logger_err ~init_mode:true compil network)
              network.var_declaration
          in
          let () = Ode_loggers.print_newline logger_buffer in
          let () =
            List.iter
              (fun (rule, coefs) ->
                List.iter
                  (fun coef ->
                    match coef with
                    | R rate ->
                      Ode_loggers.associate ~propagate_constants
                        (string_of_var_id ~compil logger)
                        ~comment:rule.comment logger logger_buffer logger_err
                        (var_of_rate rule.rule_id_with_mode)
                        rate handler_expr
                    | S (n, stoc) ->
                      Ode_loggers.associate ~propagate_constants
                        (string_of_var_id ~compil logger)
                        logger logger_buffer logger_err
                        (Ode_loggers_sig.Stochiometric_coef
                           ( (let a, _, _ = rule.rule_id_with_mode in
                              a),
                             n ))
                        stoc handler_expr)
                  coefs)
              split.const_rate
          in
          let () =
            Sbml_backend.do_dotnet logger logger_err (fun logger logger_err ->
                List.iter
                  (fun (rule, coefs) ->
                    List.iter
                      (fun coef ->
                        match coef with
                        | R rate ->
                          (match
                             Sbml_backend.eval_const_alg_expr logger
                               handler_expr rate
                           with
                          | Some _ ->
                            (* if not propagate_constants then*)
                            Ode_loggers.associate ~propagate_constants
                              (string_of_var_id ~compil logger)
                              ~comment:rule.comment logger logger_buffer
                              logger_err
                              (var_of_rate rule.rule_id_with_mode)
                              rate handler_expr
                          | None ->
                            let () =
                              Sbml_backend.warn_expr rate
                                "DOTNET backend does not support non-constant \
                                 rates for rules: cowardly replacing it with 1"
                                logger logger_err
                            in
                            Ode_loggers.associate ~propagate_constants
                              (string_of_var_id ~compil logger)
                              ~comment:rule.comment logger logger_buffer
                              logger_err
                              (var_of_rate rule.rule_id_with_mode)
                              (Alg_expr.CONST Nbr.one, snd rate)
                              handler_expr)
                        | S _ -> ())
                      coefs)
                  split.var_rate)
          in
          (*---------------------------------------------------------------*)
          let () =
            Sbml_backend.close_box logger_buffer logger_err "listOfParameters"
          in
          let () =
            if propagate_constants then
              ()
            else
              Sbml_backend.close_box_dotnet logger_buffer logger_err
                "end parameters"
          in
          ()
        in

        (*---------------------------------------------------------------*)
        let () = Ode_loggers.print_newline logger in
        let () = Ode_loggers.print_license_check logger in
        let () = Ode_loggers.print_newline logger in
        let pos i =
          match Mods.DynArray.get network.ode_vars_tab i with
          | Nembed _ | Noccurrences _ -> true
          | Token _ | Dummy -> false
        in
        let nodevar = network.fresh_ode_var_id - 1 in
        let () =
          Ode_loggers.print_options ~compute_jacobian ~pos ~nodevar logger
        in
        let () = Ode_loggers.print_newline logger in
        ()
      )
    in
    (*---------------------------------------------------------------*)
    (*Get titles*)
    let titles = I.get_obs_titles compil in
    let () =
      if good_step ~step logger then (
        let () = Ode_loggers.print_integrate ~nobs ~nodevar logger in
        let () = Ode_loggers.print_newline logger in
        let () = Ode_loggers.associate_nrows logger in
        let () = Ode_loggers.initialize ~nodevar logger Ode_loggers_sig.Tmp in
        let () = Ode_loggers.print_newline logger in
        let () = Ode_loggers.print_interpolate logger in
        let () = Ode_loggers.print_newline logger in
        let () =
          Ode_loggers.print_dump_plots ~nobs ~data_file ~command_line ~titles
            logger
        in
        let () = Ode_loggers.print_newline logger in
        ()
      )
    in
    (*---------------------------------------------------------------*)
    let () =
      if is_step_one ~step then (
        let () = Ode_loggers.close_procedure logger in
        let () = Ode_loggers.print_newline logger in
        let () = Ode_loggers.print_newline logger in
        ()
      )
    in
    (*---------------------------------------------------------------*)
    let () = Ode_loggers.print_newline logger in
    ()

  let export_main = export_main_gen ~step:1
  let export_main_follow_up = export_main_gen ~step:2

  let export_dydt ~propagate_constants ~show_time_advance logger logger_err
      compil network split =
    let nodevar = get_last_ode_var_id network in
    let is_zero = fresh_is_zero network in
    let label = "listOfReactions" in
    let label_dotnet = "begin reactions" in
    let () = Ode_loggers.open_procedure logger "dydt" "ode_aux" [ "t"; "y" ] in
    (*------------------------------------------------*)
    let () = if show_time_advance then Ode_loggers.show_time_advance logger in
    let () = Sbml_backend.open_box logger logger_err label in
    let () = Sbml_backend.line_dotnet_or_sbml logger logger_err in
    let () = Sbml_backend.line_dotnet logger logger_err in
    let () = Sbml_backend.open_box_dotnet logger logger_err label_dotnet in
    let () = Sbml_backend.line_dotnet logger logger_err in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.declare_global logger Ode_loggers_sig.N_ode_var in
    let () =
      Ode_loggers.declare_global logger Ode_loggers_sig.N_max_stoc_coef
    in
    let () = Ode_loggers.declare_global logger (Ode_loggers_sig.Expr 1) in
    let () = declare_rates_global logger network in
    let () =
      if not (Sbml_backend.is_dotnet logger) then (
        let () =
          List.iter
            (affect_var ~propagate_constants is_zero logger logger logger_err
               ~init_mode:false compil network)
            split.var_decl
        in
        let () = Ode_loggers.print_newline logger in
        (*------------------------------------------------*)
        let () =
          List.iter
            (fun (rule, coefs) ->
              List.iter
                (fun coef ->
                  match coef with
                  | R rate ->
                    Ode_loggers.associate ~propagate_constants
                      (string_of_var_id ~compil logger)
                      logger logger logger_err (var_of_rule rule) rate
                      (handler_expr network)
                  | S (n, stoc) ->
                    Ode_loggers.associate ~propagate_constants
                      (string_of_var_id ~compil logger)
                      logger logger logger_err
                      (Ode_loggers_sig.Stochiometric_coef
                         ( (let a, _, _ = rule.rule_id_with_mode in
                            a),
                           n ))
                      stoc (handler_expr network))
                coefs)
            split.var_rate
        in
        let () = Ode_loggers.print_newline logger in
        ()
      )
    in
    let () = Ode_loggers.initialize ~nodevar logger (Ode_loggers_sig.Deriv 1) in
    (*------------------------------------------------*)
    let do_it f l reactants enriched_rule nocc =
      List.iter
        (fun species ->
          let nauto_in_species =
            if I.do_we_count_in_embeddings compil then
              snd (species_of_species_id network species)
            else
              1
          in
          let nauto_in_lhs = enriched_rule.divide_rate_by in
          f logger (Ode_loggers_sig.Deriv species) ~nauto_in_species
            ~nauto_in_lhs ~nocc
            (var_of_rule enriched_rule)
            reactants)
        l
    in
    let () =
      List.iter
        (fun ((reactants, products, token_vector, enriched_rule), nocc) ->
          (*each reaction will be computed here*)
          let add_factor l =
            if I.do_we_count_in_embeddings compil then
              List.rev_map
                (fun x ->
                  let nauto = snd (species_of_species_id network x) in
                  x, nauto)
                (List.rev l)
            else
              List.rev_map (fun x -> x, 1) (List.rev l)
          in
          let reactants' = add_factor reactants in
          let products' = add_factor products in
          let () =
            if I.do_we_prompt_reactions compil then (
              let rule_string =
                Format.asprintf "%a"
                  (I.print_rule_name ~compil)
                  enriched_rule.rule
              in
              let tokens_prod = I.token_vector enriched_rule.rule in
              let dump_token_list fmt list =
                let _ =
                  List.fold_left
                    (fun bool ((alg, _), k) ->
                      let prefix =
                        if bool then
                          ", "
                        else
                          " | "
                      in
                      let () =
                        Format.fprintf fmt "%s%a %a" prefix
                          (Alg_expr.print
                             (fun fmt mixture ->
                               let () = Format.fprintf fmt "|" in
                               let _ =
                                 List.fold_left
                                   (Array.fold_left
                                      (fun bool connected_component ->
                                        let prefix =
                                          if bool then
                                            " , "
                                          else
                                            ""
                                        in
                                        let () =
                                          Format.fprintf fmt "%s%a" prefix
                                            (I.print_connected_component ~compil)
                                            connected_component
                                        in
                                        true))
                                   false mixture
                               in
                               let () = Format.fprintf fmt "|" in
                               ())
                             (I.print_token ~compil)
                             (fun fmt var_id ->
                               Format.fprintf fmt "%s"
                                 (string_of_var_id ~compil logger (succ var_id))))
                          alg (I.print_token ~compil) k
                      in
                      true)
                    false (List.rev list)
                in
                ()
              in
              (*----------------------------------------------*)
              (*print rule in commend format *)
              let () = Ode_loggers.print_newline logger in
              let () =
                Ode_loggers.print_comment ~breakline logger
                  ("rule    : " ^ rule_string)
              in
              let dump fmt list =
                let compil = I.with_dot_and_plus compil in
                let _ =
                  List.fold_left
                    (fun bool k ->
                      let prefix =
                        if bool then
                          Kade_backend.Utils.print_agent_sep_plus
                            (I.symbol_table compil)
                        else
                          Pp.empty
                      in
                      let species_string =
                        Format.asprintf "%a"
                          (fun log id ->
                            I.print_chemical_species ~compil log
                              (fst (Mods.DynArray.get network.species_tab id)))
                          k
                      in
                      let () =
                        Format.fprintf fmt "%t%s" prefix species_string
                      in
                      true)
                    false (List.rev list)
                in
                ()
              in
              (*----------------------------------------------*)
              match Ode_loggers_sig.get_encoding_format logger with
              | Loggers.Matlab | Loggers.Octave | Loggers.SBML
              | Loggers.Mathematica | Loggers.Maple ->
                let s =
                  Format.asprintf "reaction: %a -> %a%a" dump reactants dump
                    products dump_token_list tokens_prod
                in
                Ode_loggers.print_comment ~breakline logger s
              | Loggers.DOTNET ->
                let s =
                  Format.asprintf "%a -> %a%a" dump reactants dump products
                    dump_token_list tokens_prod
                in
                Ode_loggers.print_comment ~breakline logger s
              | Loggers.Matrix | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS
              | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
              | Loggers.Js_Graph | Loggers.GEPHI | Loggers.HTML_Tabular
              | Loggers.Json ->
                ()
            )
          in

          (*------------------------------------------*)
          (*one reaction*)
          let () =
            Sbml_backend.dump_sbml_reaction ~propagate_constants
              Ode_loggers.print_alg_expr_few_parenthesis
              (string_of_var_id ~compil logger)
              get_rule
              (fun a ->
                let a, _, _ = a.rule_id_with_mode in
                a)
              I.print_rule_name (Some compil) logger logger_err
              (handler_expr network) reactants' products' token_vector
              enriched_rule
              (var_of_rule enriched_rule)
              enriched_rule.divide_rate_by nocc network.fictitious_species
          in
          let () =
            match Ode_loggers_sig.formatter_of_logger logger with
            | None -> ()
            | Some fmt ->
              let () = Loggers.flush_and_clean logger_err fmt in
              ()
          in
          let reactants' =
            List.rev_map
              (fun x ->
                let nauto = snd (species_of_species_id network x) in
                Ode_loggers_sig.Concentration x, to_nocc_correct compil nauto)
              (List.rev reactants)
          in
          let nauto_in_lhs = enriched_rule.divide_rate_by in
          let () = Ode_loggers.print_newline logger in
          (*------------------------------------------*)
          let () =
            do_it Ode_loggers.consume reactants reactants' enriched_rule nocc
          in
          let () =
            do_it Ode_loggers.produce products reactants' enriched_rule nocc
          in
          (*------------------------------------------*)
          let _ =
            List.fold_left
              (fun n (token, _) ->
                let () =
                  Ode_loggers.update_token logger (Ode_loggers_sig.Deriv token)
                    ~nauto_in_lhs ~nocc
                    (var_of_rule enriched_rule)
                    (var_of_stoch enriched_rule n)
                    reactants'
                in
                n + 1)
              1 token_vector
          in
          ())
        network.reactions
    in
    (*------------------------------------------------------------*)
    (* Derivative of time is equal to 1 *)
    let network =
      if may_be_not_time_homogeneous network then
        if Sbml_backend.is_dotnet logger then (
          let s =
            "DOTNET backend does not support time dependent expressions\n"
          in
          let () = Printf.printf "%s" s in
          let () = Sbml_backend.print_comment logger logger_err s in
          network
        ) else (
          let () =
            Ode_loggers.associate ~propagate_constants
              (string_of_var_id ~compil logger)
              logger logger logger_err
              (Ode_loggers_sig.Deriv (get_last_ode_var_id network))
              (Alg_expr.const Nbr.one) (handler_expr network)
          in
          let network = { network with has_time_reaction = Some true } in
          let () = Ode_loggers.print_newline logger in
          let () = Sbml_backend.time_advance logger logger_err in
          network
        )
      else
        network
    in
    let () = Ode_loggers.close_procedure logger in
    let () = Sbml_backend.close_box logger logger_err label in
    let label_close = "end reactions" in
    let () = Sbml_backend.close_box_dotnet logger logger_err label_close in
    let () =
      match Ode_loggers_sig.formatter_of_logger logger with
      | None -> ()
      | Some fmt -> Loggers.flush_and_clean logger_err fmt
    in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    network

  let export_jac ~propagate_constants logger logger_err compil network split =
    let nodevar = get_last_ode_var_id network in
    match Ode_loggers_sig.get_encoding_format logger with
    | Loggers.Matrix | Loggers.TXT | Loggers.Maple | Loggers.Mathematica
    | Loggers.GEPHI | Loggers.TXT_Tabular | Loggers.XLS | Loggers.DOT
    | Loggers.HTML | Loggers.HTML_Graph | Loggers.Js_Graph
    | Loggers.HTML_Tabular | Loggers.Json | Loggers.SBML | Loggers.DOTNET ->
      ()
    | Loggers.Matlab | Loggers.Octave ->
      let is_zero = fresh_is_zero network in
      let label = "listOfReactions" in
      let () =
        Ode_loggers.open_procedure logger "jac" "ode_jacobian" [ "t"; "y" ]
      in
      let () = Ode_loggers.print_newline logger in
      let () = Ode_loggers.declare_global logger Ode_loggers_sig.N_ode_var in
      let () =
        Ode_loggers.declare_global logger Ode_loggers_sig.N_max_stoc_coef
      in
      let () =
        Ode_loggers.declare_global logger (Ode_loggers_sig.Jacobian_var (1, 1))
      in
      let () = Ode_loggers.declare_global logger (Ode_loggers_sig.Expr 1) in
      let () = declare_rates_global logger network in
      let () = declare_jacobian_rates_global logger network in
      let () =
        List.iter
          (affect_var ~propagate_constants is_zero logger logger logger_err
             ~init_mode:false compil network)
          split.var_decl
      in
      let () = Ode_loggers.print_newline logger in
      let () =
        List.iter
          (fun (rule, coef_list) ->
            List.iter
              (fun coef ->
                match coef with
                | R rate ->
                  Ode_loggers.associate ~propagate_constants
                    (string_of_var_id ~compil logger)
                    logger logger logger_err (var_of_rule rule) rate
                    (handler_expr network)
                | S (n, rate) ->
                  Ode_loggers.associate ~propagate_constants
                    (string_of_var_id ~compil logger)
                    logger logger logger_err (var_of_stoch rule n) rate
                    (handler_expr network))
              coef_list)
          split.var_rate
      in
      let dep_var =
        List.fold_left
          (fun dep var ->
            affect_deriv_var ~propagate_constants is_zero logger logger
              logger_err compil network var dep)
          Mods.IntMap.empty split.var_decl
      in
      let () = Ode_loggers.print_newline logger in
      let dep_rates =
        List.fold_left
          (fun dep (rule, coef_list) ->
            List.fold_left
              (fun dep coef ->
                match coef with
                | R rate ->
                  affect_deriv_rate ~propagate_constants dep_var logger logger
                    logger_err compil rule rate network dep
                | S (n, rate) ->
                  affect_deriv_stoch ~propagate_constants dep_var logger logger
                    logger_err compil rule n rate network dep)
              dep coef_list)
          VMAP.empty split.var_rate
      in
      let () = Ode_loggers.print_newline logger in
      let do_it f l dep_set reactants enriched_rule nocc =
        List.iter
          (fun species ->
            let nauto_in_species =
              if I.do_we_count_in_embeddings compil then
                snd (species_of_species_id network species)
              else
                1
            in
            let nauto_in_lhs = enriched_rule.divide_rate_by in
            f logger (Ode_loggers_sig.Concentration species) ~nauto_in_species
              ~nauto_in_lhs ~nocc
              (var_of_rule enriched_rule)
              reactants dep_set)
          l
      in
      let () =
        Ode_loggers.initialize ~nodevar logger
          (Ode_loggers_sig.Jacobian
             (network.fresh_ode_var_id, network.fresh_ode_var_id))
      in
      let () =
        List.iter
          (fun ((reactants, products, token_vector, enriched_rule), nocc) ->
            let dep_set =
              match VMAP.find_option (var_of_rule enriched_rule) dep_rates with
              | Some set -> set
              | None -> Mods.IntSet.empty, Mods.IntSet.empty
            in
            let dep_set_rate = Mods.IntSet.union (fst dep_set) (snd dep_set) in
            let () =
              if I.do_we_prompt_reactions compil then (
                let rule_string =
                  Format.asprintf "%a"
                    (I.print_rule_name ~compil)
                    enriched_rule.rule
                in
                let tokens_prod = I.token_vector enriched_rule.rule in
                let dump_token_list fmt list =
                  let _ =
                    List.fold_left
                      (fun bool ((alg, _), k) ->
                        let prefix =
                          if bool then
                            ", "
                          else
                            " | "
                        in
                        let () =
                          Format.fprintf fmt "%s%a %a" prefix
                            (Alg_expr.print
                               (fun fmt mixture ->
                                 let () = Format.fprintf fmt "|" in
                                 let _ =
                                   List.fold_left
                                     (Array.fold_left
                                        (fun bool connected_component ->
                                          let prefix =
                                            if bool then
                                              " , "
                                            else
                                              ""
                                          in
                                          let () =
                                            Format.fprintf fmt "%s%a" prefix
                                              (I.print_connected_component
                                                 ~compil)
                                              connected_component
                                          in
                                          true))
                                     false mixture
                                 in
                                 let () = Format.fprintf fmt "|" in
                                 ())
                               (I.print_token ~compil)
                               (fun fmt var_id ->
                                 Format.fprintf fmt "%s"
                                   (string_of_var_id ~compil logger
                                      (succ var_id))))
                            alg (I.print_token ~compil) k
                        in
                        true)
                      false (List.rev list)
                  in
                  ()
                in
                (*--------------------------------------------------*)
                (*print rule in a comment format*)
                let () = Ode_loggers.print_newline logger in
                let () =
                  Ode_loggers.print_comment ~breakline logger
                    ("rule    : " ^ rule_string)
                in
                let dump fmt list =
                  let compil = I.with_dot_and_plus compil in
                  let _ =
                    List.fold_left
                      (fun bool k ->
                        let prefix =
                          if bool then
                            Kade_backend.Utils.print_agent_sep_plus
                              (I.symbol_table compil)
                          else
                            Pp.empty
                        in
                        let species_string =
                          Format.asprintf "%a"
                            (fun log id ->
                              I.print_chemical_species ~compil log
                                (fst (Mods.DynArray.get network.species_tab id)))
                            k
                        in
                        let () =
                          Format.fprintf fmt "%t%s" prefix species_string
                        in
                        true)
                      false (List.rev list)
                  in
                  ()
                in
                let s =
                  Format.asprintf "reaction: %a -> %a%a" dump reactants dump
                    products dump_token_list tokens_prod
                in
                let () = Ode_loggers.print_comment ~breakline logger s in
                ()
              )
            in
            (*--------------------------------------------------*)
            let reactants' =
              List.rev_map
                (fun x ->
                  let nauto = snd (species_of_species_id network x) in
                  x, to_nocc_correct compil nauto)
                (List.rev reactants)
            in
            let nauto_in_lhs = enriched_rule.divide_rate_by in
            let () = Ode_loggers.print_newline logger in
            let () =
              do_it Ode_loggers.consume_jac reactants dep_set_rate reactants'
                enriched_rule nocc
            in
            let () =
              do_it Ode_loggers.produce_jac products dep_set_rate reactants'
                enriched_rule nocc
            in
            let _ =
              List.fold_left
                (fun n (token, _loc) ->
                  let dep_set_token_expr =
                    match
                      VMAP.find_option (var_of_stoch enriched_rule n) dep_rates
                    with
                    | Some set -> set
                    | None -> Mods.IntSet.empty, Mods.IntSet.empty
                  in
                  let () =
                    Ode_loggers.update_token_jac logger
                      (Ode_loggers_sig.Concentration token) ~nauto_in_lhs ~nocc
                      (var_of_rule enriched_rule)
                      (var_of_stoch enriched_rule n)
                      reactants' dep_set_rate
                      ~dep_mixture:(fst dep_set_token_expr)
                      ~dep_token:(snd dep_set_token_expr)
                  in
                  n + 1)
                1 token_vector
            in
            ())
          network.reactions
      in
      (* Derivative of time is equal to 1 *)
      let () = Ode_loggers.close_procedure logger in
      let () = Sbml_backend.close_box logger logger_err label in
      let () = Ode_loggers.print_newline logger in
      let () = Ode_loggers.print_newline logger in
      ()

  let export_init logger logger_err compil network =
    let nodevar = get_last_ode_var_id network in
    let label = "listOfSpecies" in
    let label_dotnet = "begin species" in
    let () = Sbml_backend.open_box logger logger_err label in
    let () = Sbml_backend.line_dotnet_or_sbml logger logger_err in
    let () = Sbml_backend.open_box_dotnet logger logger_err label_dotnet in
    let () = Sbml_backend.line_dotnet logger logger_err in
    let () = Ode_loggers.open_procedure logger "Init" "ode_init" [] in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.declare_global logger Ode_loggers_sig.N_ode_var in
    let () =
      Ode_loggers.declare_global logger
        (Ode_loggers_sig.Init (get_last_ode_var_id network))
    in
    let () =
      Ode_loggers.initialize ~nodevar logger
        (Ode_loggers_sig.Initbis (get_last_ode_var_id network))
    in
    let () = Ode_loggers.print_newline logger in
    let rec aux k =
      if
        k >= get_fresh_ode_var_id network
        || k = get_fresh_ode_var_id network - 1
           && Sbml_backend.is_dotnet logger
           && may_be_not_time_homogeneous network
      then
        ()
      else (
        let id, comment, units =
          if
            may_be_not_time_homogeneous network
            && k = get_fresh_ode_var_id network - 1
          then
            "time", "t", Some "substance"
          else (
            match network.fictitious_species with
            | Some id when id = k -> string_of_int k, "I()", Some ""
            | Some _ | None ->
              let variable = Mods.DynArray.get network.ode_vars_tab k in
              (match variable with
              | Dummy -> "dummy", "", None
              | Token id ->
                (match Ode_loggers_sig.get_encoding_format logger with
                | Loggers.SBML ->
                  ( "t" ^ string_of_int k,
                    Format.asprintf "%a"
                      (fun log id -> I.print_token ~compil log id)
                      id,
                    Some "substance" )
                | Loggers.DOTNET ->
                  ( string_of_int k,
                    Format.asprintf "%a"
                      (fun log id -> I.print_token ~compil log id)
                      id,
                    Some "substance" )
                | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
                | Loggers.GEPHI | Loggers.Js_Graph | Loggers.HTML_Tabular
                | Loggers.Json | Loggers.Maple | Loggers.Mathematica
                | Loggers.Matlab | Loggers.Matrix | Loggers.Octave | Loggers.TXT
                | Loggers.TXT_Tabular | Loggers.XLS ->
                  ( "",
                    Format.asprintf "%a"
                      (fun log k ->
                        I.print_chemical_species ~compil log
                          (fst (Mods.DynArray.get network.species_tab k)))
                      k,
                    None ))
              | Nembed _ | Noccurrences _ ->
                (match Ode_loggers_sig.get_encoding_format logger with
                | Loggers.SBML ->
                  ( "s" ^ string_of_int k,
                    Format.asprintf "%a"
                      (fun log k ->
                        I.print_chemical_species ~compil log
                          (fst (Mods.DynArray.get network.species_tab k)))
                      k,
                    Some "substance" )
                | Loggers.DOTNET ->
                  let compil = I.to_dotnet compil in
                  ( string_of_int k,
                    Format.asprintf "%a"
                      (fun log k ->
                        I.print_chemical_species ~compil log
                          (fst (Mods.DynArray.get network.species_tab k)))
                      k,
                    Some "" )
                | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
                | Loggers.HTML_Tabular | Loggers.Json | Loggers.Js_Graph
                | Loggers.Maple | Loggers.GEPHI | Loggers.Mathematica
                | Loggers.Matlab | Loggers.Matrix | Loggers.Octave | Loggers.TXT
                | Loggers.TXT_Tabular | Loggers.XLS ->
                  ( "",
                    Format.asprintf "%a"
                      (fun log k ->
                        I.print_chemical_species ~compil log
                          (fst (Mods.DynArray.get network.species_tab k)))
                      k,
                    None )))
          )
        in
        let () = Ode_loggers.declare_init ~comment logger k in
        let () =
          Sbml_backend.dump_initial_species ?units logger logger_err
            (handler_expr network) k comment id
        in
        aux (next_id k)
      )
    in
    let () = aux fst_id in
    let () = Ode_loggers.close_procedure logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    let () = Sbml_backend.close_box logger logger_err label in
    let label_close = "end species" in
    let () = Sbml_backend.close_box_dotnet logger logger_err label_close in
    ()

  let export_obs ~propagate_constants logger logger_err compil network split =
    let nodevar = get_last_ode_var_id network in
    let is_zero = fresh_is_zero network in
    let () = Ode_loggers.open_procedure logger "obs" "ode_obs" [ "y" ] in
    (* add t *)
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.declare_global logger Ode_loggers_sig.N_obs in
    let () = Ode_loggers.declare_global logger (Ode_loggers_sig.Expr 1) in
    let () =
      Ode_loggers.initialize ~nodevar logger (Ode_loggers_sig.Obs network.n_obs)
    in
    let () = Ode_loggers.print_newline logger in
    let () =
      if obs_may_be_not_time_homogeneous network then
        Ode_loggers.associate_t logger (get_last_ode_var_id network)
      else
        ()
    in
    let titles = I.get_obs_titles compil in
    let () =
      match Ode_loggers_sig.get_encoding_format logger with
      | Loggers.DOTNET | Loggers.Matlab | Loggers.Octave ->
        List.iter
          (affect_var ~propagate_constants is_zero logger logger logger_err
             ~init_mode:false compil network)
          split.var_decl
      | Loggers.Mathematica | Loggers.Maple | Loggers.Matrix
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
      | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS | Loggers.SBML | Loggers.Json | Loggers.GEPHI ->
        ()
    in
    let () =
      if
        Sbml_backend.is_dotnet logger
        && List.for_all
             (fun (_id, expr) -> Ode_loggers.is_time expr)
             network.obs
      then
        (* No observable in the model *)
        ()
      else (
        let () =
          Sbml_backend.do_dotnet logger logger_err (fun log _ ->
              Ode_loggers.print_newline log)
        in
        let () =
          Sbml_backend.open_box_dotnet logger logger_err "begin groups"
        in
        let () =
          Sbml_backend.do_dotnet logger logger_err (fun log _ ->
              Ode_loggers.print_newline log)
        in
        let () = Loggers.print_newline logger_err in
        let titles =
          List.fold_left
            (fun titles (id, expr) ->
              match titles with
              | comment :: tail ->
                let () =
                  Ode_loggers.associate ~comment ~propagate_constants
                    (string_of_var_id ~compil logger)
                    logger logger logger_err (Ode_loggers_sig.Obs id) expr
                    (handler_expr network)
                in
                tail
              | [] ->
                let () =
                  Loggers.fprintf logger_err
                    "Internal error, more obs than obs labels"
                in
                titles)
            titles network.obs
        in
        let () =
          if not (titles = []) then
            Loggers.fprintf logger_err
              "Internal error, less obs than obs labels"
        in
        let () =
          if Sbml_backend.is_dotnet logger then
            Sbml_backend.close_box_dotnet logger logger_err "end groups"
        in
        let () = Ode_loggers.print_newline logger in
        ()
      )
    in
    let () = Ode_loggers.close_procedure logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    ()

  let export_network ~command_line ~command_line_quotes ?data_file ?init_t
      ~max_t ?plot_period ?(compute_jacobian = true)
      ?(propagate_constants = false) ?(show_time_advance = false)
      ?(nonnegative = false) ?(initial_step = 0.000001) ?(max_step = 0.02)
      ?(abstol = 0.001) ?(reltol = 0.001) parameters logger logger_buffer
      (logger_err : Loggers.t) compil network =
    let network =
      if may_be_not_time_homogeneous network then
        (* add a spurious variable for time *)
        inc_fresh_ode_var_id network
      else
        network
    in
    let network, sorted_rules_and_decl =
      split_rules_and_decl parameters compil network
    in
    let () = Format.printf "+ exporting the network... @." in
    let () = Format.printf "\t -main function @." in
    let () =
      export_main ~propagate_constants ~compute_jacobian ~command_line
        ~command_line_quotes ?data_file ?init_t ~max_t ~nonnegative
        ~initial_step ~max_step ~abstol ~reltol ?plot_period logger
        logger_buffer logger_err compil network sorted_rules_and_decl
    in
    let () = Format.printf "\t -initial state @." in
    let () = export_init logger logger_err compil network in
    let () =
      match Ode_loggers_sig.formatter_of_logger logger with
      | None -> ()
      | Some fmt ->
        let () = Ode_loggers_sig.flush_buffer logger_buffer fmt in
        Loggers.flush_and_clean logger_err fmt
    in
    let () = Format.printf "\t -ode system @." in
    let network =
      export_dydt ~propagate_constants ~show_time_advance logger logger_err
        compil network sorted_rules_and_decl
    in
    let () =
      if compute_jacobian then (
        let () = Format.printf "\t -jacobian @." in
        let () =
          export_jac ~propagate_constants logger logger_err compil network
            sorted_rules_and_decl
        in
        ()
      )
    in
    let () = Format.printf "\t -observables @." in
    let () =
      export_obs ~propagate_constants logger logger_err compil network
        sorted_rules_and_decl
    in
    let () = Ode_loggers.launch_main logger in
    let () =
      export_main_follow_up ~propagate_constants ~nonnegative ~initial_step
        ~max_step ~reltol ~abstol ~compute_jacobian ~command_line
        ~command_line_quotes ?data_file ?init_t ~max_t ?plot_period logger
        logger_buffer logger_err compil network sorted_rules_and_decl
    in
    network

  let get_reactions network =
    let list = get_reactions network in
    List.rev_map (fun ((a, b, c, d), n) -> (a, b, c, d.rule), n) (List.rev list)

  (*********************)
  (*compute symmetries *)
  (*********************)

  let compute_symmetries_from_model parameters compil network contact_map =
    (********************************************************)
    (*initial_states*)
    let network, chemical_species =
      species_of_initial_state compil network (I.get_init compil)
    in
    (********************************************************)
    let cache = network.cache in
    let cache, symmetries =
      I.detect_symmetries parameters compil cache chemical_species contact_map
    in
    let network = { network with cache; symmetries = Some symmetries } in
    network

  let set_to_forward_symmetries_from_model network =
    match network.symmetries with
    | None -> { network with sym_reduction = Symmetries.Ground }
    | Some sym ->
      (match sym.Symmetries.rules_and_alg_expr with
      | None -> { network with sym_reduction = Symmetries.Ground }
      | Some sym -> { network with sym_reduction = Symmetries.Forward sym })

  let set_to_backward_symmetries_from_model network =
    match network.symmetries with
    | None -> { network with sym_reduction = Symmetries.Ground }
    | Some sym ->
      (match sym.Symmetries.rules_and_initial_states with
      | None -> { network with sym_reduction = Symmetries.Ground }
      | Some sym -> { network with sym_reduction = Symmetries.Backward sym })

  let print_symmetries parameters compil network =
    match network.symmetries with
    | None -> ()
    | Some sym -> I.print_symmetries parameters compil sym

  let init_bwd_bisim_info network =
    match network.sym_reduction with
    | Symmetries.Backward red -> Some (I.init_bwd_bisim_info red)
    | Symmetries.Forward _ | Symmetries.Ground -> None

  let _ = is_step_two
  let _ = Init_decl
  let _ = Var_decl ""
  let _ = Init_value Dummy
  let _ = var_id_of_decl
  let _ = direction_of
  let _ = may_be_time_homogeneous_gen
  let _ = is_known_variable
  let _ = last_fresh_obs_id
  let _ = nembed_of_connected_component
  let _ = translate_token
  let _ = convert_one_obs
end
