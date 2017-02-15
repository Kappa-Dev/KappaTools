(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Feb 15 2017>
*)

type contact_map = (int list * (int * int) list) array array

type compil =
  {
    (*contact_map: state list * (agent_name * site) list *)
    contact_map: (*(int list * (int * int) list) array array*) contact_map ;
    environment: Model.t ;
    init: (Alg_expr.t * Primitives.elementary_rule * Locality.t) list ;
    rate_convention: Ode_args.rate_convention ;
    show_reactions: bool ;
    count: Ode_args.count ;
    compute_jacobian: bool
  }

type cache = Pattern.PreEnv.t
type nauto_in_rules_cache = LKappa_auto.cache
type hidden_init = Primitives.elementary_rule
type init = (Alg_expr.t * hidden_init * Locality.t) list

let get_init compil= compil.init

let lift_opt f compil_opt =
  match
    compil_opt
  with
  | None -> None
  | Some a -> Some (f a)

let contact_map compil = compil.contact_map

let environment compil = compil.environment

let domain compil = Model.domain (environment compil)

let domain_opt = lift_opt domain
let environment_opt = lift_opt environment
type mixture = Edges.t(* not necessarily connected, fully specified *)
type chemical_species = Pattern.cc
(* connected, fully specified *)
type canonic_species = chemical_species (* chemical species in canonic form *)
type pattern = Pattern.id array
(* not necessarity connected, maybe partially specified *)
type connected_component = Pattern.id
(* connected, maybe partially specified *)

let dummy_chemical_species compil =
  Pattern.empty_cc (Pattern.Env.signatures (domain compil))

let rate_convention compil = compil.rate_convention
let what_do_we_count compil = compil.count

let do_we_count_in_embeddings compil =
  match
    what_do_we_count compil
  with
  | Ode_args.Occurrences -> false
  | Ode_args.Embeddings -> true

let do_we_prompt_reactions compil =
  compil.show_reactions

let print_chemical_species ?compil f =
  Format.fprintf f "@[<h>%a@]"
    (Pattern.print_cc
       ?sigs:(Tools.option_map Model.signatures (environment_opt compil))
       ?cc_id:None)

let print_token ?compil fmt k =
  Format.fprintf fmt
    "%a"
    (Model.print_token ?env:(environment_opt compil))
    k

let print_canonic_species = print_chemical_species

let nbr_automorphisms_in_chemical_species x =
  List.length (Pattern.automorphisms x)

let compare_connected_component = Pattern.compare_canonicals

let print_connected_component ?compil =
  Pattern.print ?domain:(domain_opt compil) ~with_id:false

let canonic_form x = x

let connected_components_of_patterns = Array.to_list

let connected_components_of_mixture compil cache e =
  let contact_map = contact_map compil in
  let sigs = Pattern.Env.signatures (domain compil) in
  let snap = Edges.build_snapshot sigs e in
  List.fold_left
    (fun (cache,acc) (i,m) ->
       match Snip.connected_components_sum_of_ambiguous_mixture
               contact_map cache (LKappa.of_raw_mixture m) with
       | cache',[[|_,x|],_] ->
         cache',Tools.recti (fun a _ -> x::a) acc i
       | _ -> assert false)
    (cache,[]) snap

type embedding = Renaming.t (* the domain is connected *)
type embedding_forest = Matching.t
(* the domain may be not connected *)

let lift_embedding x =
  Tools.unsome
    Matching.empty
    (Matching.add_cc Matching.empty 0 x)

let find_embeddings compil =
  Pattern.embeddings_to_fully_specified (domain compil)

(*let find_embeddings compil pattern species =
  let () =
    print_connected_component ~compil Format.std_formatter pattern
  in
  let () = Format.fprintf Format.std_formatter " into " in
  let () =
    print_chemical_species ~compil Format.std_formatter species
  in
  let l =
    find_embeddings compil pattern species
  in
  let () = Format.fprintf Format.std_formatter "? embeddings: %i \n" (List.length l) in
  l*)

let find_embeddings_unary_binary compil p x =
  Tools.array_fold_lefti
    (fun i acc cc ->
       let em = find_embeddings compil cc x in
       List_util.map_flatten
         (fun m ->
            List_util.map_option (fun r -> Matching.add_cc m i r) em)
         acc)
    [Matching.empty]
    p

let disjoint_union compil l =
  let sigs = Model.signatures (compil.environment) in
  let pat = Tools.array_map_of_list (fun (x,_,_) -> x) l in
  let _,em,mix =
    List.fold_left
      (fun (i,em,mix) (_,r,cc) ->
         let i = pred i in
         let (mix',r') =
           Pattern.add_fully_specified_to_graph sigs mix cc  in
         let r'' = Renaming.compose false r r' in
         (i,
          Tools.unsome
            Matching.empty
            (Matching.add_cc em i r''),
          mix'))
      (List.length l,Matching.empty,
       Edges.empty ~with_connected_components:false)
      l in
  (pat,em,mix)

type rule = Primitives.elementary_rule
type rule_id = int
type arity = Usual | Unary
type direction = Direct | Op
type rule_name = string
type rule_id_with_mode = rule_id * arity * direction

let lhs _compil _rule_id r = r.Primitives.connected_components

let add x y list  =
  match y with
  | None -> list
  | Some _ -> x::list

let mode_of_rule compil rule =
  let _env = environment compil in
  let _id = rule.Primitives.syntactic_rule in
  if (* Pierre, could you help me here please ? *)
    (* I would like to know if the rule comes from the interpretation of a rule in a direct way, or from the interpretation of a rule in a reverse way *)
    (* ex: 'A.B' A(x),B(x) <-> A(x!1),B(x!1) @ 1(2),3(4) *)
    (* I am expecting Direct for the rule A(x),B(x) -> A(x!1),B(x!1) @ 1(2) *)
    (* and Op for the rule A(x!1),B(x!1) -> A(x),B(x) @ 3(4) *)
    true
  then
    Direct
  else
    Op

let valid_modes compil rule id =
  let mode = mode_of_rule compil rule in
  List.rev_map
    (fun x -> id,x,mode)
    (List.rev
       (Usual::
        (add Unary rule.Primitives.unary_rate [])))

let rate _compil rule (_,arity,_) =
  match
    arity
  with
  | Usual -> Some rule.Primitives.rate
  | Unary -> Tools.option_map fst rule.Primitives.unary_rate

let token_vector a = a.Primitives.delta_tokens

let token_vector_of_init = token_vector

let print_rule_id log = Format.fprintf log "%i"

let print_rule ?compil =
  Kappa_printer.elementary_rule ?env:(environment_opt compil)

let print_rule_name ?compil f r =
  let env = environment_opt compil in
  let id = r.Primitives.syntactic_rule in
  Model.print_ast_rule ?env f id

let string_of_var_id ?compil r =
  let env = environment_opt compil in
  match env with
  | None -> "var("^(string_of_int r)^")"
  | Some env ->
    try
      let array = Model.get_algs env in
      fst (array.(r-1))
    with
      _ -> "var("^(string_of_int r)^")"

let rate_name compil rule rule_id =
  let (_kade_id,arity,direction) = rule_id in
  let arity_tag =
    match arity with
    | Usual -> ""
    | Unary -> "(unary context)"
  in
  let direction_tag =
    match direction with
    | Direct -> ""
    | Op -> "(op)"
  in
  Format.asprintf "%a%s%s" (print_rule_name ~compil) rule
    arity_tag direction_tag

let apply compil rule inj_nodes mix =
  let sigs = Model.signatures compil.environment in
  let concrete_removed =
    List.map (Primitives.Transformation.concretize
                (inj_nodes, Mods.IntMap.empty))
      rule.Primitives.removed
  in
  let (side_effects, dummy, edges_after_neg) =
    List.fold_left
      Rule_interpreter.apply_negative_transformation
      ([], Pattern.ObsMap.dummy Mods.IntMap.empty, mix)
      concrete_removed
  in
  let (_, remaining_side_effects, _, edges'), concrete_inserted =
    List.fold_left
      (fun (x,p) h ->
         let (x',h') =
           Rule_interpreter.apply_positive_transformation sigs x h in
         (x', h' :: p))
      (((inj_nodes, Mods.IntMap.empty),
        side_effects, dummy, edges_after_neg), [])
      rule.Primitives.inserted
  in
  let (edges'',_) =
    List.fold_left
      (fun (e,i)  ((id,_ as nc),s) ->
         Edges.add_free id s e,
         Primitives.Transformation.Freed (nc, s) :: i)
      (edges', concrete_inserted) remaining_side_effects
  in
  edges''

let lift_species compil x =
  fst @@
  Pattern.add_fully_specified_to_graph
    (Model.signatures compil.environment)
    (Edges.empty ~with_connected_components:false) x

let get_rules compil =
  Model.fold_rules
    (fun _ acc r -> r::acc) [] (environment compil)

let get_variables compil = Model.get_algs (environment compil)

let get_obs compil =
  Array.to_list
    (Model.map_observables (fun r -> r) (environment compil))

let remove_escape_char =
  (* I do not know anything about it be single quote are not allowed in Octave, please correct this function if you are more knowledgeable *)
  String.map
    (function '\'' -> '|' | x -> x)

let get_obs_titles compil =
  let env = environment compil in
  Array.to_list @@
  Model.map_observables
    (fun x -> remove_escape_char
        (Format.asprintf "%a"
           (Kappa_printer.alg_expr ~env) x))
    env

let get_compil
    ~rate_convention  ~show_reactions ~count ~compute_jacobian cli_args =
  let (_,env, contact_map,  _, _, _, _, init), _ =
    Cli_init.get_compilation cli_args in
  {
    environment = env ;
    contact_map = contact_map ;
    init = init ;
    rate_convention = rate_convention ;
    show_reactions = show_reactions ;
    count = count ;
    compute_jacobian = compute_jacobian
  }

let empty_cache compil =
  Pattern.PreEnv.of_env (Model.domain compil.environment)

let empty_lkappa_cache () = LKappa_auto.init_cache ()

let mixture_of_init compil c =
  let _,emb,m = disjoint_union compil [] in
  let m = apply compil c emb m in
  m

let nb_tokens compil =
  Model.nb_tokens (environment compil)

let divide_rule_rate_by cache compil rule =
  match compil.rate_convention with
  | Ode_args.KaSim -> cache, 1
  | Ode_args.Biochemist
  | Ode_args.Divide_by_nbr_of_autos_in_lhs ->
    let rule_id = rule.Primitives.syntactic_rule in
    let lkappa_rule =
      Model.get_ast_rule compil.environment rule_id
    in
    LKappa_auto.nauto compil.rate_convention cache
      lkappa_rule.LKappa.r_mix lkappa_rule.LKappa.r_created

(****************************************************************)
(*cannonic form per rule*)

let cannonic_form_from_syntactic_rule cache compil rule =
  let rule_id = rule.Primitives.syntactic_rule in
  let lkappa_rule =
    Model.get_ast_rule compil.environment rule_id
  in
  (*get rule_id_with_mode*)
  let rule_id_with_mode_list = valid_modes compil rule rule_id in
  let rate_opt_list =
    List.fold_left (fun current_list rule_id_with_mode ->
        let rate_opt =
          rate compil rule rule_id_with_mode
        in
        let l = rate_opt :: current_list in
        l
      ) [] rule_id_with_mode_list
  in
  let cache, hash_list =
    LKappa_auto.cannonic_form
      cache
      lkappa_rule.LKappa.r_mix
      lkappa_rule.LKappa.r_created
  in
  rule_id, rate_opt_list, cache, hash_list
