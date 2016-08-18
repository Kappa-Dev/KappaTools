(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Aug 18 2016>
*)

type compil =
  {
    contact_map: (int list * (int * int) list) array array ;
    environment: Environment.t ;
    init: (Alg_expr.t * Primitives.elementary_rule * Location.t) list ;
    rate_convention: Ode_args.rate_convention ;
    show_reactions: bool ;
    count: Ode_args.count ;
    compute_jacobian: bool
  }

type cache = Connected_component.PreEnv.t
type hidden_init = Primitives.elementary_rule
type init = (Alg_expr.t * hidden_init * Location.t) list

let get_init compil= compil.init

let lift_opt f compil_opt =
  match
    compil_opt
  with
  | None -> None
  | Some a -> Some (f a)

let contact_map compil = compil.contact_map
let environment compil = compil.environment
let sigs compil = Environment.signatures (environment compil)

let sigs_opt = lift_opt sigs
let environment_opt = lift_opt environment
type mixture = Edges.t(* not necessarily connected, fully specified *)
type chemical_species = Connected_component.cc
(* connected, fully specified *)
type canonic_species = chemical_species (* chemical species in canonic form *)
type pattern = Connected_component.cc array
(* not necessarity connected, maybe partially specified *)
type connected_component = Connected_component.cc
(* connected, maybe partially specified *)

let dummy_chemical_species compil =
  Connected_component.empty_cc (sigs compil)

let rate_convention compil = compil.rate_convention
let what_do_we_count compil = compil.count

let do_we_divide_rates_by_n_auto_in_lhs compil =
  match
    rate_convention compil
  with
  | Ode_args.KaSim -> false
  | Ode_args.Biochemist -> true

  let do_we_count_in_embeddings compil =
    match
      what_do_we_count compil 
    with
    | Ode_args.Occurrences -> false
    | Ode_args.Embeddings -> true

let do_we_prompt_reactions compil =
  compil.show_reactions

let print_chemical_species ?compil =
  Connected_component.print ?sigs:(sigs_opt compil) ?with_id:None

let print_canonic_species = print_chemical_species

let nbr_automorphisms_in_chemical_species x =
  List.length (Connected_component.automorphisms x)
let nbr_automorphisms_in_pattern a =
  let a' = Array.copy a in
  let () = Array.sort Connected_component.compare_canonicals a' in
  let rec aux i acc n el =
    if i >= Array.length a' then n * acc else
      let n' =
        if Connected_component.is_equal_canonicals a'.(i) el then  n+1 else 1 in
      aux (succ i) (n *acc) n' a'.(i) in
  if Array.length a' = 0 then 1 else
    let acc = Array.fold_left
        (fun a x -> a*nbr_automorphisms_in_chemical_species x) 1 a' in
    aux 1 acc 1 a'.(0)

let compare_connected_component = Connected_component.compare_canonicals
let print_connected_component ?compil =
  Connected_component.print ?sigs:(sigs_opt compil) ?with_id:None

let canonic_form x = x

let connected_components_of_patterns = Array.to_list

let connected_components_of_mixture compil cache e =
  let contact_map = contact_map compil in
  let sigs = sigs compil in
  let snap = Edges.build_snapshot sigs e in
  List.fold_left
    (fun (cache,acc) (i,m) ->
       match Snip.connected_components_sum_of_ambiguous_mixture
               contact_map cache (LKappa.of_raw_mixture m) with
       | cache',[[|x|],_] -> cache',Tools.recti (fun a _ -> x::a) acc i
       | _ -> assert false)
    (cache,[]) snap

type embedding = Renaming.t (* the domain is connected *)
type embedding_forest = Connected_component.Matching.t
(* the domain may be not connected *)

let lift_embedding x =
  Tools.unsome
    Connected_component.Matching.empty
    (Connected_component.Matching.add_cc Connected_component.Matching.empty 0 x)
let find_embeddings = Connected_component.embeddings_to_fully_specified

let find_embeddings_unary_binary p x =
  Tools.array_fold_lefti
    (fun i acc cc ->
       let em = find_embeddings cc x in
       Tools.list_map_flatten
         (fun m ->
            Tools.list_map_option
              (fun r -> Connected_component.Matching.add_cc m i r)
              em)
         acc)
    [Connected_component.Matching.empty]
    p

let disjoint_union compil l =
  let sigs = sigs compil in
  let pat = Tools.array_map_of_list (fun (x,_,_) -> x) l in
  let _,em,mix =
    List.fold_left
      (fun (i,em,mix) (_,r,cc) ->
         let i = pred i in
         let (mix',r') =
           Connected_component.add_fully_specified_to_graph sigs mix cc  in
         let r'' = Renaming.compose false r r' in
         (i,
          Tools.unsome
            Connected_component.Matching.empty
            (Connected_component.Matching.add_cc em i r''),
          mix'))
      (List.length l,Connected_component.Matching.empty,Edges.empty ())
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

let rate_name compil rule rule_id =
  let _env = environment compil in
  let _id = rule.Primitives.syntactic_rule in
  let (kade_id,arity,direction) = rule_id in
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
  let rule_name = (* Pierre, could you help me here please ? *)
    (* I would need the base label of the rule *)
    (* ex: 'A.B' A(x),B(x) <-> A(x!1),B(x!1) @ 1(2),3(4) *)
    (* I am expecting "A.B" whatever the value of the variables arity or direct is *)
    "Rule "^(string_of_int kade_id)
  in
  rule_name^arity_tag^direction_tag

let token_vector a =
  let add,remove  =
    a.Primitives.injected_tokens,a.Primitives.consumed_tokens
  in
  List.fold_left
    (fun token_vector (a,b) ->
       (Location.dummy_annot (Alg_expr.UN_ALG_OP(Operator.UMINUS,a)),b)::token_vector)
    add remove

let token_vector_of_init = token_vector
let print_rule_id log = Format.fprintf log "%i"
let print_rule ?compil =
  Kappa_printer.elementary_rule ?env:(environment_opt compil)

let apply compil rule inj_nodes mix =
  let sigs = sigs compil in
  let concrete_removed =
    List.map (Primitives.Transformation.concretize
                (inj_nodes,Mods.IntMap.empty)) rule.Primitives.removed in
  let (side_effects,edges_after_neg) =
    List.fold_left
      Rule_interpreter.apply_negative_transformation
      ([],mix) concrete_removed in
  let (_,remaining_side_effects,edges'),concrete_inserted =
    List.fold_left
      (fun (x,p) h ->
         let (x', h') =
           Rule_interpreter.apply_positive_transformation sigs x h in
         (x',h'::p))
      (((inj_nodes,Mods.IntMap.empty),side_effects,edges_after_neg),[])
      rule.Primitives.inserted in
  let (edges'',_) =
    List.fold_left
      (fun (e,i)  ((id,_ as nc),s) ->
         Edges.add_free id s e,Primitives.Transformation.Freed (nc,s)::i)
      (edges',concrete_inserted) remaining_side_effects in
  edges''

let lift_species compil x =
  fst @@
  Connected_component.add_fully_specified_to_graph
    (sigs compil) (Edges.empty ()) x

let get_rules compil =
  Environment.fold_rules
    (fun _ acc r -> r::acc) [] (environment compil)
let get_variables compil = Environment.get_algs (environment compil)
let get_obs compil =
  Array.to_list @@ Environment.map_observables (fun r -> r) (environment compil)

let remove_escape_char =
  (* I do not know anything about it be single quote are not allowed in Octave, please correct this function if you are moe knowledgeable *)
  String.map
    (function '\'' -> '|' | x -> x)


let get_obs_titles compil =
  let env = environment compil in
  Array.to_list @@
  Environment.map_observables
    (fun x -> remove_escape_char
        (Format.asprintf "%a"
           (Kappa_printer.alg_expr ~env) x))
    env

let get_compil
    ~rate_convention  ~show_reactions ~count ~compute_jacobian
    common_args cli_args =
  let (env,_,contact_map,_,_,_,_,init),_,_ =
    Cli_init.get_compilation common_args cli_args in
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
  Connected_component.PreEnv.empty (sigs compil)

let mixture_of_init compil c =
  let _,emb,m = disjoint_union compil [] in
  let m = apply compil c emb m in
  m

let nb_tokens compil =
  Environment.nb_tokens (environment compil)
