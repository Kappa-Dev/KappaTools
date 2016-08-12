(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jul 29 2016>
*)

type mixture = Edges.t(* not necessarily connected, fully specified *)
type chemical_species = Connected_component.cc
(* connected, fully specified *)
type canonic_species = chemical_species (* chemical species in canonic form *)
type pattern = Connected_component.cc array
(* not necessarity connected, maybe partially specified *)
type connected_component = Connected_component.cc
(* connected, maybe partially specified *)

let dummy_chemical_species sigs = Connected_component.empty_cc sigs

let do_we_divide_rates_by_n_auto_in_lhs = true
let print_chemical_species = Connected_component.print ?sigs:None ~with_id:()
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
let print_connected_component =
  Connected_component.print ?sigs:None ~with_id:()

let canonic_form x = x

let connected_components_of_patterns = Array.to_list

let connected_components_of_mixture sigs e =
  let snap = Edges.build_snapshot sigs e in
  List.fold_left
    (fun acc (i,m) ->
       match Snip.connected_components_sum_of_ambiguous_mixture
               [||] (Connected_component.PreEnv.empty sigs)
               (LKappa.of_raw_mixture m) with
       | _,[[|x|],_] -> Tools.recti (fun a _ -> x::a) acc i
       | _ -> assert false)
    [] snap

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

let disjoint_union sigs l =
  let pat = Tools.array_map_of_list (fun (x,_,_) -> x) l in
  let _,em,mix =
    List.fold_left
      (fun (i,em,mix) (_,r,cc) ->
         let (mix',r') =
           Connected_component.add_fully_specified_to_graph sigs mix cc in
         let r'' = Renaming.compose false r r' in
         (succ i,
          Tools.unsome
            Connected_component.Matching.empty
            (Connected_component.Matching.add_cc em i r''),
          mix'))
      (0,Connected_component.Matching.empty,Edges.empty ())
      l in
  (pat,em,mix)

type rule = Primitives.elementary_rule
type rule_id = int
type arity = Usual | Unary
type rule_id_with_mode = rule_id * arity

let lhs r = r.Primitives.connected_components

let add x y list  =
  match y with
  | None -> list
  | Some _ -> x::list

let valid_modes rule =
  Usual::
  (add Unary rule.Primitives.unary_rate [])

let rate rule mode =
  match
    mode
  with
  | Usual -> Some rule.Primitives.rate
  | Unary -> Tools.option_map fst rule.Primitives.unary_rate

let token_vector a =
  let add,remove  =
    a.Primitives.injected_tokens,a.Primitives.consumed_tokens
  in
  List.fold_left
    (fun token_vector (a,b) ->
       (Location.dummy_annot (Alg_expr.UN_ALG_OP(Operator.UMINUS,a)),b)::token_vector)
    add remove

let print_rule_id log = Format.fprintf log "%i"

let apply sigs rule inj_nodes mix =
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

let lift_species sigs x =
  fst @@
  Connected_component.add_fully_specified_to_graph
    sigs (Edges.empty ()) x

let get_rules env =
  Environment.fold_rules (fun _ acc r -> r::acc) [] env
let get_variables env = Environment.get_algs env
let get_obs env =
  Array.to_list @@ Environment.map_observables (fun r -> r) env

let get_obs_titles env =
  Array.to_list @@
  Environment.map_observables
    (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
    env

let get_compil common_args cli_args =
  let (env,_,_,_,_,_,_,init),_,_ =
    Cli_init.get_compilation common_args cli_args in
  env,init
