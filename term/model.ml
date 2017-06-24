(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  domain : Pattern.Env.t;
  tokens : unit NamedDecls.t;
  algs : (Alg_expr.t Locality.annot) NamedDecls.t;
  observables : Alg_expr.t Locality.annot array;
  ast_rules :
    (string Locality.annot option * LKappa.rule Locality.annot)
      array;
  rules : Primitives.elementary_rule array;
  perturbations : Primitives.perturbation array;
  dependencies_in_time : Operator.DepSet.t;
  dependencies_in_event : Operator.DepSet.t;
  algs_reverse_dependencies : Operator.DepSet.t array;
  tokens_reverse_dependencies : Operator.DepSet.t array;
  contact_map : Contact_map.t;
}

let init domain tokens algs (deps_in_t,deps_in_e,tok_rd,alg_rd)
    (ast_rules,rules) observables perturbations contact_map =
  { domain; tokens; ast_rules; rules; algs; observables; perturbations;
    algs_reverse_dependencies = alg_rd; tokens_reverse_dependencies = tok_rd;
    dependencies_in_time = deps_in_t; dependencies_in_event = deps_in_e;
    contact_map;
  }

let domain env = env.domain
let get_obs env = env.observables
let get_rules env = env.rules

let new_domain domain env = {env with domain}
let signatures env = Pattern.Env.signatures env.domain
let tokens_finder env = env.tokens.NamedDecls.finder
let algs_finder env = env.algs.NamedDecls.finder
let contact_map env = env.contact_map

let num_of_agent nme env = Signature.num_of_agent nme (signatures env)

let fold_rules f x env =
  Tools.array_fold_lefti (fun i x rule -> f i x rule) x env.rules

let fold_perturbations f x env =
  Tools.array_fold_lefti (fun i x p -> f i x p) x env.perturbations

let get_rule env i = env.rules.(i)

let get_ast_rule env i =
  fst (snd (env.ast_rules.(i-1)))

  let fold_ast_rules f x env =
    Tools.array_fold_lefti (fun i x (_, _rule) ->
        let lkappa_rule = get_ast_rule env i in
        f i x lkappa_rule
      ) x env.ast_rules

let get_ast_rule_rate_pos ~unary env i =
  if unary then
    match (fst (snd (env.ast_rules.(i-1)))).LKappa.r_un_rate with
    | None -> failwith "No unary rate to get position of"
    | Some ((_,pos),_) -> pos
  else snd (fst (snd (env.ast_rules.(i-1)))).LKappa.r_rate

let nb_rules env = Array.length env.rules

let nums_of_rule name env =
  fold_rules
    (fun i acc r ->
       match env.ast_rules.(pred r.Primitives.syntactic_rule) with
       | Some (x,_), _ -> if x = name then i::acc else acc
       | None, _ -> acc)
    [] env

let nb_syntactic_rules env = Array.length env.ast_rules

let num_of_alg s env = NamedDecls.elt_id ~kind:"variable" env.algs s
let get_alg env i = fst @@ snd env.algs.NamedDecls.decls.(i)
let get_algs env = env.algs.NamedDecls.decls
let nb_algs env = NamedDecls.size env.algs

let num_of_token str env = NamedDecls.elt_id ~kind:"token" env.tokens str
let nb_tokens env = NamedDecls.size env.tokens

let get_perturbation env i = env.perturbations.(i)
let nb_perturbations env = Array.length env.perturbations

let get_alg_reverse_dependencies env i = env.algs_reverse_dependencies.(i)
let get_token_reverse_dependencies env i = env.tokens_reverse_dependencies.(i)
let all_dependencies env =
  (env.dependencies_in_time,env.dependencies_in_event,
   env.tokens_reverse_dependencies,env.algs_reverse_dependencies)

let print_agent ?env f i =
  match env with
  | None -> Format.fprintf f "__agent_%i" i
  | Some env -> Signature.print_agent (signatures env) f i

let print_alg ?env f id =
  match env with
  | None -> Format.fprintf f "__alg_%i" id
  | Some env ->
    let name = NamedDecls.elt_name env.algs id in
    let special = Tools.not_an_id name in
    let () = if special then Format.pp_print_string f "'" in
    let () = Format.pp_print_string f name in
    if special then Format.pp_print_string f "'"

let print_token ?env f id =
  match env with
  | None -> Format.fprintf f "__token_%i" id
  | Some env ->
    Format.fprintf f "%s" (NamedDecls.elt_name env.tokens id)

let print_ast_rule ?env f i =
  match env with
  | None -> Format.fprintf f "__ast_rule_%i" i
  | Some env ->
    let sigs = signatures env in
    if i = 0 then Format.pp_print_string f "Perturbations"
    else
      match env.ast_rules.(pred i) with
      | (Some (na,_),_) -> Format.pp_print_string f na
      | (None,(r,_)) ->
        LKappa.print_rule ~full:false sigs
          (print_token ~env) (print_alg ~env) f r

let print_rule ?env f id =
  match env with
  | None -> Format.fprintf f "__rule_%i" id
  | Some env -> print_ast_rule ~env f env.rules.(id).Primitives.syntactic_rule

let map_observables f env =
  Array.map (fun (x,_) -> f x) env.observables

let print_kappa pr_alg pr_pert f env =
  let sigs = signatures env in
  Format.fprintf
    f "@[<v>%a%t@,%a%t%a@,%a%t%a@]"
    (NamedDecls.print
       ~sep:Pp.space (fun _ n f () -> Format.fprintf f "%%token: %s" n))
    env.tokens
    (fun f -> if env.tokens.NamedDecls.decls <> [||] then Pp.space f)
    (NamedDecls.print
       ~sep:Pp.space
       (fun i n f (e,_) ->
          Format.fprintf f "@[<h>%%var:/*%i*/ '%s' %a@]" i n (pr_alg env) e))
    env.algs
    (fun f -> if env.algs.NamedDecls.decls <> [||] then Pp.space f)
    (Pp.array Pp.space ~trailing:Pp.space
       (fun _ f (e,_) -> Format.fprintf f "@[<h>%%plot: %a@]" (pr_alg env) e))
    env.observables
    (Pp.array Pp.space ~trailing:Pp.space
       (fun _ f (na,(e,_)) ->
          Format.fprintf f "%a%a"
            (Pp.option ~with_space:false
               (fun f (na,_) -> Format.fprintf f "'%s' " na)) na
            (LKappa.print_rule
               ~full:true sigs (print_token ~env) (print_alg ~env))
            e))
    env.ast_rules
    (fun f -> if env.perturbations <> [||] then Pp.space f)
    (Pp.array Pp.space (fun i f p ->
         Format.fprintf f "@[<h>/*%i*/%a@]" i (pr_pert env) p))
    env.perturbations

let print pr_alg pr_rule pr_pert f env =
  let () = print_kappa pr_alg pr_pert f env in
  Format.fprintf
    f "@,@[<v>@[<v 2>Signatures:@,%a@]@,@[<v 2>Rules:@,%a@]@]"
     Signature.print (signatures env)
    (Pp.array Pp.space
       (fun i f r -> Format.fprintf f "@[<2>%i:@ %a@]" i (pr_rule env) r))
    env.rules

let check_if_counter_is_filled_enough x =
  if not @@
     Primitives.exists_modification
       (function Primitives.STOP _ -> true
               | (Primitives.ITER_RULE _ | Primitives.UPDATE _ |
                  Primitives.SNAPSHOT _ | Primitives.CFLOW _ |
                  Primitives.FLUX _ | Primitives.FLUXOFF _ |
                  Primitives.CFLOWOFF _ | Primitives.PLOTENTRY |
                  Primitives.PRINT _ | Primitives.SPECIES _ |
                  Primitives.SPECIES_OFF _ ) -> false) x.perturbations then
    raise (ExceptionDefn.Malformed_Decl
             (Locality.dummy_annot
                "There is no way for the simulation to stop."))

let overwrite_vars alg_overwrite env =
  let algs' =
    Array.map (fun (x,y) -> (Locality.dummy_annot x,y))
      env.algs.NamedDecls.decls in
  let () = List.iter (fun (i,v) ->
      algs'.(i) <- (fst algs'.(i),Locality.dummy_annot v)) alg_overwrite in
  { env with algs = NamedDecls.create algs' }

let propagate_constant ?max_time ?max_events updated_vars alg_overwrite x =
  let algs' =
    Array.map (fun (x,y) -> (Locality.dummy_annot x,y))
      x.algs.NamedDecls.decls in
  let () = List.iter (fun (i,v) ->
      algs'.(i) <- (fst algs'.(i),Locality.dummy_annot v)) alg_overwrite in
  let () =
    Array.iteri
      (fun i (na,v) ->
         algs'.(i) <-
           (na,Alg_expr.propagate_constant
              ?max_time ?max_events updated_vars algs' v))
      algs' in
  {
    domain = x.domain;
    tokens = x.tokens;
    algs = NamedDecls.create algs';
    observables =
      Array.map
        (Alg_expr.propagate_constant
           ?max_time ?max_events updated_vars algs') x.observables;
    ast_rules = x.ast_rules;
    rules =
      Array.map
        (Primitives.map_expr_rule
           (Alg_expr.propagate_constant
              ?max_time ?max_events updated_vars algs')) x.rules;
    perturbations =
      Array.map
        (Primitives.map_expr_perturbation
           (Alg_expr.propagate_constant
              ?max_time ?max_events updated_vars algs')
           (Alg_expr.propagate_constant_bool
              ?max_time ?max_events updated_vars algs'))
        x.perturbations;
    dependencies_in_time = x.dependencies_in_time;
    dependencies_in_event = x.dependencies_in_event;
    algs_reverse_dependencies = x.algs_reverse_dependencies;
    tokens_reverse_dependencies = x.tokens_reverse_dependencies;
    contact_map = x.contact_map;
  }

let kappa_instance_to_yojson =
  JsonUtil.of_list (JsonUtil.of_array Pattern.id_to_yojson)

let to_yojson env =
  `Assoc [
    "update", Pattern.Env.to_yojson (domain env);
    "tokens", NamedDecls.to_json (fun () -> `Null) env.tokens;
    "algs", NamedDecls.to_json
      (fun (x,_) ->
         Alg_expr.e_to_yojson kappa_instance_to_yojson JsonUtil.of_int x)
      env.algs;
    "observables",
    `List
      (Array.fold_right
         (fun (x,_) l ->
            Alg_expr.e_to_yojson kappa_instance_to_yojson JsonUtil.of_int x :: l)
         env.observables []);
    "ast_rules",
    `List
      (Array.fold_right (fun (n,(r,_)) l ->
           `List [(match n with None -> `Null | Some (n,_) -> `String n);
              LKappa.rule_to_json r]::l) env.ast_rules []);
    "elementary_rules", JsonUtil.of_array Primitives.rule_to_yojson env.rules;
    "contact_map", Contact_map.to_yojson (env.contact_map);
    "perturbations",
    JsonUtil.of_array Primitives.perturbation_to_yojson env.perturbations;
    "dependencies_in_time", Operator.depset_to_yojson env.dependencies_in_time;
    "dependencies_in_event", Operator.depset_to_yojson env.dependencies_in_event;
    "algs_reverse_dependencies",
    JsonUtil.of_array Operator.depset_to_yojson env.algs_reverse_dependencies;
    "tokens_reverse_dependencies",
    JsonUtil.of_array Operator.depset_to_yojson env.tokens_reverse_dependencies;
  ]

let kappa_instance_of_yojson =
  JsonUtil.to_list (JsonUtil.to_array Pattern.id_of_yojson)

let of_yojson = function
  | `Assoc l as x when List.length l = 12 ->
    begin
      try {
        domain = Pattern.Env.of_yojson (List.assoc "update" l);
        tokens = NamedDecls.of_json (fun _ -> ()) (List.assoc "tokens" l);
        algs = NamedDecls.of_json
            (fun x -> Locality.dummy_annot
                (Alg_expr.e_of_yojson kappa_instance_of_yojson
                   (JsonUtil.to_int ?error_msg:None) x))
            (List.assoc "algs" l);
        observables = (match List.assoc "observables" l with
            | `List o ->
              Tools.array_map_of_list
                (fun x -> Locality.dummy_annot
                    (Alg_expr.e_of_yojson kappa_instance_of_yojson
                       (JsonUtil.to_int ?error_msg:None) x)) o
            | `Null -> [||]
            | _ -> raise Not_found);
        ast_rules = (match List.assoc "ast_rules" l with
            | `List o ->
              Tools.array_map_of_list
                (function
                  | `List [`Null;r]->
                    (None, Locality.dummy_annot (LKappa.rule_of_json r))
                  | `List [`String n;r]->
                    (Some (Locality.dummy_annot n),
                     Locality.dummy_annot (LKappa.rule_of_json r))
                  | _ -> raise Not_found) o
            | `Null -> [||]
            | _ -> raise Not_found);
        rules = (match (List.assoc "elementary_rules" l) with
            | `List o ->
              Tools.array_map_of_list Primitives.rule_of_yojson o
            | _ -> raise Not_found);
        perturbations =
          JsonUtil.to_array Primitives.perturbation_of_yojson
            (Yojson.Basic.Util.member "perturbations" x);
        dependencies_in_time =
          Operator.depset_of_yojson
            (Yojson.Basic.Util.member "dependencies_in_time" x);
        dependencies_in_event =
          Operator.depset_of_yojson
            (Yojson.Basic.Util.member "dependencies_in_event" x);
        algs_reverse_dependencies =
          JsonUtil.to_array Operator.depset_of_yojson
            (Yojson.Basic.Util.member "algs_reverse_dependencies" x);
        tokens_reverse_dependencies =
          JsonUtil.to_array Operator.depset_of_yojson
            (Yojson.Basic.Util.member "tokens_reverse_dependencies" x);
        contact_map = Contact_map.of_yojson (List.assoc "contact_map" l);
      }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))

let unary_patterns env = 
  fold_rules
    (fun _ acc r ->
        match r.Primitives.unary_rate with
        | None -> acc
        | Some _ ->
          Pattern.Set.add
            r.Primitives.connected_components.(0)
            (Pattern.Set.add r.Primitives.connected_components.(1) acc)
    ) Pattern.Set.empty env