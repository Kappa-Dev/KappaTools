type t = {
  domain : Pattern.Env.t;
  tokens : unit NamedDecls.t;
  algs : (Alg_expr.t Location.annot) NamedDecls.t;
  observables : Alg_expr.t Location.annot array;
  ast_rules : (string Location.annot option * LKappa.rule Location.annot) array;
  rules : Primitives.elementary_rule array;
  cc_of_unaries : Pattern.Set.t;
  perturbations : Primitives.perturbation array;
  dependencies_in_time : Operator.DepSet.t;
  dependencies_in_event : Operator.DepSet.t;
  need_update_each_loop : Operator.DepSet.t; (*union of 2 above for perf*)
  algs_reverse_dependencies : Operator.DepSet.t array;
  tokens_reverse_dependencies : Operator.DepSet.t array;
}

let init domain tokens algs (deps_in_t,deps_in_e,tok_rd,alg_rd)
    (ast_rules,rules,cc_of_unaries) obs perts =
  { domain; tokens; ast_rules; rules; cc_of_unaries; algs;
    observables = obs; perturbations = perts;
    algs_reverse_dependencies = alg_rd; tokens_reverse_dependencies = tok_rd;
    need_update_each_loop = Operator.DepSet.union deps_in_t deps_in_e;
    dependencies_in_time = deps_in_t; dependencies_in_event = deps_in_e;
  }

let domain env = env.domain
let new_domain domain env = {env with domain}
let signatures env = Pattern.Env.signatures env.domain
let tokens_finder env = env.tokens.NamedDecls.finder
let algs_finder env = env.algs.NamedDecls.finder

let num_of_agent nme env = Signature.num_of_agent nme (signatures env)

let fold_rules f x env =
  Tools.array_fold_lefti (fun i x rule -> f i x rule) x env.rules
let fold_perturbations f x env =
  Tools.array_fold_lefti (fun i x p -> f i x p) x env.perturbations

let get_rule env i = env.rules.(i)
let get_ast_rule env i =
  fst (snd (env.ast_rules.(i-1)))

let nb_rules env = Array.length env.rules
let nums_of_rule name env =
  fold_rules
    (fun i acc r ->
       match env.ast_rules.(pred r.Primitives.syntactic_rule) with
       | Some (x,_), _ -> if x = name then i::acc else acc
       | None, _ -> acc)
    [] env

let nb_syntactic_rules env = Array.length env.ast_rules

let connected_components_of_unary_rules env = env.cc_of_unaries

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
let get_always_outdated env = env.need_update_each_loop
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
        LKappa.print_rule ~ltypes:false ~rates:false sigs
          (print_token ~env) (print_alg ~env) f r

let print_rule ?env f id =
  match env with
  | None -> Format.fprintf f "__rule_%i" id
  | Some env -> print_ast_rule ~env f env.rules.(id).Primitives.syntactic_rule

let map_observables f env =
  Array.map (fun (x,_) -> f x) env.observables

let print pr_alg pr_rule pr_pert f env =
  let () =
    Format.fprintf
      f "@[<v>@[<v 2>Signatures:@,%a@]@,@[<2>Tokens:@ %a@]@,"
      Signature.print (signatures env)
      (NamedDecls.print ~sep:Pp.space (fun _ n f () -> Format.pp_print_string f n))
      env.tokens in
  let () =
    Format.fprintf
      f "@[<v 2>Alg_expr:@,%a@]@,@[<2>Plot:@ %a@]@,"
      (NamedDecls.print
         ~sep:Pp.space
         (fun i n f (e,_) -> Format.fprintf f "@[<2>%i:%s:@ %a@]" i n (pr_alg env) e))
      env.algs
      (Pp.array Pp.space (fun _ f (e,_) -> pr_alg env f e))
      env.observables in
  Format.fprintf
    f"@[<v 2>Rules:@,%a@]@,@[<v 2>Perturbations:@,%a@]@]"
    (Pp.array Pp.space
       (fun i f r -> Format.fprintf f "@[<2>%i:@ %a@]" i (pr_rule env) r))
    env.rules
    (Pp.array Pp.space (fun i f p ->
         Format.fprintf f "@[<2>/*%i*/%a@]" i (pr_pert env) p))
    env.perturbations
(*
  desc_table : (string,out_channel * Format.formatter) Hashtbl.t;
 *)

let check_if_counter_is_filled_enough x =
  if not @@
     Primitives.exists_modification
       (function Primitives.STOP _ -> true
               | (Primitives.ITER_RULE _ | Primitives.UPDATE _ |
                  Primitives.SNAPSHOT _ | Primitives.CFLOW _ |
                  Primitives.FLUX _ | Primitives.FLUXOFF _ |
                  Primitives.CFLOWOFF _ | Primitives.PLOTENTRY |
                  Primitives.PRINT _) -> false) x.perturbations then
    raise (ExceptionDefn.Malformed_Decl
             (Location.dummy_annot
                "There is no way for the simulation to stop."))

let propagate_constant ?max_time ?max_events updated_vars x =
  let algs' =
    Array.map (fun (x,y) -> (Location.dummy_annot x,y))
      x.algs.NamedDecls.decls in
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
    cc_of_unaries = x.cc_of_unaries;
    perturbations =
      Array.map
        (Primitives.map_expr_perturbation
           (Alg_expr.propagate_constant
              ?max_time ?max_events updated_vars algs')
           (Alg_expr.propagate_constant_bool
              ?max_time ?max_events updated_vars algs'))
        x.perturbations;
    need_update_each_loop = x.need_update_each_loop;
    dependencies_in_time = x.dependencies_in_time;
    dependencies_in_event = x.dependencies_in_event;
    algs_reverse_dependencies = x.algs_reverse_dependencies;
    tokens_reverse_dependencies = x.tokens_reverse_dependencies;
  }

let kappa_instance_to_yojson =
  JsonUtil.of_list (JsonUtil.of_array Pattern.id_to_yojson)

let to_yojson env =
  let () =
    ExceptionDefn.warning
      (fun f -> Format.pp_print_string f "Environment.to_json is partial") in
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
    (* rules : Primitives.elementary_rule array;
       cc_of_unaries : Pattern.Set.t;
       perturbations : Primitives.perturbation array;
       dependencies_in_time : Operator.DepSet.t;
       dependencies_in_event : Operator.DepSet.t;
       need_update_each_loop : Operator.DepSet.t; (*union of 2 above for perf*)
       algs_reverse_dependencies : Operator.DepSet.t array;
       tokens_reverse_dependencies : Operator.DepSet.t array;*)
  ]

let kappa_instance_of_yojson =
  JsonUtil.to_list (JsonUtil.to_array Pattern.id_of_yojson)

let of_yojson = function
  | `Assoc l as x when List.length l = 5 ->
    begin
      try
        { domain = Pattern.Env.of_yojson (List.assoc "update" l);
          tokens = NamedDecls.of_json (fun _ -> ()) (List.assoc "tokens" l);
          algs = NamedDecls.of_json
              (fun x -> Location.dummy_annot
                  (Alg_expr.e_of_yojson kappa_instance_of_yojson
                     (JsonUtil.to_int ?error_msg:None) x))
              (List.assoc "algs" l);
          observables = (match List.assoc "observables" l with
              | `List o ->
                Tools.array_map_of_list
                  (fun x -> Location.dummy_annot
                      (Alg_expr.e_of_yojson kappa_instance_of_yojson
                         (JsonUtil.to_int ?error_msg:None) x)) o
              | _ -> raise Not_found);
          ast_rules = (match List.assoc "ast_rules" l with
              | `List o ->
                Tools.array_map_of_list
                  (function
                    | `List [`Null;r]->
                      (None, Location.dummy_annot (LKappa.rule_of_json r))
                    | `List [`String n;r]->
                      (Some (Location.dummy_annot n),
                       Location.dummy_annot (LKappa.rule_of_json r))
                    | _ -> raise Not_found) o
              | _ -> raise Not_found);
          rules = [||];
          cc_of_unaries = Pattern.Set.empty;
          perturbations = [||];
          dependencies_in_time = Operator.DepSet.empty;
          dependencies_in_event = Operator.DepSet.empty;
          need_update_each_loop = Operator.DepSet.empty;
          algs_reverse_dependencies = [||];
          tokens_reverse_dependencies = [||];
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))
