type t = {
  signatures : Signature.s;
  tokens : unit NamedDecls.t;
  algs : (Alg_expr.t Location.annot) NamedDecls.t;
  observables : Alg_expr.t Location.annot array;
  ast_rules : (string Location.annot option * LKappa.rule Location.annot) array;
  rules : Primitives.elementary_rule array;
  cc_of_unaries : Connected_component.Set.t;
  perturbations : Primitives.perturbation array;
  dependencies_in_time : Operator.DepSet.t;
  dependencies_in_event : Operator.DepSet.t;
  need_update_each_loop : Operator.DepSet.t; (*union of 2 above for perf*)
  algs_reverse_dependencies : Operator.DepSet.t array;
  tokens_reverse_dependencies : Operator.DepSet.t array;
}

let init sigs tokens algs (deps_in_t,deps_in_e,tok_rd,alg_rd)
    (ast_rules,rules,cc_of_unaries) obs perts =
  { signatures = sigs; tokens = tokens; ast_rules = ast_rules;
    rules = rules; cc_of_unaries = cc_of_unaries; algs = algs;
    observables = obs; perturbations = perts;
    algs_reverse_dependencies = alg_rd; tokens_reverse_dependencies = tok_rd;
    need_update_each_loop = Operator.DepSet.union deps_in_t deps_in_e;
    dependencies_in_time = deps_in_t; dependencies_in_event = deps_in_e;
  }

let signatures env = env.signatures
let tokens_finder env = env.tokens.NamedDecls.finder
let algs_finder env = env.algs.NamedDecls.finder

let num_of_agent nme env =
  Signature.num_of_agent nme env.signatures

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
  | Some env ->
    Signature.print_agent env.signatures f i
let print_alg ?env f id =
  match env with
  | None -> Format.fprintf f "__alg_%i" id
  | Some env ->
    Format.fprintf f "%s" (NamedDecls.elt_name env.algs id)
let print_token ?env f id =
  match env with
  | None -> Format.fprintf f "__token_%i" id
  | Some env ->
    Format.fprintf f "%s" (NamedDecls.elt_name env.tokens id)

let print_ast_rule ?env f i =
  match env with
  | None -> Format.fprintf f "__ast_rule_%i" i
  | Some env ->
    let sigs = env.signatures in
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
      Signature.print env.signatures
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

let check_if_counter_is_filled_enough counter x =
  let () =
    if Counter.max_time counter = None && Counter.max_events counter = None
       && not @@
       Primitives.exists_modification
         (function Primitives.STOP _ -> true
                 | (Primitives.ITER_RULE _ | Primitives.UPDATE _ |
                    Primitives.SNAPSHOT _ | Primitives.CFLOW _ |
                    Primitives.FLUX _ | Primitives.FLUXOFF _ |
                        Primitives.CFLOWOFF _ | Primitives.PLOTENTRY |
                    Primitives.PRINT _) -> false) x.perturbations then
      raise (ExceptionDefn.Malformed_Decl
               (Location.dummy_annot
                  "There is no way for the simulation to stop.")) in
  if Array.length x.observables > 0 && Counter.plot_points counter < 0
     && not @@ Primitives.exists_modification
       (fun x -> x = Primitives.PLOTENTRY)
       x.perturbations
  then raise (ExceptionDefn.Malformed_Decl
                ("Number of point to plot has not been defined.",
                snd x.observables.(0)))

let propagate_constant updated_vars counter x =
  let algs' = Array.copy x.algs.NamedDecls.decls in
  let () =
    Array.iteri
      (fun i (na,v) ->
         algs'.(i) <-
           (na,Alg_expr.propagate_constant updated_vars counter algs' v))
      algs' in
  {
    signatures = x.signatures;
    tokens = x.tokens;
    algs = NamedDecls.create algs';
    observables =
      Array.map
        (Alg_expr.propagate_constant updated_vars counter algs') x.observables;
    ast_rules = x.ast_rules;
    rules =
      Array.map
        (Primitives.map_expr_rule
           (Alg_expr.propagate_constant updated_vars counter algs')) x.rules;
    cc_of_unaries = x.cc_of_unaries;
    perturbations =
      Array.map
        (Primitives.map_expr_perturbation
           (Alg_expr.propagate_constant updated_vars counter algs')
           (Alg_expr.propagate_constant_bool updated_vars counter algs'))
        x.perturbations;
    need_update_each_loop = x.need_update_each_loop;
    dependencies_in_time = x.dependencies_in_time;
    dependencies_in_event = x.dependencies_in_event;
    algs_reverse_dependencies = x.algs_reverse_dependencies;
    tokens_reverse_dependencies = x.tokens_reverse_dependencies;
  }

let dummy_kappa_instance _ =
  let () =
    ExceptionDefn.warning
      (fun f -> Format.pp_print_string
          f "KAPPA_INSTANCE not translated in json") in
  `Null

let to_json env =
  let () =
    ExceptionDefn.warning
      (fun f -> Format.pp_print_string f "Environment.to_json is partial") in
  `Assoc [
    "signatures", Signature.to_json env.signatures;
    "tokens", NamedDecls.to_json (fun () -> `Null) env.tokens;
    "algs", NamedDecls.to_json
      (fun (x,_) ->
         Alg_expr.to_json dummy_kappa_instance JsonUtil.of_int x) env.algs;
    "observables",
    `List (Array.fold_right
             (fun (x,_) l ->
                Alg_expr.to_json dummy_kappa_instance JsonUtil.of_int x :: l)
             env.observables []);
    "ast_rules",
    `List
      (Array.fold_right (fun (n,(r,_)) l ->
           `List [(match n with None -> `Null | Some (n,_) -> `String n);
              LKappa.rule_to_json r]::l) env.ast_rules []);
    (* rules : Primitives.elementary_rule array;
       cc_of_unaries : Connected_component.Set.t;
       perturbations : Primitives.perturbation array;
       dependencies_in_time : Operator.DepSet.t;
       dependencies_in_event : Operator.DepSet.t;
       need_update_each_loop : Operator.DepSet.t; (*union of 2 above for perf*)
       algs_reverse_dependencies : Operator.DepSet.t array;
       tokens_reverse_dependencies : Operator.DepSet.t array;*)
  ]

let kappa_instance_of_dummy = function
  | `Null -> []
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct kappa instance",x))

let of_json = function
  | `Assoc l as x when List.length l = 5 ->
    begin
      try
        { signatures = Signature.of_json (List.assoc "signatures" l);
          tokens = NamedDecls.of_json (fun _ -> ()) (List.assoc "tokens" l);
          algs = NamedDecls.of_json
              (fun x -> Location.dummy_annot
                  (Alg_expr.of_json kappa_instance_of_dummy
                     (JsonUtil.to_int ?error_msg:None) x))
              (List.assoc "algs" l);
          observables = (match List.assoc "observables" l with
              | `List o ->
                Tools.array_map_of_list
                  (fun x -> Location.dummy_annot
                      (Alg_expr.of_json kappa_instance_of_dummy
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
          cc_of_unaries = Connected_component.Set.empty;
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
