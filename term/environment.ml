type t = {
  signatures : Signature.s;
  tokens : unit NamedDecls.t;
  algs : (Alg_expr.t Location.annot) NamedDecls.t;
  observables : Alg_expr.t Location.annot array;
  ast_rules : (string Location.annot option * LKappa.rule Location.annot) array;
  rules : Primitives.elementary_rule array;
  cc_of_unaries : Connected_component.Set.t;
  perturbations : Primitives.perturbation array;
  need_update_each_loop : Operator.DepSet.t;
  algs_reverse_dependencies : Operator.DepSet.t array;
  tokens_reverse_dependencies : Operator.DepSet.t array;
  desc_table : (string,out_channel * Format.formatter) Hashtbl.t;
}

let init sigs tokens algs (deps_in_t,deps_in_e,tok_rd,alg_rd)
	 (ast_rules,rules,cc_of_unaries) obs perts =
  { signatures = sigs; tokens = tokens; ast_rules = ast_rules;
    rules = rules; cc_of_unaries = cc_of_unaries; algs = algs;
    observables = obs; perturbations = perts;
    algs_reverse_dependencies = alg_rd; tokens_reverse_dependencies = tok_rd;
    need_update_each_loop = Operator.DepSet.union deps_in_t deps_in_e;

    desc_table = Hashtbl.create 2;
  }

let signatures env = env.signatures

let get_desc file env =
  try snd (Hashtbl.find env.desc_table file)
  with Not_found ->
       let d_chan = Kappa_files.open_out file in
    let d = Format.formatter_of_out_channel d_chan in
    (Hashtbl.add env.desc_table file (d_chan,d) ; d)

let close_desc env =
	Hashtbl.iter (fun _file (d_chan,d) ->
		      let () = Format.pp_print_newline d () in
		      close_out d_chan) env.desc_table

let num_of_agent nme env =
  Signature.num_of_agent nme env.signatures

let fold_rules f x env =
  Tools.array_fold_lefti
    (fun i x rule -> f i x rule) x env.rules

let get_rule env i = env.rules.(i)
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
let nb_algs env = NamedDecls.size env.algs

let num_of_token str env = NamedDecls.elt_id ~kind:"token" env.tokens str
let nb_tokens env = NamedDecls.size env.tokens

let get_perturbation env i = env.perturbations.(i)
let nb_perturbations env = Array.length env.perturbations

let get_alg_reverse_dependencies env i = env.algs_reverse_dependencies.(i)
let get_token_reverse_dependencies env i = env.tokens_reverse_dependencies.(i)
let get_always_outdated env = env.need_update_each_loop

let print_agent ?env f i =
  match env with
  | None -> Format.fprintf f "__agent_%i" i
  | Some env ->
     Signature.print_agent env.signatures f i
let print_alg ?env f id =
  match env with
  | None -> Format.fprintf f "'__alg_%i'" id
  | Some env ->
     Format.fprintf f "'%s'" (NamedDecls.elt_name env.algs id)
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
     if i = 0 then Format.pp_print_string f "A generated rule for perturbations"
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
