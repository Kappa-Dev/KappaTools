type t = {
  signatures : Signature.s;
  tokens : unit NamedDecls.t;
  algs : (Alg_expr.t Location.annot) NamedDecls.t;
  observables : Alg_expr.t Location.annot array;
  rules  :Primitives.elementary_rule NamedDecls.t;
  unary_rules : Alg_expr.t Mods.IntMap.t;
  perturbations : Primitives.perturbation array;
  need_update_each_loop : Operator.DepSet.t;
  reverse_dependencies : Operator.DepSet.t array;
  desc_table : (string,out_channel * Format.formatter) Hashtbl.t;
}

let init sigs tokens algs (deps_in_t,deps_in_e,rd) rules urules obs perts =
  { signatures = sigs; tokens = tokens;
    rules = rules; unary_rules = urules; algs = algs; observables = obs;
    perturbations = perts; reverse_dependencies = rd;
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

let num_of_rule s env = NamedDecls.elt_id ~kind:"rule" env.rules s
let get_rule env i = snd env.rules.NamedDecls.decls.(i)
let nb_rules env = NamedDecls.size env.rules

let num_of_alg s env = NamedDecls.elt_id ~kind:"variable" env.rules s
let get_alg env i = fst @@ snd env.algs.NamedDecls.decls.(i)
let nb_algs env = NamedDecls.size env.algs

let num_of_token str env =
  NamedDecls.elt_id ~kind:"token" env.rules str
let nb_tokens env =
  NamedDecls.size env.tokens

let get_perturbation env i = env.perturbations.(i)
let nb_perturbations env = Array.length env.perturbations

let get_reverse_dependencies env i = env.reverse_dependencies.(i)
let get_always_outdated env = env.need_update_each_loop

let print_agent ?env f i =
  match env with
  | None -> Format.fprintf f "__agent_%i" i
  | Some env ->
     Signature.print_agent env.signatures f i
let print_rule ?env f id =
  match env with
  | None -> Format.fprintf f "__rule_%i" id
  | Some env ->
     Format.fprintf f "%s" (NamedDecls.elt_name env.rules id)
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

let map_observables f env =
  Array.map (fun (x,_) -> f x) env.observables
let iteri_rules f env =
    Array.iteri (fun i (_,rule) -> f i rule) env.rules.NamedDecls.decls

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
    (NamedDecls.print
       ~sep:Pp.space
       (fun i n f r -> Format.fprintf f "@[<2>%i:%s:@ %a@]" i n (pr_rule env) r))
    env.rules
    (Pp.array Pp.space (fun i f p ->
			Format.fprintf f "@[<2>/*%i*/%a@]" i (pr_pert env) p))
    env.perturbations
(*
  desc_table : (string,out_channel * Format.formatter) Hashtbl.t;
 *)
