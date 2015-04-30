type t = {
  stopping_times : (Nbr.t * int) list;
  perturbations_alive : bool array;
  activities : Random_tree.tree;
  variables_overwrite: Expr.alg_expr option array;
}

let initial env counter graph stopping_times =
  let activity_tree =
    Random_tree.create (NamedDecls.size env.Environment.rules) in
  let initial_value e =
    Nbr.to_float
      (Rule_interpreter.value_alg
	 ~get_alg:(fun i -> fst (snd env.Environment.algs.NamedDecls.decls.(i)))
	 counter graph e) in
  let () =
    Array.iteri
      (fun i (_,r) ->
       Random_tree.add i (initial_value r.Primitives.rate) activity_tree)
      env.Environment.rules.NamedDecls.decls in
  {
    stopping_times =
      List.sort (fun (a,_) (b,_) -> Nbr.compare a b) stopping_times;
    perturbations_alive =
      Array.make (NamedDecls.size env.Environment.perturbations) true;
    activities = activity_tree;
    variables_overwrite =
      Array.make (NamedDecls.size env.Environment.algs) None;
}

let get_alg env state i =
  match state.variables_overwrite.(i) with
  | None -> fst (snd env.Environment.algs.NamedDecls.decls.(i))
  | Some expr -> expr


let perturbate env counter state = ()
