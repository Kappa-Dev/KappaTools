type contact_map = ((int list) * (int*int) list) array array

module Transformation =
  struct
    type 'a t =
      | Agent of 'a
      | Freed of 'a Instantiation.site
      | Linked of 'a Instantiation.site * 'a Instantiation.site
      | NegativeWhatEver of 'a Instantiation.site
      | PositiveInternalized of
	  'a * Instantiation.site_name * Instantiation.internal_state
      | NegativeInternalized of 'a Instantiation.site

    let rename wk id cc inj = function
      | Freed (p,s) as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else Freed (p',s)
      | NegativeWhatEver (p,s) as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else NegativeWhatEver (p',s)
      | Linked ((p1,s1),(p2,s2)) as x ->
	 let p1' = Agent_place.rename wk id cc inj p1 in
	 let p2' = Agent_place.rename wk id cc inj p2 in
	 if p1 == p1' && p2 == p2' then x else Linked ((p1',s1),(p2',s2))
      | PositiveInternalized (p,s,i) as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else PositiveInternalized (p',s,i)
      | NegativeInternalized (p,s) as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else NegativeInternalized (p',s)
      | Agent p as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else Agent p'

    let concretize inj2graph = function
      | Agent n -> Agent (Agent_place.concretize inj2graph n)
      | Freed (n,s) -> Freed (Agent_place.concretize inj2graph n,s)
      | Linked ((n,s),(n',s')) ->
	 Linked ((Agent_place.concretize inj2graph n,s),
		 (Agent_place.concretize inj2graph n',s'))
      | NegativeWhatEver (n,s) ->
	 NegativeWhatEver (Agent_place.concretize inj2graph n,s)
      | PositiveInternalized (n,s,i) ->
	 PositiveInternalized (Agent_place.concretize inj2graph n,s,i)
      | NegativeInternalized (n,s) ->
	 NegativeInternalized (Agent_place.concretize inj2graph n,s)

    let print ?sigs f = function
      | Agent p ->
	 Format.fprintf f "@[%a@]" (Agent_place.print ?sigs) p
      | Freed (p,s) ->
	 Format.fprintf
	   f "@[%a.%a = %t@]" (Agent_place.print ?sigs) p
	   (Agent_place.print_site ?sigs p) s Pp.bottom
      | NegativeWhatEver (p,s) ->
	 Format.fprintf
	   f "@[%a.%a = ???@]" (Agent_place.print ?sigs) p
	   (Agent_place.print_site ?sigs p) s
      | Linked ((p1,s1),(p2,s2)) ->
	 Format.fprintf
	   f "@[%a.%a = %a.%a@]"
	   (Agent_place.print ?sigs) p1 (Agent_place.print_site ?sigs p1) s1
	   (Agent_place.print ?sigs) p2 (Agent_place.print_site ?sigs p2) s2
      | PositiveInternalized (p,s,i) ->
	 Format.fprintf
	   f "@[%a.%a =@]" (Agent_place.print ?sigs) p
	   (Agent_place.print_internal ?sigs p s) i
      | NegativeInternalized (p,s) ->
	 Format.fprintf
	   f "@[%a.%a~ =@]" (Agent_place.print ?sigs) p
	   (Agent_place.print_site ?sigs p) s

    let fresh_bindings ~short_branch_agents =
      List.fold_left
	(fun unaries_to_expl ->
	 function
	 | (Freed _ | Agent _ | PositiveInternalized _) -> unaries_to_expl
	 | (NegativeWhatEver _ | NegativeInternalized _) -> assert false
	 | Linked ((n,s),(n',s')) ->
	    if Agent_place.same_connected_component n n'
	    then unaries_to_expl
            else (if List.mem (Agent_place.get_type n') short_branch_agents
                  then ((n',s'),(n,s))
                  else ((n,s),(n',s')))::unaries_to_expl) []
  end

type elementary_rule = {
  rate : Alg_expr.t Location.annot;
  unary_rate : (Alg_expr.t Location.annot * int option) option;
  connected_components : Connected_component.t array;
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  fresh_bindings :
    (Instantiation.abstract Instantiation.site *
       Instantiation.abstract Instantiation.site) list;
  consumed_tokens : (Alg_expr.t Location.annot * int) list;
  injected_tokens : (Alg_expr.t Location.annot * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
}

type modification =
    ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr list
  | STOP of Alg_expr.t Ast.print_expr list
  | CFLOW of string option * Connected_component.t array *
	       Instantiation.abstract Instantiation.test list
  | FLUX of bool * Alg_expr.t Ast.print_expr list
  | FLUXOFF of Alg_expr.t Ast.print_expr list
  | CFLOWOFF of Connected_component.t array
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr list *
	 Alg_expr.t Ast.print_expr list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr Location.annot;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr Location.annot option;
    }

let exists_modification check l =
  List.exists (fun p -> List.exists check p.effect) l

let map_expr_rule f x = {
    rate = f x.rate;
    unary_rate = Tools.option_map (fun (x,d) -> (f x,d)) x.unary_rate;
    connected_components = x.connected_components;
    removed = x.removed;
    inserted = x.inserted;
    fresh_bindings = x.fresh_bindings;
    consumed_tokens = List.map (fun (x,t) -> (f x,t)) x.consumed_tokens;
    injected_tokens = List.map (fun (x,t) -> (f x,t)) x.injected_tokens;
    syntactic_rule = x.syntactic_rule;
    instantiations = x.instantiations;
  }

let map_expr_print f x =
  List.map (function
	     | Ast.Str_pexpr _ as x -> x
	     | Ast.Alg_pexpr e -> Ast.Alg_pexpr (f e)) x

let map_expr_modification f = function
  | ITER_RULE (e,r) -> ITER_RULE (f e, map_expr_rule f r)
  | UPDATE (i,e) -> UPDATE (i,f e)
  | SNAPSHOT p -> SNAPSHOT (map_expr_print f p)
  | STOP p -> STOP (map_expr_print f p)
  | PRINT (fn,p) -> PRINT (map_expr_print f fn, map_expr_print f p)
  | FLUX (b,p) -> FLUX (b,map_expr_print f p)
  | FLUXOFF p -> FLUXOFF (map_expr_print f p)
  | (CFLOW _ | CFLOWOFF _ | PLOTENTRY) as x -> x

let map_expr_perturbation f_alg f_bool x =
  { precondition = f_bool x.precondition;
    effect = List.map (map_expr_modification f_alg) x.effect;
    abort = Tools.option_map f_bool x.abort;
  }

let stops_of_perturbation algs_deps x =
  let stopping_time =
    try Alg_expr.stops_of_bool_expr algs_deps (fst x.precondition)
    with ExceptionDefn.Unsatisfiable ->
      raise
	(ExceptionDefn.Malformed_Decl
	   ("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
	   ,snd x.precondition))
  in
  match x.abort with
  | None -> stopping_time
  | Some (x,pos) ->
     try stopping_time@Alg_expr.stops_of_bool_expr algs_deps x
     with ExceptionDefn.Unsatisfiable ->
      raise
	(ExceptionDefn.Malformed_Decl
	   ("Precondition of perturbation is using an invalid equality test on time, I was expecting a preconditon of the form [T]=n"
	   ,pos))
