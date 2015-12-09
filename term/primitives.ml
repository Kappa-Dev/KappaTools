module Transformation =
  struct
    type t =
      | Agent of Agent_place.t
      | Freed of Agent_place.t * int
      | Linked of (Agent_place.t * int) * (Agent_place.t * int)
      | PositiveInternalized of Agent_place.t * int * int
      | NegativeInternalized of Agent_place.t * int

    let rename wk id cc inj = function
      | Freed (p,s) as x ->
	 let p' = Agent_place.rename wk id cc inj p in
	 if p == p' then x else Freed (p',s)
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

    let print ?sigs f = function
      | Agent p ->
	 Format.fprintf f "@[%a@]" (Agent_place.print ?sigs) p
      | Freed (p,s) ->
	 Format.fprintf
	   f "@[%a.%a = %t@]" (Agent_place.print ?sigs) p
	   (Agent_place.print_site ?sigs p) s Pp.bottom
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
  end

type elementary_rule = {
  rate : Alg_expr.t;
  unary_rate : Alg_expr.t option;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  syntactic_rule : int;
  (** negative number [n] means opposite of rule |[n]|,
[0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
}

type modification =
    ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr Location.annot list
  | STOP of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOW of string option * Connected_component.t array *
	       Instantiation.abstract Instantiation.test list
  | FLUX of Alg_expr.t Ast.print_expr Location.annot list
  | FLUXOFF of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOWOFF of Connected_component.t array
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr Location.annot list *
	 Alg_expr.t Ast.print_expr Location.annot list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr option;
      stopping_time : Nbr.t list
    }
