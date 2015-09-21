module Transformation =
  struct
    type t =
	Freed of Place.t * int
      | Linked of (Place.t * int) * (Place.t * int)
      | Internalized of Place.t * int * int

    let rename wk id cc inj = function
      | Freed (p,s) as x ->
	 let p' = Place.rename wk id cc inj p in
	 if p == p' then x else Freed (p',s)
      | Linked ((p1,s1),(p2,s2)) as x ->
	 let p1' = Place.rename wk id cc inj p1 in
	 let p2' = Place.rename wk id cc inj p2 in
	 if p1 == p1' && p2 == p2' then x else Linked ((p1',s1),(p2',s2))
      | Internalized (p,s,i) as x ->
	 let p' = Place.rename wk id cc inj p in
	 if p == p' then x else Internalized (p',s,i)

    let print ?sigs f = function
      | Freed (p,s) ->
	 Format.fprintf
	   f "@[%a.%a = %t@]" (Place.print ?sigs) p
	   (Place.print_site ?sigs p) s Pp.bottom
      | Linked ((p1,s1),(p2,s2)) ->
	 Format.fprintf
	   f "@[%a.%a = %a.%a@]"
	   (Place.print ?sigs) p1 (Place.print_site ?sigs p1) s1
	   (Place.print ?sigs) p2 (Place.print_site ?sigs p2) s2
      | Internalized (p,s,i) ->
	 Format.fprintf
	   f "@[%a.%a =@]" (Place.print ?sigs) p
	   (Place.print_internal ?sigs p s) i
  end

type elementary_rule = {
  rate : Alg_expr.t;
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
  | UPDATE of Operator.rev_dep * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr Location.annot list
  | STOP of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOW of
      Connected_component.t * Instantiation.abstract Instantiation.test list
  | FLUX of Alg_expr.t Ast.print_expr Location.annot list
  | FLUXOFF of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOWOFF of Connected_component.t
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
