module Place =
  struct
    type t =
	Existing of Connected_component.ContentAgent.t * int
      | Fresh of int * int (* type, id *)

    let rename wk id cc inj = function
      | Existing (n, id') as x ->
	 if id <> id' then x else
	   let n' = Connected_component.ContentAgent.rename wk cc inj n in
	   if n == n' then x else Existing (n',id')
      | Fresh _ as x -> x

    let print ?sigs f = function
      | Existing (n,id) ->
	 Format.fprintf f "%a/*%i*/"
			(Connected_component.ContentAgent.print ?sigs) n id
      | Fresh (ty,i) ->
	 Format.fprintf f "%a/*%t %i*/"
			(match sigs with
			 | None -> Format.pp_print_int
			 | Some sigs -> Signature.print_agent sigs) ty Pp.nu i

    let print_site ?sigs place f site =
      match place with
      | Existing (n,_) ->
	 Connected_component.ContentAgent.print_site ?sigs n f site
      | Fresh (ty,_) ->
	 match sigs with
	 | None -> Format.pp_print_int f ty
	 | Some sigs -> Signature.print_site sigs ty f site

    let print_internal ?sigs place site f id =
      match place with
      | Existing (n,_) ->
	 Connected_component.ContentAgent.print_internal ?sigs n site f id
      | Fresh (ty,_) ->
	 match sigs with
	 | None -> Format.pp_print_int f id
	 | Some sigs ->
	    Signature.print_site_internal_state sigs ty site f (Some id)

    let get_type = function
      | Existing (n,_) -> Connected_component.ContentAgent.get_sort n
      | Fresh (i,_) -> i
  end

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

module Instantiation =
  struct
    type agent_name = int
    type site_name = int
    type internal_state  = int

    type binding_type = agent_name * site_name

    type abstract = Place.t
    type concrete = int (*agent_id*) * agent_name

    type 'a site = 'a * site_name

    type 'a test =
      | Is_Here of 'a
      | Has_Internal of 'a site * internal_state
      | Is_Free of 'a site
      | Is_Bound of 'a site
      | Has_Binding_type of 'a site * binding_type
      | Is_Bound_to of 'a site * 'a site

    type 'a action =
      | Create of 'a * (site_name * internal_state option) list
      | Mod_internal of 'a site * internal_state
      | Bind of 'a site * 'a site
      | Free of 'a site
      | Remove of 'a

    let rename_abstract_test wk id cc inj = function
      | Is_Here pl as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Is_Here aux
      | Has_Internal ((pl,s),i) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Has_Internal ((aux,s),i)
      | Is_Free (pl,s) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Is_Free (aux,s)
      | Is_Bound (pl,s) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Is_Bound (aux,s)
      | Has_Binding_type ((pl,s),t) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Has_Binding_type ((aux,s),t)
      | Is_Bound_to ((pl,s),(pl',s')) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 let aux' = Place.rename wk id cc inj pl' in
	 if aux == pl && aux' == pl' then x else Is_Bound_to ((aux,s),(aux',s'))

    let rename_abstract_action wk id cc inj = function
      | Create (pl,i) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Create (aux,i)
      | Mod_internal ((pl,s),i) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Mod_internal ((aux,s),i)
      | Bind ((pl,s),(pl',s')) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 let aux' = Place.rename wk id cc inj pl' in
	 if aux == pl && aux' == pl' then x else Bind ((aux,s),(aux',s'))
      | Free (pl,s) as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Free (aux,s)
      | Remove pl as x ->
	 let aux = Place.rename wk id cc inj pl in
	 if aux == pl then x else Remove aux

    let concretize_test f = function
      | Is_Here pl -> Is_Here (f pl,Place.get_type pl)
      | Has_Internal ((pl,s),i) -> Has_Internal(((f pl,Place.get_type pl),s),i)
      | Is_Free (pl,s) -> Is_Free ((f pl,Place.get_type pl),s)
      | Is_Bound (pl,s) -> Is_Bound ((f pl,Place.get_type pl),s)
      | Has_Binding_type ((pl,s),t) ->
	 Has_Binding_type (((f pl,Place.get_type pl),s),t)
      | Is_Bound_to ((pl,s),(pl',s')) ->
	 Is_Bound_to (((f pl,Place.get_type pl),s),
		      ((f pl',Place.get_type pl'),s'))

    let concretize_action f = function
      | Create (pl,i) -> Create ((f pl,Place.get_type pl),i)
      | Mod_internal ((pl,s),i) -> Mod_internal (((f pl,Place.get_type pl),s),i)
      | Bind ((pl,s),(pl',s')) ->
	 Bind (((f pl,Place.get_type pl),s),((f pl',Place.get_type pl'),s'))
      | Free (pl,s) -> Free ((f pl,Place.get_type pl),s)
      | Remove pl -> Remove (f pl,Place.get_type pl)

    let abstract_action_of_transformation = function
      | Transformation.Freed (pl,s) -> Free (pl,s)
      | Transformation.Linked (x,y) -> Bind (x,y)
      | Transformation.Internalized (p,s,i) -> Mod_internal ((p,s),i)

    let with_sigs f = function
      | None -> Format.pp_print_int
      | Some sigs -> f sigs
    let print_concrete_agent ?sigs f (id,ty) =
      Format.fprintf
	f "%a_%i" (with_sigs Signature.print_agent sigs) ty id
    let print_concrete_agent_site ?sigs f ((_,ty as agent),id) =
      Format.fprintf f "%a.%a" (print_concrete_agent ?sigs) agent
		     (with_sigs (fun s -> Signature.print_site s ty) sigs) id
    let print_concrete_test ?sigs f = function
      | Is_Here agent ->
	 Format.fprintf f "Is_Here(%a)" (print_concrete_agent ?sigs) agent
      | Has_Internal (((_,ty),id as site),int) ->
	 Format.fprintf f "Has_Internal(%a~%a)"
			(print_concrete_agent_site ?sigs) site
			(with_sigs
			   (fun s -> Signature.print_internal_state s ty id)
			   sigs) int
      | Is_Free site ->
	 Format.fprintf f "Is_Free(%a)" (print_concrete_agent_site ?sigs) site
      | Is_Bound site ->
	 Format.fprintf f "Is_Bound(%a)" (print_concrete_agent_site ?sigs) site
      | Has_Binding_type (site,(ty,sid)) ->
	 Format.fprintf f "Btype(%a,%t)"
			(print_concrete_agent_site ?sigs) site
			(fun f ->
			 match sigs with
			 | None -> Format.fprintf f "%i.%i" ty sid
			 | Some sigs ->
			    Format.fprintf
			      f "%a.%a" (Signature.print_agent sigs) ty
			      (Signature.print_site sigs ty) sid)
      | Is_Bound_to (site1,site2) ->
	 Format.fprintf f "Is_Bound(%a,%a)"
			(print_concrete_agent_site ?sigs) site1
			(print_concrete_agent_site ?sigs) site2
    let print_concrete_action ?sigs f = function
      | Create ((_,ty as agent),list) ->
	 Format.fprintf
	   f "Create(%a[@[<h>%a@]])" (print_concrete_agent ?sigs) agent
	   (Pp.list Pp.comma
		    (fun f (x,y) ->
		     match sigs with
		     | Some sigs ->
			Signature.print_site_internal_state sigs ty x f y
		     | None ->
			match y with
			| None -> Format.pp_print_int f x
			| Some y ->
			   Format.fprintf f "%i.%i" x y))
	   list
      | Mod_internal (((_,ty),id as site),int) ->
	 Format.fprintf f "Mod(%a~%a)" (print_concrete_agent_site ?sigs) site
			(with_sigs
			   (fun s -> Signature.print_internal_state s ty id)
			   sigs) int
      | Bind (site1,site2) ->
	 Format.fprintf f "Bind(%a,%a)" (print_concrete_agent_site ?sigs) site1
			(print_concrete_agent_site ?sigs) site2
      | Free site ->
	 Format.fprintf f "Free(%a)" (print_concrete_agent_site ?sigs) site
      | Remove agent ->
	 Format.fprintf f "Remove(%a)" (print_concrete_agent ?sigs) agent

  end

type elementary_rule = {
  rate : Alg_expr.t;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  instantiations :
    Instantiation.abstract Instantiation.test list *
      Instantiation.abstract Instantiation.action list;
}

type modification =
    ITER_RULE of Alg_expr.t Term.with_pos * elementary_rule
  | UPDATE of Term.dep_type * Alg_expr.t Term.with_pos
  | SNAPSHOT of Alg_expr.t Ast.print_expr Term.with_pos list
  | STOP of Alg_expr.t Ast.print_expr Term.with_pos list
  | CFLOW of Connected_component.t
  | FLUX of Alg_expr.t Ast.print_expr Term.with_pos list
  | FLUXOFF of Alg_expr.t Ast.print_expr Term.with_pos list
  | CFLOWOFF of Connected_component.t
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr Term.with_pos list *
	 Alg_expr.t Ast.print_expr Term.with_pos list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr option;
      stopping_time : Nbr.t list
    }
