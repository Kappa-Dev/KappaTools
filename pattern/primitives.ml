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

module Compilation_info =
  struct
    type t = {
      sites_tested_unmodified : (Place.t * int) list;
      sites_tested_modified : (Place.t * int) list;
      sites_untested_modified : (Place.t * int) list;
      internal_states_tested_unmodified : (Place.t * int) list;
      internal_states_tested_modified : (Place.t * int) list;
      internal_states_untested_modified : (Place.t * int) list;
    }

    let of_empty_rule =
      {
	sites_tested_unmodified = [];
	sites_tested_modified = [];
	sites_untested_modified = [];
	internal_states_tested_unmodified = [];
	internal_states_tested_modified = [];
	internal_states_untested_modified = [];
      }

    let rename_place wk id cc inj (pl,i as x) =
      let aux = Place.rename wk id cc inj pl in
      if aux == pl then x else (aux,i)

    let add_site_tested_only p i info =
      {
	sites_tested_unmodified = (p,i)::info.sites_tested_unmodified;
	sites_tested_modified = info.sites_tested_modified;
	sites_untested_modified = info.sites_untested_modified;
	internal_states_tested_unmodified =
	  info.internal_states_tested_unmodified;
	internal_states_tested_modified = info.internal_states_tested_modified;
	internal_states_untested_modified =
	  info.internal_states_untested_modified;
      }
    let add_site_modified ~tested p i info =
      {
	sites_tested_unmodified = info.sites_tested_unmodified;
	sites_tested_modified =
	  if tested then (p,i)::info.sites_tested_modified
	  else info.sites_tested_modified;
	sites_untested_modified =
	  if tested then info.sites_untested_modified
	  else (p,i)::info.sites_untested_modified;
	internal_states_tested_unmodified =
	  info.internal_states_tested_unmodified;
	internal_states_tested_modified = info.internal_states_tested_modified;
	internal_states_untested_modified =
	  info.internal_states_untested_modified;
      }
    let add_internal_state_tested_only p i info =
      {
	sites_tested_unmodified = info.sites_tested_unmodified;
	sites_tested_modified = info.sites_tested_modified;
	sites_untested_modified = info.sites_untested_modified;
	internal_states_tested_unmodified =
	  (p,i)::info.internal_states_tested_unmodified;
	internal_states_tested_modified = info.internal_states_tested_modified;
	internal_states_untested_modified =
	  info.internal_states_untested_modified;
      }
    let add_internal_state_modified ~tested p i info =
      {
	sites_tested_unmodified = info.sites_tested_unmodified;
	sites_tested_modified = info.sites_tested_modified;
	sites_untested_modified = info.sites_untested_modified;
	internal_states_tested_unmodified =
	  info.internal_states_tested_unmodified;
	internal_states_tested_modified =
	  if tested then (p,i)::info.internal_states_tested_modified
	  else info.internal_states_tested_modified;
	internal_states_untested_modified =
	  if tested then info.internal_states_untested_modified
	  else (p,i)::info.internal_states_untested_modified;
      }

    let rename wk id cc inj info =
      {
	sites_tested_unmodified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.sites_tested_unmodified;
	sites_tested_modified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.sites_tested_modified;
	sites_untested_modified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.sites_untested_modified;
	internal_states_tested_unmodified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.internal_states_tested_unmodified;
	internal_states_tested_modified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.internal_states_tested_modified;
	internal_states_untested_modified =
	  Tools.list_smart_map
	    (rename_place wk id cc inj) info.internal_states_untested_modified;
      }
  end

module Causality :
sig
  type t
  val empty : t
  val is_link_tested : t -> bool
  val is_link_modif : t -> bool
  val is_link_modif_side : t -> bool
  val is_link_something : t -> bool
  val is_internal_tested : t -> bool
  val is_internal_modif : t -> bool
  val is_internal_modif_side : t -> bool
  val is_internal_something : t -> bool
  val add_internal_tested : t -> t
  val add_internal_modif : t -> t
  val add_internal_modif_side : t -> t
  val add_link_tested : t -> t
  val add_link_modif : t -> t
  val add_link_modif_side : t -> t
end = struct
  type t = int
  let _INTERNAL_SIDE_EFFECT = 32
  let _INTERNAL_TESTED = 16
  let _INTERNAL_MODIF = 8
  let _LINK_SIDE_EFFECT = 4
  let _LINK_TESTED = 2
  let _LINK_MODIF = 1

  let empty = 0
  let is i c = (i land c <> 0)
  let is_link_tested = is _LINK_TESTED
  let is_link_modif = is _LINK_MODIF
  let is_link_modif_side = is _LINK_SIDE_EFFECT
  let is_link_something =
    is (_LINK_MODIF lor _LINK_TESTED lor _LINK_SIDE_EFFECT)
  let is_internal_tested = is _INTERNAL_TESTED
  let is_internal_modif = is _INTERNAL_MODIF
  let is_internal_modif_side = is _INTERNAL_SIDE_EFFECT
  let is_internal_something =
    is (_INTERNAL_MODIF lor _INTERNAL_TESTED lor _INTERNAL_SIDE_EFFECT)
  let add_internal_tested c = c lor _INTERNAL_TESTED
  let add_internal_modif c = c lor _INTERNAL_MODIF
  let add_internal_modif_side c = c lor _INTERNAL_SIDE_EFFECT
  let add_link_tested c = c lor _LINK_TESTED
  let add_link_modif c = c lor _LINK_MODIF
  let add_link_modif_side c = c lor _LINK_SIDE_EFFECT
end

type elementary_rule = {
  rate : Alg_expr.t;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  infos : Compilation_info.t;
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
