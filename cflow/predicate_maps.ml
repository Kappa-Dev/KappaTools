type predicate_value =
       | Internal_state_is of int
       | Undefined (** the wire does not exist yet *)
       | Present   (** for agent presence *)
       | Free      (** for binding sites *)
       | Bound_to of int * Instantiation.agent_name * Instantiation.site_name   (** for binding sites *)


module A = Mods.DynArray
type predicate_info =
| Here of int
| Bound_site of int * Instantiation.site_name
| Internal_state of int * Instantiation.site_name

module PredicateSetMap =
       SetMap.Make (struct type t = predicate_info let compare = compare end)

module PredicateMap = PredicateSetMap.Map


let string_of_predicate_info pi =
  match
    pi
  with
  | Here ag -> "Here "^(string_of_int ag)
  | Bound_site (ag,s) -> "Bound_state "^(string_of_int ag)^" "^(string_of_int s)
  | Internal_state (ag,s) -> "Internal_state "^(string_of_int ag)^" "^(string_of_int s)

module QPredicateMap =
struct
  type 'a t = 'a PredicateMap.t A.t

  let empty n = A.make n PredicateMap.empty

  let iter f t =
    A.iter
      (PredicateMap.iter f)
      t

  let hash predicate =
    match
      predicate
    with
    | Here ag
    | Bound_site (ag,_)
    | Internal_state (ag,_) -> ag

  let lift f predicate_id tab =
    f predicate_id (A.get tab (hash predicate_id))

  let find_default def p t = lift (PredicateMap.find_default def) p t
  let find_option p t = lift (PredicateMap.find_option) p t
  let mem p t = lift (PredicateMap.mem) p t

  let add predicate_id x tab =
    let hash = hash predicate_id in
    let old = A.get tab hash in
    let _ = A.set tab hash (PredicateMap.add predicate_id x old) in
    tab

  let recycle tab i =
    A.set tab i PredicateMap.empty

end

 module MPredicateMap =
       struct
	 type 'a t = 'a PredicateMap.t

	 let predicate_max parameter handler info error list = error,info,0
	 let empty n = PredicateMap.empty

	 let iter = PredicateMap.iter
	 let find_default = PredicateMap.find_default
	 let find_option = PredicateMap.find_option
	 let mem = PredicateMap.mem
	 let add = PredicateMap.add
       end


 module CPredicateMap = MPredicateMap

