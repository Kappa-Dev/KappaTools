module type Set_with_logs =
  sig
    type elt
    type t

    val empty: t
    val is_empty: t -> bool
    val singleton: elt -> t
    val is_singleton: t -> bool

   
    val add: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> t -> Exception.method_handler  * t 
    val add_when_not_in: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> t -> Exception.method_handler  * t 
    val remove:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> t -> Exception.method_handler  * t 			      

    val minus: Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> t -> Exception.method_handler  * t
    val union: Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> t -> Exception.method_handler  * t 
    val disjoint_union: Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> t -> Exception.method_handler  * t 
    val inter: Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> t -> Exception.method_handler  * t
    val diff:  Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> t -> Exception.method_handler  * t
    val cardinal: t -> int

    val mem: elt -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val for_all: (elt -> bool) -> t -> bool
    val partition: (elt -> bool) -> t -> t * t

    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool

    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_inv: (elt -> 'a -> 'a) -> t -> 'a -> 'a

    val elements: t -> elt list

    val choose: t -> elt option
    val min_elt: t -> elt option
    val max_elt: t -> elt option
  end

module type Map_with_logs =
  sig
    type elt
    type set
    type +'a t

    val empty: 'a t
    val is_empty: 'a t -> bool
    val min_elt: (elt -> 'a -> bool) -> 'a t -> elt option
    val mem: elt -> 'a t -> bool 			      
    val find_option:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a t -> Exception.method_handler  * 'a option
    val find_default:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> elt -> 'a t -> Exception.method_handler  * 'a
    val find_default_without_logs: Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> elt -> 'a t -> Exception.method_handler  * 'a
    val find_option_without_logs: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a t -> Exception.method_handler  * 'a option
    val add: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a -> 'a t -> Exception.method_handler  * 'a t
    val overwrite: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a -> 'a t -> Exception.method_handler  * 'a t
    val add_or_overwrite:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a -> 'a t -> Exception.method_handler  * 'a t						   
    val remove: Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a t -> Exception.method_handler  * 'a t
    val remove_or_not: 	Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a t -> Exception.method_handler  * 'a t    							 
    val update: Remanent_parameters_sig.parameters -> Exception.method_handler   -> 'a t -> 'a t -> Exception.method_handler  * 'a t    
    val map2:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> Exception.method_handler  * 'a) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> Exception.method_handler  *  'a) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> 'a -> Exception.method_handler  * 'a) -> 'a t -> 'a t -> Exception.method_handler  * 'a t
    val map2z:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a -> 'a -> Exception.method_handler  * 'a) -> 'a t -> 'a t -> Exception.method_handler  * 'a t 
    val fold2z: Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a  -> 'b  -> 'c   -> (Exception.method_handler  * 'c)) -> 'a t -> 'b t -> 'c -> Exception.method_handler  * 'c 
    val fold2:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a   -> 'c  -> Exception.method_handler  * 'c) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'b  ->  'c  -> Exception.method_handler  * 'c) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a -> 'b  -> 'c  -> Exception.method_handler  * 'c) ->  'a t -> 'b t -> 'c -> Exception.method_handler  * 'c 
  
    val fold2_sparse:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a  -> 'b  -> 'c  -> (Exception.method_handler  * 'c)) ->  'a t -> 'b t -> 'c -> Exception.method_handler  * 'c
    val iter2_sparse:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a  -> 'b  -> Exception.method_handler )->  'a t -> 'b t -> Exception.method_handler  
    val diff:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a t -> 'a t -> Exception.method_handler  * 'a t * 'a t 
    val diff_pred:  Remanent_parameters_sig.parameters -> Exception.method_handler  -> ('a -> 'a -> bool) -> 'a t -> 'a t -> Exception.method_handler  * 'a t * 'a t 
    val merge: Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a t -> 'a t -> Exception.method_handler  * 'a t
    val union: Remanent_parameters_sig.parameters -> Exception.method_handler  -> 'a t -> 'a t -> Exception.method_handler  * 'a t
    val fold_restriction: Remanent_parameters_sig.parameters -> Exception.method_handler  -> (elt -> 'a -> (Exception.method_handler  * 'b) -> (Exception.method_handler * 'b)) -> set -> 'a t -> 'b -> Exception.method_handler  * 'b 	
    val fold_restriction_with_missing_associations: Remanent_parameters_sig.parameters -> Exception.method_handler  -> (elt -> 'a -> (Exception.method_handler  * 'b) -> (Exception.method_handler * 'b)) -> (elt -> (Exception.method_handler * 'b) -> (Exception.method_handler * 'b)) -> set -> 'a t -> 'b -> Exception.method_handler  * 'b 	
    val iter: (elt -> 'a -> unit) -> 'a t -> unit
    val iter2: Remanent_parameters_sig.parameters -> Exception.method_handler  ->  (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a  -> Exception.method_handler ) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'b  -> Exception.method_handler ) -> (Remanent_parameters_sig.parameters -> Exception.method_handler  -> elt -> 'a  -> 'b  -> Exception.method_handler )->  'a t -> 'b t -> Exception.method_handler
																								   val fold: (elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val mapi: (elt -> 'a -> 'b) -> 'a t -> 'b t
    val map: ('a -> 'b) -> 'a t -> 'b t 
    val for_all: (elt -> 'a -> bool) -> 'a t -> bool
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val bindings : 'a t -> (elt * 'a) list
  end

module type S_with_logs = sig
    type elt
    module Set : Set_with_logs with type elt = elt
    module Map : Map_with_logs with type elt = elt and type set = Set.t
  end
			    
module Make(S_both:SetMap.S): S_with_logs
       with type elt = S_both.elt
	and type 'a Map.t = 'a S_both.Map.t
	and type Set.t = S_both.Set.t
	and type Map.elt = S_both.elt
	and type Set.elt = S_both.elt
							    
module type Projection = sig
  
    type elt_a
    type elt_b
    type 'a map_a
    type 'a map_b
    val monadic_proj: (Remanent_parameters_sig.parameters -> Exception.method_handler -> elt_a -> Exception.method_handler * elt_b) -> Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a -> 'a -> Exception.method_handler * 'a) -> 'a map_a -> Exception.method_handler * 'a map_b
    val proj: (elt_a -> elt_b) -> Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a -> ('a -> 'a -> 'a) -> 'a map_a -> Exception.method_handler * 'a map_b
  end

module Proj(A:S_with_logs)(B:S_with_logs) : Projection
       with type elt_a = A.elt
	and type elt_b = B.elt
	and type 'a map_a = 'a A.Map.t
	and type 'a map_b = 'a B.Map.t 

module type Projection2 =
  sig
    type elt_a
    type elt_b
    type elt_c
    type 'a map_a
    type 'a map_b
    type 'a map_c

    val proj2:
      Remanent_parameters_sig.parameters -> Exception.method_handler ->
      (elt_a -> elt_b) -> 
      (elt_a -> elt_c) ->
      'a ->
      (Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a -> 'a -> 'a) ->
      'a map_a ->
      'a map_c map_b

    val proj2_monadic:
      Remanent_parameters_sig.parameters -> Exception.method_handler ->
      (elt_a -> elt_b) -> 
      (elt_a -> elt_c) ->
      'a ->
      (Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a -> 'a ->
       Exception.method_handler * 'a) ->
      'a map_a -> Exception.method_handler * 'a map_c map_b
  end
  
module Proj2 (A:S_with_logs)(B:S_with_logs)(C:S_with_logs) : Projection2
  with type elt_a = A.elt
  and type elt_b = B.elt
  and type elt_c = C.elt
  and type 'a map_a = 'a A.Map.t
  and type 'a map_b = 'a B.Map.t
  and type 'a map_c = 'a C.Map.t
