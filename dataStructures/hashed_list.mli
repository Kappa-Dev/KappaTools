module Make (A:SetMap.OrderedType) :
sig
  type cache
  type hashed_list

  val compare: hashed_list -> hashed_list -> int
  val init: unit -> cache
  val hash:
     cache -> A.t list -> cache * hashed_list
  val cons: cache -> A.t -> hashed_list  -> cache * hashed_list
  val empty: hashed_list
  val print: Format.formatter -> hashed_list -> unit
  val print_cache: Format.formatter -> cache -> unit 
end
