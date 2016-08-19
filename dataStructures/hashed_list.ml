module Hashed_list =
  functor (A:SetMap.OrderedType) ->
  struct
    type elt_id = int
    type hashed_list = int
    let compare = compare
    module SetMap = SetMap.Make(A) 
    type cache =
      {
        dictionary: elt_id SetMap.Map.t ;
        next_elt_id: elt_id ;
        cons: hashed_list option Mods.DynArray.t Mods.DynArray.t ;
        next_list_id: hashed_list;
      }

    let fst_elt_id = 1
    let next_elt_id = succ
    let fresh_elt_id cache =
      cache.next_elt_id,
      {cache with next_elt_id = next_elt_id cache.next_elt_id}

    let fst_list_id = 1
    let next_list_id = succ
    let fresh_list_id cache =
      {cache with next_list_id = next_list_id cache.next_list_id},
      cache.next_list_id
    let init () =
      {
        dictionary = SetMap.Map.empty ;
        next_elt_id = fst_elt_id ;
        cons = Mods.DynArray.make_matrix 0 0 None;
        next_list_id = fst_list_id;
      }

    let empty = 0
    let hash_elt cache elt =
      match
        SetMap.Map.find_option elt cache.dictionary
      with
      | Some i -> cache, i
      | None ->
        let id, cache = fresh_elt_id cache in
        {cache with dictionary = SetMap.Map.add elt id cache.dictionary}, id

    let cons cache head tail =
      let cache, hash_head = hash_elt cache head in
      match
        Mods.DynArray.get
          (Mods.DynArray.get cache.cons hash_head) tail
      with
      | Some hash -> cache, hash
      | None ->
        let cache, hash = fresh_list_id cache in
        let () =
          Mods.DynArray.set
            (Mods.DynArray.get cache.cons hash_head)
            tail
            (Some hash)
        in
        cache, hash

    let rec hash cache list  =
      match list with
      | [] -> cache,empty
      | h::t ->
        let cache, t = hash cache t in
        cons cache h t
  end

let main () = ()
