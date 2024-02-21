module type Hash = sig
  type hashed_list
  type elt
  type cache

  val int_of_hashed_list : hashed_list -> int
  val compare : hashed_list -> hashed_list -> int
  val init : unit -> cache
  val hash : cache -> elt list -> cache * hashed_list
  val cons : cache -> elt -> hashed_list -> cache * hashed_list
  val empty : hashed_list
  val print : Format.formatter -> hashed_list -> unit
  val print_cache : Format.formatter -> cache -> unit
end

module Make =
functor
  (A : SetMap.OrderedType)
  ->
  struct
    type elt = A.t
    type elt_id = int
    type hashed_list = int

    let int_of_hashed_list (h : hashed_list) : int = h
    let compare = compare

    module SetMap = SetMap.Make (A)

    type cache = {
      dictionary: elt_id SetMap.Map.t;
      next_elt_id: elt_id;
      cons: hashed_list option Mods.DynArray.t option Mods.DynArray.t;
      next_list_id: hashed_list;
    }

    let fst_elt_id = 1
    let next_elt_id = succ

    let fresh_elt_id cache =
      ( cache.next_elt_id,
        { cache with next_elt_id = next_elt_id cache.next_elt_id } )

    let fst_list_id = 1
    let next_list_id = succ

    let fresh_list_id cache =
      ( { cache with next_list_id = next_list_id cache.next_list_id },
        cache.next_list_id )

    let init () =
      {
        dictionary = SetMap.Map.empty;
        next_elt_id = fst_elt_id;
        cons = Mods.DynArray.create 0 None;
        next_list_id = fst_list_id;
      }

    let empty = 0

    let hash_elt cache elt =
      match SetMap.Map.find_option elt cache.dictionary with
      | Some i -> cache, i
      | None ->
        let id, cache = fresh_elt_id cache in
        { cache with dictionary = SetMap.Map.add elt id cache.dictionary }, id

    let cons cache head tail =
      let cache, hash_head = hash_elt cache head in
      let subtab =
        match Mods.DynArray.get cache.cons hash_head with
        | Some subtab -> subtab
        | None ->
          let subtab = Mods.DynArray.create 0 None in
          let () = Mods.DynArray.set cache.cons hash_head (Some subtab) in
          subtab
      in
      match Mods.DynArray.get subtab tail with
      | Some hash -> cache, hash
      | None ->
        let cache, hash = fresh_list_id cache in
        let () = Mods.DynArray.set subtab tail (Some hash) in
        cache, hash

    let rec hash cache list =
      match list with
      | [] -> cache, empty
      | h :: t ->
        let cache, t = hash cache t in
        cons cache h t

    let print formatter = Format.fprintf formatter "%i"

    let print_cache formatter cache =
      let () =
        Format.fprintf formatter
          "Cache\n next_fresh_list_id: %i; next_fresh_elt_id: %i\n"
          cache.next_list_id cache.next_elt_id
      in
      let () =
        SetMap.Map.iter
          (fun a i -> Format.fprintf formatter "DIC:%a:%i\n" A.print a i)
          cache.dictionary
      in
      Mods.DynArray.iteri
        (fun a opt ->
          match opt with
          | None -> ()
          | Some opt ->
            Mods.DynArray.iteri
              (fun b k ->
                match k with
                | None -> ()
                | Some k -> Format.fprintf formatter "(%i,%i)->%i \n" a b k)
              opt)
        cache.cons
  end
