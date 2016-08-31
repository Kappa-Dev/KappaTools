module Binding_states =
struct
  type t = int * ((int, unit) Ast.link)
  let compare = compare
  let print log (a,b) =
    Format.fprintf log "%i:%a" a
      (fun log  ->
         Ast.print_link
           (fun _ f x -> Format.pp_print_int f x)
           (fun f x -> Format.pp_print_int f x)
           (fun _ () -> ()) log)
      b
end

module BindingCache = Hashed_list.Make(Binding_states)

module Int2 =
struct
  type t = int * int
  let compare = compare
  let print log (a,b) = Format.fprintf log "(%i,%i)" a b
end

module PropertiesCache = Hashed_list.Make(Int2)

type cannonic_node =
  | Regular of int * PropertiesCache.hashed_list * BindingCache.hashed_list
  | Back_to of int

module Node =
struct
  type t = cannonic_node
  let compare = compare
  let print log =
    function
    | Regular (i,a,b) -> Format.fprintf log "Regular (%i,%a,%a);" i PropertiesCache.print a BindingCache.print b
    | Back_to i -> Format.fprintf log "Back_to(%i);" i
end

module CannonicCache = Hashed_list.Make(Node)
module CannonicSet_and_map =
  SetMap.Make
    (struct
      type t = CannonicCache.hashed_list
      let compare = CannonicCache.compare
      let print _ _ = ()
    end)
module CannonicMap = CannonicSet_and_map.Map

type cache =
  {
    internal_state_cache: PropertiesCache.cache ;
    binding_state_cache: BindingCache.cache ;
    cannonic_cache: CannonicCache.cache
  }

let init_cache () =
  {
    internal_state_cache = PropertiesCache.init () ;
    binding_state_cache = BindingCache.init () ;
    cannonic_cache = CannonicCache.init ()
  }

(* id gets rid of location annotation *)
let id =
  function
  | Ast.LNK_VALUE (i,_) -> Ast.LNK_VALUE (i,())
  | Ast.FREE -> Ast.FREE
  | Ast.LNK_ANY -> Ast.LNK_ANY
  | Ast.LNK_SOME -> Ast.LNK_SOME
  | Ast.LNK_TYPE (a,b) -> Ast.LNK_TYPE (a,b)

(* This function translate a mixture into an array of views and a function
   mapping each binding site to its partnet *)
(* In views, any location annotation has been removed *)
(* Link values have been replaced with the corresponding binding type *)
(* Lastly, each agent is provided with its set of bond sites *)
let translate cache lkappa_mixture =
  let add_map i j map =
    Mods.IntMap.add
      i (j::(Mods.IntMap.find_default [] i map)) map
  in
  let lkappa_array = Array.of_list lkappa_mixture in
  let array_name = Array.make (Array.length lkappa_array) 0 in
  let state_of_internal x =
    match x with
    | LKappa.I_ANY
    | LKappa.I_ANY_CHANGED _
    | LKappa.I_ANY_ERASED -> None
    | LKappa.I_VAL_CHANGED (state, _)
    | LKappa.I_VAL_ERASED state -> Some state
  in
  let scan_bonds_identifier, _  =
    Array.fold_left
      (fun (map, agent_id) agent ->
         let () = array_name.(agent_id) <- agent.LKappa.ra_type in
         let map, _ =
           Array.fold_left
             (fun (map, site_id) ((state,_),_) ->
                match state with
                | Ast.LNK_VALUE (i,_) ->
                  (add_map i (agent_id, site_id) map, site_id + 1)
                | Ast.FREE
                | Ast.LNK_ANY
                | Ast.LNK_SOME
                | Ast.LNK_TYPE _ -> map, site_id + 1)
             (map, 0)
             agent.LKappa.ra_ports
         in
         (map, agent_id + 1))
      (Mods.IntMap.empty,0)
      lkappa_array
  in
  let bonds_map =
    Mods.IntMap.fold
      (fun _ list map ->
         match list
         with
         | [a,b;c,d] ->
           Mods.Int2Map.add (a,b) (c,d)
             (Mods.Int2Map.add (c,d) (a,b) map)
         | [] | _::_ -> assert false )
      scan_bonds_identifier
      Mods.Int2Map.empty
  in
  let translate_agent cache array_name agent_id agent =
    let agent_name = agent.LKappa.ra_type in
    let rule_internal,_ =
      Array.fold_left
        (fun (list,site_id) state ->
           match state_of_internal state with
           | None -> list, site_id +1
           | Some x -> (site_id,x)::list, site_id+1)
        ([],0) agent.LKappa.ra_ints
    in
    let rule_port,interface,_ =
      Array.fold_left
        (fun (list, interface, site_id) ((port,_),_) ->
           match port with
           | Ast.LNK_VALUE _  ->
             let ag_partner, site_partner =
               match
                 Mods.Int2Map.find_option (agent_id, site_id) bonds_map
               with
               | None -> assert false
               | Some x -> x
             in
             let ag_partner = array_name.(ag_partner) in
             (site_id,
              Ast.LNK_TYPE
                (site_partner,
                 ag_partner))
             ::list,
             Mods.IntSet.add site_id interface,site_id + 1
           | Ast.FREE
           | Ast.LNK_ANY
           | Ast.LNK_SOME ->
             (site_id,id port)::list, interface, site_id+1
           | Ast.LNK_TYPE (a,b) ->
             (site_id,
              Ast.LNK_TYPE
                (a,b))::list, interface,site_id+1
        )
        ([],Mods.IntSet.empty,0)
        agent.LKappa.ra_ports
    in
    let cache_prop = cache.internal_state_cache in
    let cache_binding = cache.binding_state_cache in
    let cache_prop, rule_internal =
      PropertiesCache.hash cache_prop rule_internal
    in
    let cache_binding, rule_port =
      BindingCache.hash cache_binding rule_port
    in
    {cache
     with internal_state_cache = cache_prop ;
          binding_state_cache = cache_binding},
    (agent_name, rule_internal, rule_port, interface)
  in
  let array =
    Array.make (Array.length lkappa_array) (0,PropertiesCache.empty,BindingCache.empty,Mods.IntSet.empty)
  in
  let cache, _ =
    Array.fold_left
      (fun (cache, ag_id) agent ->
         let cache, ag = translate_agent cache array_name ag_id agent in
         let () = array.(ag_id)<-ag in
         cache, ag_id+1)
      (cache, 0)
      lkappa_array
  in
  cache, array, bonds_map

(* the following function computes the cc of the agent ag_id *)
(* it outputs the list of agent_id in the same cc as agent ag_id *)
let extract_cc n bonds_map array ag_id =
  let seen = Array.make n false in
  let rec aux to_visit acc =
    match to_visit with
    | [] -> acc
    | head::tail when seen.(head) -> aux tail acc
    | head::tail ->
      let _,_,_,intf = array.(head) in
      let () = seen.(head) <- true in
      let to_visit =
        Mods.IntSet.fold
          (fun site_id to_visit ->
             match
               Mods.Int2Map.find_option (head,site_id) bonds_map
             with
             | None -> assert false
             | Some (a,_) -> a::to_visit)
          intf
          tail
      in
      aux to_visit (head::acc)
  in
  aux [ag_id] []

(* the following function decompose the mixture in cc *)
let decompose bonds_map array =
  let n = Array.length array in
  let rec aux k set =
    if k = n then set
    else aux (k+1) (Mods.IntSet.add k set)
  in
  let set = aux 0 Mods.IntSet.empty in
  let rec aux set acc =
    match Mods.IntSet.min_elt set
    with
    | None -> acc
    | Some min_elt ->
      let cc = extract_cc n bonds_map array min_elt in
      let set =
        List.fold_left
          (fun set elt -> Mods.IntSet.remove elt set)
          set cc
      in
      aux set (cc::acc)
  in
  aux set []

(* the following function does a depth-first exploration of a cc starting from the root *)
(* Only a spanning tree is explored, in case of cycles, a pointer to the position of the node in the list is given *)


let cannonical_of_root bonds_map array ag_id =
  let rec aux node_id stack (acc: 'a list) port_seen agent_seen =
    match stack
    with
    | [] -> acc
    | (ag_id,intf)::tail ->
      begin
        (* we currently explore agent ag_id *)
        (* the sites in intf not in port_seen remain to be explored *)
        match
          Mods.IntSet.min_elt intf
        with
        | None ->
          (* we are done with this agent *)
          (* we pop up the stack *)
          aux node_id tail acc port_seen agent_seen
        | Some s when Mods.Int2Set.mem (ag_id,s) port_seen
          ->
          (* next site, has been seen in a cycle *)
          (* we ignore it *)
          aux
            node_id
            ((ag_id,Mods.IntSet.remove s intf)::tail)
            acc
            port_seen agent_seen
        | Some s ->
          (* s is the next unvisited site *)
          (* we remove s from the stack *)
          let stack = (ag_id, Mods.IntSet.remove s intf)::tail in
          begin
            match
              Mods.Int2Map.find_option (ag_id, s) bonds_map
            with None ->
              (* pointers shall not be null *)
              let _ = assert false in
              []
               | Some (ag_id',s') ->
                 begin
                   match
                     Mods.IntMap.find_option ag_id' agent_seen
                   with
                   | None ->
                     (* this is the first time we see ag_id'*)
                     let agent_name,prop,binding,intf = array.(ag_id') in
                     let intf = Mods.IntSet.remove s' intf in
                     let agent_seen =
                       Mods.IntMap.add ag_id' node_id agent_seen
                     in
                     let stack = (ag_id',intf)::stack in
                     let acc =
                       Regular (agent_name,prop,binding)
                       :: acc
                     in
                     let node_id = node_id + 1 in
                     aux node_id stack acc port_seen agent_seen
                   | Some fst_pos ->
                     (* ag_id' has been seen at position fst_pos *)
                     let port_seen =
                       Mods.Int2Set.add (ag_id',s') port_seen
                     in
                     let acc =
                       Back_to fst_pos :: acc
                     in
                     let node_id = node_id + 1 in
                     aux node_id stack acc port_seen agent_seen
                 end
          end
      end
  in
  let agent_name,prop,binding,intf = array.(ag_id) in
  aux 0
    [ag_id,intf]
    [Regular (agent_name, prop, binding)]
    Mods.Int2Set.empty Mods.IntMap.empty


let mixture_to_species_map cache lkappa_mixture =
  let map = CannonicMap.empty in
  let cache, array, bonds_map = translate cache lkappa_mixture in
  let cc_list = decompose bonds_map array in
  let cannonic_cache, map =
    List.fold_left
      (fun (cache, map) cc ->
         match cc with
         | [] -> cache, map
         | h::t ->
           let _,occs =
             List.fold_left
               (fun (best,occs) i ->
                  let cmp = compare array.(i) best in
                  if cmp < 0
                  then (array.(i),[i])
                  else if cmp = 0
                  then (best,i::occs)
                  else (best,occs))
               (array.(h),[h]) t
           in
           let occs =
             List.rev_map
               (cannonical_of_root bonds_map array)
               occs
           in
           let cache, cannonic, nauto =
             match occs with
             | [] -> assert false
             | h::t ->
               let cache, hash = CannonicCache.hash cache h in
               List.fold_left
                 (fun (cache, cannonic, nauto) list ->
                    let cache, hash = CannonicCache.hash cache list in
                    let cmp = compare cannonic hash in
                    if cmp < 0
                    then cache, cannonic, nauto
                    else if cmp = 0
                    then cache, cannonic, nauto+1
                    else cache, hash, 1)
                 (cache, hash, 1)
                 t
           in
           match
             CannonicMap.find_option cannonic map
           with
           | None -> cache, CannonicMap.add cannonic (1,nauto) map
           | Some (occ, nauto') when nauto = nauto' ->
             cache, CannonicMap.add cannonic (occ+1,nauto) map
           | Some _ -> assert false)
      (cache.cannonic_cache, map)
      cc_list
  in
  {cache with cannonic_cache = cannonic_cache},
  map

let nauto_kind nauto nocc =
  let rec aux k acc =
    if k=0 then acc
    else
      aux (k-1) acc*k*nauto
  in
  aux nocc 1

let nauto_of_map map =
  CannonicMap.fold
    (fun _ (nocc,nauto) acc ->
       acc * (nauto_kind nauto nocc))
    map
    1

let nauto cache lkappa_mixture =
  let cache, map = mixture_to_species_map cache lkappa_mixture in
  cache, nauto_of_map map
