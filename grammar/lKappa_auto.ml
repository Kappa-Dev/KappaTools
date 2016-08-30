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
  | Regular of PropertiesCache.hashed_list * BindingCache.hashed_list
  | Back_to of int

module Node =
struct
  type t = cannonic_node
  let compare = compare
  let print log =
    function
    | Regular _ -> Format.fprintf log "Regular"
    | Back_to i -> Format.fprintf log "Back_to(%i)" i
end

module CannonicCache = Hashed_list.Make(Node)

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

let id =
  function
  | Ast.LNK_VALUE (i,_) -> Ast.LNK_VALUE (i,())
  | Ast.FREE -> Ast.FREE
  | Ast.LNK_ANY -> Ast.LNK_ANY
  | Ast.LNK_SOME -> Ast.LNK_SOME
  | Ast.LNK_TYPE (a,b) -> Ast.LNK_TYPE (a,b)

let translate cache lkappa_mixture =
  let add_map i j map =
    Mods.IntMap.add
      i (j::(Mods.IntMap.find_default [] i map)) map
  in
  let lkappa_array = Array.of_list lkappa_mixture in
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
  let translate_agent cache agent_id agent =
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
         let cache, ag = translate_agent cache ag_id agent in
         let () = array.(ag_id)<-ag in
         cache, ag_id+1)
      (cache, 0)
      lkappa_array
  in
  cache, array, bonds_map


let _ = translate
let _ = Regular (PropertiesCache.empty,BindingCache.empty)
let _ = Back_to 0
let nauto cache _mixture = cache, 1
