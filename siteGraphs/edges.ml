(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Edge = struct
  type t = Agent.t * int
  (** agent * site *)

  let _compare ((n,_),s) ((n',_),s') =
    let c = Mods.int_compare n n' in
    if c <> 0 then c else Mods.int_compare s s'

  (* let dummy_link = ((-1,-1),-1) *)
end

(* functions using the cache are responsible of reseting the cache at exit *)
module Cache = struct
  type t = {
    tests : int Mods.DynArray.t;
    bag : int Mods.DynArray.t;
    mutable limit : int;
  }

  let int_l = 31 (*Sys.int_size*)

  let create () = {
    tests = Mods.DynArray.make 1 0;
    bag = Mods.DynArray.make 1 0;
    limit = 0;
  }

  let mark t i =
    let x = i / int_l in
    let old = Mods.DynArray.get t.tests x in
    let () = if old = 0 then
        let () = Mods.DynArray.set t.bag t.limit x in
        t.limit <- succ t.limit in
    Mods.DynArray.set t.tests x (old lor (1 lsl (i mod int_l)))

  let test t i =
    (Mods.DynArray.get t.tests (i / int_l)) land (1 lsl (i mod int_l)) <> 0

  let reset t =
    let () =
      Tools.iteri
        (fun i -> Mods.DynArray.set t.tests (Mods.DynArray.get t.bag i) 0)
        t.limit in
    t.limit <- 0

  let iteri_reset f t =
    let () =
    Tools.iteri
      (fun k ->
         let i = Mods.DynArray.get t.bag k in
         let v = Mods.DynArray.get t.tests i in
         if v <> 0 then
           let acc = int_l * i in
           let () =
             Tools.iteri
               (fun j -> if v land (1 lsl j) <> 0 then f (acc+j))
               int_l in
           Mods.DynArray.set t.tests i 0)
      t.limit in
    t.limit <- 0
end

let glue_connected_component links cache ccs node1 node2 =
  let cc_id_op = Mods.DynArray.get ccs node2 in
  let rec explore_site id site next =
    if site = 0 then next else
      match (Mods.DynArray.get links id).(pred site) with
      | None -> explore_site id (pred site) next
      | Some ((id',_),_) ->
        if Mods.DynArray.get ccs id' = cc_id_op ||
           Cache.test cache id' then explore_site id (pred site) next
        else
          let () = Cache.mark cache id' in
          explore_site id (pred site) (id'::next) in
  let rec is_in_cc next = function
    | id ::todos ->
      is_in_cc
        (explore_site id (Array.length (Mods.DynArray.get links id)) next)
        todos
    | [] -> match next with
      | [] ->
        Cache.iteri_reset (fun i -> Mods.DynArray.set ccs i cc_id_op) cache
      | _ -> is_in_cc [] next in
  let () = Cache.mark cache node1 in
  is_in_cc [] [node1]

let separate_connected_component links cache ccs node1 node2 =
  let old_cc_id = Option_util.unsome (-1) (Mods.DynArray.get ccs node2) in
  let rec inspect_site dst id site next =
    if site = 0 then Some next else
      match (Mods.DynArray.get links id).(pred site) with
      | None -> inspect_site dst id (pred site) next
      | Some ((id',_),_) ->
        if id' = dst then None
        else if Cache.test cache id' then inspect_site dst id (pred site) next
        else
          let () = Cache.mark cache id' in
          inspect_site dst id (pred site) (id'::next) in
  let rec in_same_cc orig dst next = function
    | id ::todos ->
      begin match
          inspect_site dst id (Array.length (Mods.DynArray.get links id)) next with
      | None -> let () = Cache.reset cache in None
      | Some next' -> in_same_cc orig dst next' todos
      end
    | [] -> match next with
      | [] ->
        if Cache.test cache old_cc_id then
          let () = Cache.reset cache in
          let () = Cache.mark cache node2 in
          in_same_cc node2 node1 [] [node2]
        else
          let () =
            Cache.iteri_reset
              (fun i -> Mods.DynArray.set ccs i (Some orig)) cache in
          Some (old_cc_id,orig)
      | _ -> in_same_cc orig dst [] next in
  let () = Cache.mark cache node1 in
  in_same_cc node1 node2 [] [node1]

type t =
  {
    mutable outdated : bool;
    connect : Edge.t option array Mods.DynArray.t;
    missings : Mods.Int2Set.t;
    state : int option array Mods.DynArray.t;
    sort : int option Mods.DynArray.t;
    cache : Cache.t;
    free_id : int * int list;
    connected_component : int option Mods.DynArray.t option;
  }
(** (agent,site -> binding_state; missings);
    agent,site -> internal_state; agent -> sort; free_id
    the free sites are neither in missings nor in linking_destination *)

let empty ~with_connected_components =
  {
    outdated = false;
    connect = Mods.DynArray.make 1 [||];
    missings = Mods.Int2Set.empty;
    state = Mods.DynArray.make 1 [||];
    sort = Mods.DynArray.make 1 None;
    cache = Cache.create ();
    free_id =(0,[]);
    connected_component = if with_connected_components
      then Some (Mods.DynArray.make 1 None)
      else None;
  }

let copy graph =
  let () = assert (not graph.outdated) in
  let () = assert (Mods.Int2Set.is_empty graph.missings) in
  {
    outdated = false;
    connect = Mods.DynArray.map Array.copy graph.connect;
    missings = Mods.Int2Set.empty;
    state = Mods.DynArray.map Array.copy graph.state;
    sort = Mods.DynArray.copy graph.sort;
    cache = Cache.create ();
    free_id = graph.free_id;
    connected_component =
      (match graph.connected_component with
       | None -> None
       | Some ccs -> Some (Mods.DynArray.copy ccs));
  }

type stats = { nb_agents : int }

let stats graph = {
  nb_agents =
    Mods.DynArray.length graph.sort - List.length (snd graph.free_id);
}

let add_agent ?id sigs ty graph =
  let ar = Signature.arity sigs ty in
  let al = Array.make ar None in
  let ai = Array.make ar None in
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let h,free_id =
    match id with
    | Some id ->
      (id,
       let new_id,l = graph.free_id in
       if id < new_id then
         match List.partition (fun i -> i = id) l with
         | [ _ ], t -> (new_id,t)
         | _, _ ->
           raise
             (ExceptionDefn.Internal_Error
                (Locality.dummy_annot
                   ("Try to add an agent with a the free id "
                    ^string_of_int id)))
       else
         (succ id, Tools.recti (fun acc k -> (k+new_id)::acc) l (id-new_id))
      )
    | None -> match graph.free_id with
      | new_id,h :: t -> h,(new_id,t)
      | new_id,[] -> new_id,(succ new_id,[]) in
  let missings' = Tools.recti (fun a s -> Mods.Int2Set.add (h,s) a)
      graph.missings ar in
  let () = Mods.DynArray.set graph.connect h al in
  let () = Mods.DynArray.set graph.state h ai in
  let () = Mods.DynArray.set graph.sort h (Some ty) in
  let () = match graph.connected_component with
    | None -> ()
    | Some ccs -> Mods.DynArray.set ccs h (Some h) in
  h,
  {
    outdated = false;
    connect = graph.connect;
    missings = missings';
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id;
    connected_component = graph.connected_component;
  }

let add_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (Mods.DynArray.get graph.connect ag).(s) <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Mods.Int2Set.remove (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
    connected_component = graph.connected_component;
  }

let add_internal ag s i graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (Mods.DynArray.get graph.state ag).(s) <- Some i in
  {
    outdated = false;
    connect = graph.connect;
    missings = graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
    connected_component = graph.connected_component;
  }

let add_link (ag,ty) s (ag',ty') s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (Mods.DynArray.get graph.connect ag).(s) <- Some ((ag',ty'),s') in
  let () = (Mods.DynArray.get graph.connect ag').(s') <- Some ((ag,ty),s) in
  let out = match graph.connected_component with
    | None -> None
    | Some ccs ->
      let i = Option_util.unsome (-1) (Mods.DynArray.get ccs ag) in
      let j = Option_util.unsome (-2) (Mods.DynArray.get ccs ag') in
      if i = j then None else
        let () =
          glue_connected_component graph.connect graph.cache ccs ag ag' in
        Some (j,i) in
  {
    outdated = false;
    connect = graph.connect;
    missings =
      Mods.Int2Set.remove (ag,s) (Mods.Int2Set.remove (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
    connected_component = graph.connected_component;
  },out

let remove_agent ag graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = Mods.DynArray.set graph.connect ag [||] in
  let () = Mods.DynArray.set graph.state ag [||] in
  let () = Mods.DynArray.set graph.sort ag None in
  let () = match graph.connected_component with
    | None -> ()
    | Some ccs -> Mods.DynArray.set ccs ag None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Mods.Int2Set.filter (fun (ag',_) -> ag <> ag') graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = (let new_id,ids = graph.free_id in (new_id,ag::ids));
    connected_component = graph.connected_component;
  }
let remove_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = assert ((Mods.DynArray.get graph.connect ag).(s) = None) in
  {
    outdated = false;
    connect = graph.connect;
    missings = Mods.Int2Set.add (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
    connected_component = graph.connected_component;
  }
let get_internal ag s graph =
  let () = assert (not graph.outdated) in
  match (Mods.DynArray.get graph.state ag).(s) with
  | Some i -> i
  | None ->
    failwith ("Site "^string_of_int s^ " of agent "^string_of_int ag^
              " has no internal state in the current graph.")
let get_sites ag graph =
  let arr = Mods.DynArray.get graph.state ag in
  Array.length arr

let get_sort ag graph = 
  match Mods.DynArray.get graph.sort ag with
  | Some ty -> ty
  | None -> 
    failwith ("Agent "^string_of_int ag^
              " has no type in the current graph.")

let remove_internal ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let i = (Mods.DynArray.get graph.state ag).(s) in
  let () = (Mods.DynArray.get graph.state ag).(s) <- None in
  match i with
    None -> assert false
  | Some i ->
    i, {
      outdated = false;
      connect = graph.connect;
      missings = graph.missings;
      state = graph.state;
      sort = graph.sort;
      cache = graph.cache;
      free_id = graph.free_id;
      connected_component = graph.connected_component;
    }

let remove_link ag s ag' s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (Mods.DynArray.get graph.connect ag).(s) <- None in
  let () = (Mods.DynArray.get graph.connect ag').(s') <- None in
  let out = match graph.connected_component with
    | None -> None
    | Some ccs ->
      separate_connected_component graph.connect graph.cache ccs ag ag' in
  {
    outdated = false;
    connect = graph.connect;
    missings =
      Mods.Int2Set.add (ag,s) (Mods.Int2Set.add (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
    connected_component = graph.connected_component;
  },out

let is_agent (ag,ty) graph =
  let () = assert (not graph.outdated&&Mods.Int2Set.is_empty graph.missings) in
  match Mods.DynArray.get graph.sort ag with
  | Some ty' -> let () = assert (ty = ty') in true
  | None -> false
let is_free ag s graph =
  let () = assert (not graph.outdated&&Mods.Int2Set.is_empty graph.missings) in
  let t = Mods.DynArray.get graph.connect ag in t <> [||] && t.(s) = None
let is_internal i ag s graph =
  let () = assert (not graph.outdated&&Mods.Int2Set.is_empty graph.missings) in
  let t = Mods.DynArray.get graph.state ag in
  t <> [||] && match t.(s) with
  | Some j -> j = i
  | None -> false
let link_exists ag s ag' s' graph =
  let () = assert (not graph.outdated&&Mods.Int2Set.is_empty graph.missings) in
  let t = Mods.DynArray.get graph.connect ag in
  t <> [||] &&
    match t.(s) with
  | Some ((ag'',_),s'') -> ag'=ag'' && s'=s''
  | None -> false

let exists_fresh ag s ty s' graph =
  let () = assert (not graph.outdated&&Mods.Int2Set.is_empty graph.missings) in
  let t = Mods.DynArray.get graph.connect ag in
  if t = [||] then None else
    match t.(s) with
    | Some ((ag',ty'),s'') ->
      if ty'=ty && s'=s'' then Some ag' else None
    | None -> None

let link_destination ag s graph =
  let () = assert (not graph.outdated) in
  (Mods.DynArray.get graph.connect ag).(s)

let all_agents_where f graph =
  let out = IntCollection.create 0 in
  let () = Mods.DynArray.iteri
      (fun id -> function
         | Some ty when f (id,ty) -> IntCollection.add id out
         | _ -> ())
      graph.sort in
  out

let in_same_connected_component ag ag' graph =
  match graph.connected_component with
  | None ->
    raise (ExceptionDefn.Internal_Error
             (Locality.dummy_annot
                "in_same_connected_component while not tracking ccs"))
  | Some ccs ->
    Mods.DynArray.get ccs ag = Mods.DynArray.get ccs ag'

let get_connected_component ag graph =
  match graph.connected_component with
  | None ->
    raise (ExceptionDefn.Internal_Error
             (Locality.dummy_annot
                "get_connected_component while not tracking ccs"))
  | Some ccs -> Mods.DynArray.get ccs ag

(** The snapshot machinery *)
let one_connected_component sigs ty node graph =
  let rec build acc free_id dangling =
    function
    | [] -> acc,free_id
    | (ty,node) :: todos ->
      if Cache.test graph.cache node
      then build acc free_id dangling todos
      else match Mods.DynArray.get graph.sort node with
        | None -> failwith "Edges.one_connected_component"
        | Some _ ->
          let () = Cache.mark graph.cache node in
          let arity = Signature.arity sigs ty in
          let ports = Array.make arity Raw_mixture.FREE in
          let (free_id',dangling',todos'),ports =
            Tools.array_fold_left_mapi
              (fun i (free_id,dangling,todos) _ ->
                 match (Mods.DynArray.get graph.connect node).(i) with
                 | None ->
                   (free_id,dangling,todos),Raw_mixture.FREE
                 | Some ((n',ty'),s') ->
                   match Mods.Int2Map.pop (n',s') dangling with
                   | None, dangling ->
                     (succ free_id,
                      Mods.Int2Map.add (node,i) free_id dangling,
                      if n' = node || List.mem (ty',n') todos
                      then todos
                      else (ty',n')::todos),
                     Raw_mixture.VAL free_id
                   | Some id, dangling' ->
                     (free_id,dangling',todos), Raw_mixture.VAL id)
              (free_id,dangling,todos) ports in
          let skel =
            { Raw_mixture.a_type = ty;
              Raw_mixture.a_ports = ports;
              Raw_mixture.a_ints = Mods.DynArray.get graph.state node; } in
          build (skel::acc) free_id' dangling' todos'
  in build [] 1 Mods.Int2Map.empty [ty,node]

let species sigs root graph =
  let specie = match Mods.DynArray.get graph.sort root with
    | None ->
       raise
         (ExceptionDefn.Internal_Error
            (Locality.dummy_annot
               ("Sort of node unavailable "^string_of_int root)))
    | Some ty -> fst (one_connected_component sigs ty root graph) in
  let () = Cache.reset graph.cache in
  specie

let build_snapshot sigs graph =
  let () = assert (not graph.outdated) in
  let rec aux ccs node =
    if node = Mods.DynArray.length graph.sort
    then let () = Cache.reset graph.cache in Raw_mixture.output_snapshot ccs
    else
    if Cache.test graph.cache node
    then aux ccs (succ node)
    else match Mods.DynArray.get graph.sort node with
      | None -> aux ccs (succ node)
      | Some ty ->
        let (out,_) =
          one_connected_component sigs ty node graph in
        aux (Raw_mixture.increment_in_snapshot sigs out ccs) (succ node) in
  aux Raw_mixture.empty_snapshot 0

let debug_print f graph =
  let print_sites ag =
    (Pp.array Pp.comma
       (fun s f l ->
          Format.fprintf
            f "%i%t%t" s
            (match (Mods.DynArray.get graph.state ag).(s) with
             | Some int -> fun f -> Format.fprintf f "~%i" int
             | None -> fun _ -> ())
            (fun f -> match l with
               | None ->
                 if Mods.Int2Set.mem (ag,s) graph.missings
                 then Format.pp_print_string f "?"
               | Some ((ag',ty'),s') ->
                 Format.fprintf f "->%i:%i.%i" ag' ty' s'))) in
  let () = if graph.outdated then Format.fprintf f "OUTDATED@ " in
  Mods.DynArray.print
    Pp.empty
    (fun ag f a ->
       match Mods.DynArray.get graph.sort ag with
       | Some ty ->
         Format.fprintf
           f "%i:%i(@[%a@])@ " ag ty (print_sites ag) a
       | None -> if a = [||] then ()
         else Format.fprintf
             f "%i:NOTYPE(@[%a@])@ " ag (print_sites ag) a
    )
    f graph.connect

type path = ((Agent.t * int) * (Agent.t * int)) list
(** ((agent_id, agent_name),site_name) *)

let rec print_path ?sigs f = function
  | [] -> Pp.empty_set f
  | [(ag,s),(ag',s')] ->
    Format.fprintf f "%a.%a@,-%a.%a"
      (Agent.print ?sigs ~with_id:true) ag (Agent.print_site ?sigs ag) s
      (Agent.print_site ?sigs ag') s' (Agent.print ?sigs ~with_id:true) ag'
  | ((ag,s),((p',_ as ag'),s'))::((((p'',_),_),_)::_ as l) ->
    Format.fprintf f "%a.%a@,-%a.%t%a"
      (Agent.print ?sigs ~with_id:true) ag (Agent.print_site ?sigs ag) s
      (Agent.print_site ?sigs ag') s'
      (fun f ->
         if p' <> p'' then
           Format.fprintf f "%a##" (Agent.print ?sigs ~with_id:true) ag')
      (print_path ?sigs) l

let empty_path = []
let singleton_path n s n' s' = [(n,s),(n',s')]
let rev_path l = List.rev_map (fun (x,y) -> (y,x)) l

let breadth_first_traversal
    ~looping ?max_distance stop_on_find is_interesting links cache out todos =
  let rec look_each_site ((id,_ as ag),path as x) site (out,next as acc) =
    if site = 0 then Some (false,out,next) else
      match (Mods.DynArray.get links id).(pred site) with
      | None -> look_each_site x (pred site) acc
      | Some ((id',_ as ag'),site' as y) ->
        if ag' = fst looping  && site' <> snd looping then None
        else if Cache.test cache id' then look_each_site x (pred site) acc
        else
          let () = Cache.mark cache id' in
          let path' = (y,(ag,pred site))::path in
          let next' = (ag',path')::next in
          let out',store =
            match is_interesting ag' with
            | Some x -> ((x,id'),path')::out,true
            | None -> out,false in
          if store&&stop_on_find then Some (true,out',next')
          else look_each_site x (pred site) (out',next') in
  (* depth = number of edges between root and node *)
  let rec aux depth out next = function
    | ((id,_),_ as x)::todos ->
      (match look_each_site
               x (Array.length (Mods.DynArray.get links id)) (out,next) with
       | None -> []
       | Some (stop,out',next') ->
         if stop then out' else aux depth out' next' todos)
    | [] -> match next with
      | [] -> out
      (* end when all graph traversed and return the list of paths *)
      | _ -> match max_distance with
        | Some d when d <= depth -> out
        (* stop when the max distance is reached *)
        | Some _ -> aux (depth+1) out [] next
        | None -> aux depth out [] next in
  aux 1 out [] todos

(* nodes_x: agent_id list = (int * int) list
   nodes_y: adent_id list = int list *)
let are_connected ?max_distance graph nodes_x nodes_y =
  let () = assert (not graph.outdated) in
  if in_same_connected_component
      (fst (List.hd nodes_x)) (fst (List.hd nodes_y)) graph then
    (* look for the closest node in nodes_y *)
    let is_in_nodes_y z = if List.mem z nodes_y then Some () else None in
    (* breadth first search is called on a list of sites;
       start the breadth first search with the boundaries of nodes_x,
       that is all sites that are connected to other nodes in x
       and with all nodes in nodes_x marked as done *)
    let prepare =
      List.fold_left (fun acc (id,_ as ag) ->
          let () = Cache.mark graph.cache id in
          (ag,[])::acc) [] nodes_x in
    match breadth_first_traversal ~looping:((-1,-1),-1) ?max_distance true
            is_in_nodes_y graph.connect graph.cache [] prepare
    with [] -> let () = Cache.reset graph.cache in None
       | [ _,p ] -> let () = Cache.reset graph.cache in Some p
       | _ :: _ -> failwith "Edges.are_they_connected completely broken"
  else None
