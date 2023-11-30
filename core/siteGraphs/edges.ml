(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Edge = struct
  type t = Agent.t * int
  (** agent * site *)

  let _compare ((n, _), s) ((n', _), s') =
    let c = Mods.int_compare n n' in
    if c <> 0 then
      c
    else
      Mods.int_compare s s'

  (* let dummy_link = ((-1,-1),-1) *)
end

(* functions using the cache are responsible of reseting the cache at exit *)
module Cache = struct
  type t = {
    tests: int Mods.DynArray.t;
    bag: int Mods.DynArray.t;
    mutable limit: int;
  }

  let int_l = 31 (*Sys.int_size*)

  let create () =
    { tests = Mods.DynArray.make 1 0; bag = Mods.DynArray.make 1 0; limit = 0 }

  let mark t i =
    let x = i / int_l in
    let old = Mods.DynArray.get t.tests x in
    let () =
      if old = 0 then (
        let () = Mods.DynArray.set t.bag t.limit x in
        t.limit <- succ t.limit
      )
    in
    Mods.DynArray.set t.tests x (old lor (1 lsl (i mod int_l)))

  let test t i =
    Mods.DynArray.get t.tests (i / int_l) land (1 lsl (i mod int_l)) <> 0

  let reset t =
    let () =
      Tools.iteri
        (fun i -> Mods.DynArray.set t.tests (Mods.DynArray.get t.bag i) 0)
        t.limit
    in
    t.limit <- 0

  let iteri_reset f t =
    let () =
      Tools.iteri
        (fun k ->
          let i = Mods.DynArray.get t.bag k in
          let v = Mods.DynArray.get t.tests i in
          if v <> 0 then (
            let acc = int_l * i in
            let () =
              Tools.iteri
                (fun j -> if v land (1 lsl j) <> 0 then f (acc + j))
                int_l
            in
            Mods.DynArray.set t.tests i 0
          ))
        t.limit
    in
    t.limit <- 0
end

let glue_connected_component links cache ccs node1 node2 =
  let cc_id_op = Mods.DynArray.get ccs node2 in
  let rec explore_site id site next =
    if site = 0 then
      next
    else (
      match (Mods.DynArray.get links id).(pred site) with
      | None -> explore_site id (pred site) next
      | Some ((id', _), _) ->
        if Mods.DynArray.get ccs id' = cc_id_op || Cache.test cache id' then
          explore_site id (pred site) next
        else (
          let () = Cache.mark cache id' in
          explore_site id (pred site) (id' :: next)
        )
    )
  in
  let rec is_in_cc next = function
    | id :: todos ->
      is_in_cc
        (explore_site id (Array.length (Mods.DynArray.get links id)) next)
        todos
    | [] ->
      (match next with
      | [] ->
        Cache.iteri_reset (fun i -> Mods.DynArray.set ccs i cc_id_op) cache
      | _ -> is_in_cc [] next)
  in
  let () = Cache.mark cache node1 in
  is_in_cc [] [ node1 ]

let separate_connected_component links (cache1, cache2) ccs node1 node2 =
  let old_cc_id = Option_util.unsome (-1) (Mods.DynArray.get ccs node1) in
  let rec inspect_site cache ?dst_cache id site next =
    if site = 0 then
      Some next
    else (
      match (Mods.DynArray.get links id).(pred site) with
      | None -> inspect_site cache ?dst_cache id (pred site) next
      | Some ((id', _), _) ->
        if
          match dst_cache with
          | None -> false
          | Some dc -> Cache.test dc id'
        then
          None
        else if Cache.test cache id' then
          inspect_site cache ?dst_cache id (pred site) next
        else (
          let () = Cache.mark cache id' in
          inspect_site cache ?dst_cache id (pred site) (id' :: next)
        )
    )
  in
  let rec mark_new_cc orig cache next = function
    | id :: todos ->
      (match
         inspect_site cache id (Array.length (Mods.DynArray.get links id)) next
       with
      | None -> assert false
      | Some next' -> mark_new_cc orig cache next' todos)
    | [] ->
      (match next with
      | [] ->
        let () =
          Cache.iteri_reset (fun i -> Mods.DynArray.set ccs i (Some orig)) cache
        in
        Some (old_cc_id, orig)
      | _ -> mark_new_cc orig cache [] next)
  in
  let rec in_same_cc other_orig other_cache other_next this_orig this_cache
      this_next = function
    | id :: todos ->
      (match
         inspect_site this_cache ~dst_cache:other_cache id
           (Array.length (Mods.DynArray.get links id))
           this_next
       with
      | None ->
        let () = Cache.reset this_cache in
        let () = Cache.reset other_cache in
        None
      | Some next' ->
        in_same_cc other_orig other_cache other_next this_orig this_cache next'
          todos)
    | [] ->
      (match this_next with
      | [] ->
        if Cache.test this_cache old_cc_id then (
          let () = Cache.reset this_cache in
          mark_new_cc other_orig other_cache [] other_next
        ) else (
          let () = Cache.reset other_cache in
          let () =
            Cache.iteri_reset
              (fun i -> Mods.DynArray.set ccs i (Some this_orig))
              this_cache
          in
          Some (old_cc_id, this_orig)
        )
      | _ ->
        in_same_cc this_orig this_cache this_next other_orig other_cache []
          other_next)
  in
  let () = Cache.mark cache1 node1 in
  let () = Cache.mark cache2 node2 in
  in_same_cc node1 cache1 [ node1 ] node2 cache2 [] [ node2 ]

type tables = {
  connect: Edge.t option array Mods.DynArray.t;
  state: int option array Mods.DynArray.t;
  sort: int option Mods.DynArray.t;
  connected_component: int option Mods.DynArray.t option;
  caches: Cache.t * Cache.t;
}

type t = {
  mutable tables: tables option;
  missings: Mods.Int2Set.t;
  free_id: int * int list;
}
(** (agent,site -> binding_state; missings);
    agent,site -> internal_state; agent -> sort; free_id
    the free sites are neither in missings nor in linking_destination *)

let empty ~with_connected_components =
  {
    tables =
      Some
        {
          connect = Mods.DynArray.make 1 [||];
          state = Mods.DynArray.make 1 [||];
          sort = Mods.DynArray.make 1 None;
          connected_component =
            (if with_connected_components then
               Some (Mods.DynArray.make 1 None)
             else
               None);
          caches = Cache.create (), Cache.create ();
        };
    missings = Mods.Int2Set.empty;
    free_id = 0, [];
  }

let copy graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    {
      tables =
        Some
          {
            connect = Mods.DynArray.map Array.copy tables.connect;
            state = Mods.DynArray.map Array.copy tables.state;
            sort = Mods.DynArray.copy tables.sort;
            connected_component =
              (match tables.connected_component with
              | None -> None
              | Some ccs -> Some (Mods.DynArray.copy ccs));
            caches = Cache.create (), Cache.create ();
          };
      free_id = graph.free_id;
      missings = graph.missings;
    }

type stats = { nb_agents: int }

let stats graph =
  let top_id, free_ids = graph.free_id in
  { nb_agents = top_id - List.length free_ids }

let add_agent ?id sigs ty graph =
  let ar = Signature.arity sigs ty in
  let al = Array.make ar None in
  let ai = Array.make ar None in
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let h, free_id =
      match id with
      | Some id ->
        ( id,
          let new_id, l = graph.free_id in
          if id < new_id then (
            match List.partition (fun i -> i = id) l with
            | [ _ ], t -> new_id, t
            | _, _ ->
              raise
                (ExceptionDefn.Internal_Error
                   (Locality.dummy_annot
                      ("Try to add an agent with a the free id "
                     ^ string_of_int id)))
          ) else
            ( succ id,
              Tools.recti (fun acc k -> (k + new_id) :: acc) l (id - new_id) ) )
      | None ->
        (match graph.free_id with
        | new_id, h :: t -> h, (new_id, t)
        | new_id, [] -> new_id, (succ new_id, []))
    in
    let missings' =
      Tools.recti (fun a s -> Mods.Int2Set.add (h, s) a) graph.missings ar
    in
    let () = Mods.DynArray.set tables.connect h al in
    let () = Mods.DynArray.set tables.state h ai in
    let () = Mods.DynArray.set tables.sort h (Some ty) in
    let () =
      match tables.connected_component with
      | None -> ()
      | Some ccs -> Mods.DynArray.set ccs h (Some h)
    in
    h, { tables = Some tables; missings = missings'; free_id }

let add_free ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () = (Mods.DynArray.get tables.connect ag).(s) <- None in
    {
      tables = Some tables;
      missings = Mods.Int2Set.remove (ag, s) graph.missings;
      free_id = graph.free_id;
    }

let add_internal ag s i graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () = (Mods.DynArray.get tables.state ag).(s) <- Some i in
    { tables = Some tables; missings = graph.missings; free_id = graph.free_id }

let add_link (ag, ty) s (ag', ty') s' graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () =
      (Mods.DynArray.get tables.connect ag).(s) <- Some ((ag', ty'), s')
    in
    let () =
      (Mods.DynArray.get tables.connect ag').(s') <- Some ((ag, ty), s)
    in
    let out =
      match tables.connected_component with
      | None -> None
      | Some ccs ->
        let i = Option_util.unsome (-1) (Mods.DynArray.get ccs ag) in
        let j = Option_util.unsome (-2) (Mods.DynArray.get ccs ag') in
        if i = j then
          None
        else (
          let () =
            glue_connected_component tables.connect (fst tables.caches) ccs ag
              ag'
          in
          Some (j, i)
        )
    in
    ( {
        tables = Some tables;
        missings =
          Mods.Int2Set.remove (ag, s)
            (Mods.Int2Set.remove (ag', s') graph.missings);
        free_id = graph.free_id;
      },
      out )

let remove_agent ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () = Mods.DynArray.set tables.connect ag [||] in
    let () = Mods.DynArray.set tables.state ag [||] in
    let () = Mods.DynArray.set tables.sort ag None in
    let () =
      match tables.connected_component with
      | None -> ()
      | Some ccs -> Mods.DynArray.set ccs ag None
    in
    {
      tables = Some tables;
      missings = Mods.Int2Set.filter (fun (ag', _) -> ag <> ag') graph.missings;
      free_id =
        (let new_id, ids = graph.free_id in
         new_id, ag :: ids);
    }

let remove_free ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () = assert ((Mods.DynArray.get tables.connect ag).(s) = None) in
    {
      tables = Some tables;
      missings = Mods.Int2Set.add (ag, s) graph.missings;
      free_id = graph.free_id;
    }

let get_internal ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    (match (Mods.DynArray.get tables.state ag).(s) with
    | Some i -> i
    | None ->
      failwith
        ("Site " ^ string_of_int s ^ " of agent " ^ string_of_int ag
       ^ " has no internal state in the current graph."))

let get_sites ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let arr = Mods.DynArray.get tables.state ag in
    Array.length arr

let get_sort ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    (match Mods.DynArray.get tables.sort ag with
    | Some ty -> ty
    | None ->
      failwith
        ("Agent " ^ string_of_int ag ^ " has no type in the current graph."))

let remove_internal ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let i = (Mods.DynArray.get tables.state ag).(s) in
    let () = (Mods.DynArray.get tables.state ag).(s) <- None in
    (match i with
    | None -> assert false
    | Some i ->
      ( i,
        {
          tables = Some tables;
          missings = graph.missings;
          free_id = graph.free_id;
        } ))

let remove_link ag s ag' s' graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = graph.tables <- None in
    let () = (Mods.DynArray.get tables.connect ag).(s) <- None in
    let () = (Mods.DynArray.get tables.connect ag').(s') <- None in
    let out =
      match tables.connected_component with
      | None -> None
      | Some ccs ->
        separate_connected_component tables.connect tables.caches ccs ag ag'
    in
    ( {
        tables = Some tables;
        missings =
          Mods.Int2Set.add (ag, s) (Mods.Int2Set.add (ag', s') graph.missings);
        free_id = graph.free_id;
      },
      out )

let is_agent (ag, ty) graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    (match Mods.DynArray.get tables.sort ag with
    | Some ty' ->
      let () = assert (ty = ty') in
      true
    | None -> false)

let is_agent_id ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    Mods.DynArray.get tables.sort ag <> None

let is_free ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    let t = Mods.DynArray.get tables.connect ag in
    t <> [||] && t.(s) = None

let is_internal i ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    let t = Mods.DynArray.get tables.state ag in
    t <> [||]
    &&
    (match t.(s) with
    | Some j -> j = i
    | None -> false)

let link_exists ag s ag' s' graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    let t = Mods.DynArray.get tables.connect ag in
    t <> [||]
    &&
    (match t.(s) with
    | Some ((ag'', _), s'') -> ag' = ag'' && s' = s''
    | None -> false)

let exists_fresh ag s ty s' graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let () = assert (Mods.Int2Set.is_empty graph.missings) in
    let t = Mods.DynArray.get tables.connect ag in
    if t = [||] then
      None
    else (
      match t.(s) with
      | Some ((ag', ty'), s'') ->
        if ty' = ty && s' = s'' then
          Some ag'
        else
          None
      | None -> None
    )

let link_destination ag s graph =
  match graph.tables with
  | None -> assert false
  | Some tables -> (Mods.DynArray.get tables.connect ag).(s)

let iter_neighbors f ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let ag_table = Mods.DynArray.get tables.connect ag in
    Array.iter
      (function
        | None -> ()
        | Some s -> f (fst s))
      ag_table

let all_agents_where f graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let out = IntCollection.create 0 in
    let () =
      Mods.DynArray.iteri
        (fun id -> function
          | Some ty when f (id, ty) -> IntCollection.add id out
          | _ -> ())
        tables.sort
    in
    out

let in_same_connected_component ag ag' graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    (match tables.connected_component with
    | None ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot
              "in_same_connected_component while not tracking ccs"))
    | Some ccs -> Mods.DynArray.get ccs ag = Mods.DynArray.get ccs ag')

let get_connected_component ag graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    (match tables.connected_component with
    | None ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot
              "get_connected_component while not tracking ccs"))
    | Some ccs -> Mods.DynArray.get ccs ag)

(** The snapshot machinery *)
let one_connected_component sigs ty node graph =
  let rec build id acc known = function
    | [] ->
      Tools.array_rev_map_of_list
        (fun (node_id_in_witness, node_type, sites) ->
          {
            Snapshot.node_id_in_witness;
            Snapshot.node_type;
            Snapshot.node_sites =
              Tools.array_map_of_list
                (fun (link, site_state) ->
                  {
                    Snapshot.site_link =
                      Option_util.map
                        (fun ((n, _), s) ->
                          Mods.IntMap.find_default (-1) n known, s)
                        link;
                    Snapshot.site_state;
                  })
                sites;
          })
        acc
    | (node, ty) :: todos ->
      if Cache.test (fst graph.caches) node then
        build id acc known todos
      else (
        match Mods.DynArray.get graph.sort node with
        | None -> failwith "Edges.one_connected_component"
        | Some _ ->
          let () = Cache.mark (fst graph.caches) node in
          let known' = Mods.IntMap.add node id known in
          let arity = Signature.arity sigs ty in
          let todos', ports =
            Tools.recti
              (fun (todos, acc) i ->
                let link = (Mods.DynArray.get graph.connect node).(i) in
                ( (match link with
                  | None -> todos
                  | Some (((n', _) as p), _) ->
                    if Mods.IntMap.mem n' known' then
                      todos
                    else
                      p :: todos),
                  (link, (Mods.DynArray.get graph.state node).(i)) :: acc ))
              (todos, []) arity
          in
          build (succ id) ((node, ty, ports) :: acc) known' todos'
      )
  in
  build 0 [] Mods.IntMap.empty [ node, ty ]

let species ~debugMode sigs root graph =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    let specie =
      match Mods.DynArray.get tables.sort root with
      | None ->
        raise
          (ExceptionDefn.Internal_Error
             (Locality.dummy_annot
                ("Sort of node unavailable " ^ string_of_int root)))
      | Some ty ->
        Snapshot.cc_to_user_cc ~debugMode ~raw:true sigs
          (one_connected_component sigs ty root tables)
    in
    let () = Cache.reset (fst tables.caches) in
    specie

let rec aux_build_snapshot raw sigs tables ccs node =
  if node = Mods.DynArray.length tables.sort then (
    let () = Cache.reset (fst tables.caches) in
    ccs
  ) else if Cache.test (fst tables.caches) node then
    aux_build_snapshot raw sigs tables ccs (succ node)
  else (
    match Mods.DynArray.get tables.sort node with
    | None -> aux_build_snapshot raw sigs tables ccs (succ node)
    | Some ty ->
      let out = one_connected_component sigs ty node tables in
      aux_build_snapshot raw sigs tables
        (Snapshot.increment_in_snapshot ~raw sigs out ccs)
        (succ node)
  )

let build_snapshot ~raw sigs graph =
  match graph.tables with
  | None -> assert false
  | Some tables -> aux_build_snapshot raw sigs tables Snapshot.empty 0

let build_user_snapshot ~debugMode ~raw sigs graph =
  Snapshot.export ~debugMode ~raw sigs (build_snapshot ~raw sigs graph)

let debug_print f graph =
  match graph.tables with
  | None -> Format.fprintf f "OUTDATED@ "
  | Some tables ->
    let print_sites ag =
      Pp.array Pp.comma (fun s f l ->
          Format.fprintf f "%i%t%t" s
            (match (Mods.DynArray.get tables.state ag).(s) with
            | Some int -> fun f -> Format.fprintf f "~%i" int
            | None -> fun _ -> ())
            (fun f ->
              match l with
              | None ->
                if Mods.Int2Set.mem (ag, s) graph.missings then
                  Format.pp_print_string f "?"
              | Some ((ag', ty'), s') ->
                Format.fprintf f "->%i:%i.%i" ag' ty' s'))
    in
    Mods.DynArray.print Pp.empty
      (fun ag f a ->
        match Mods.DynArray.get tables.sort ag with
        | Some ty -> Format.fprintf f "%i:%i(@[%a@])@ " ag ty (print_sites ag) a
        | None ->
          if a = [||] then
            ()
          else
            Format.fprintf f "%i:NOTYPE(@[%a@])@ " ag (print_sites ag) a)
      f tables.connect

type path = ((Agent.t * int) * (Agent.t * int)) list
(** ((agent_id, agent_name),site_name) *)

let rec print_path ?sigs f = function
  | [] -> Pp.empty_set f
  | [ ((ag, s), (ag', s')) ] ->
    Format.fprintf f "%a.%a@,-%a.%a"
      (Agent.print ?sigs ~with_id:true)
      ag
      (Agent.print_site ?sigs ag)
      s
      (Agent.print_site ?sigs ag')
      s'
      (Agent.print ?sigs ~with_id:true)
      ag'
  | ((ag, s), (((p', _) as ag'), s')) :: ((((p'', _), _), _) :: _ as l) ->
    Format.fprintf f "%a.%a@,-%a.%t%a"
      (Agent.print ?sigs ~with_id:true)
      ag
      (Agent.print_site ?sigs ag)
      s
      (Agent.print_site ?sigs ag')
      s'
      (fun f ->
        if p' <> p'' then
          Format.fprintf f "%a##" (Agent.print ?sigs ~with_id:true) ag')
      (print_path ?sigs) l

let empty_path = []
let singleton_path n s n' s' = [ (n, s), (n', s') ]
let rev_path l = List.rev_map (fun (x, y) -> y, x) l

let is_valid_path p graph =
  List.for_all
    (fun (((ag, _), s), ((ag', _), s')) -> link_exists ag s ag' s' graph)
    p

let breadth_first_traversal ~looping ?max_distance stop_on_find is_interesting
    links cache out todos =
  let rec look_each_site ((((id, _) as ag), path) as x) site
      ((out, next) as acc) =
    if site = 0 then
      Some (false, out, next)
    else (
      match (Mods.DynArray.get links id).(pred site) with
      | None -> look_each_site x (pred site) acc
      | Some ((((id', _) as ag'), site') as y) ->
        if ag' = fst looping && site' <> snd looping then
          None
        else if Cache.test cache id' then
          look_each_site x (pred site) acc
        else (
          let () = Cache.mark cache id' in
          let path' = (y, (ag, pred site)) :: path in
          let next' = (ag', path') :: next in
          let out', store =
            match is_interesting ag' with
            | Some x -> ((x, id'), path') :: out, true
            | None -> out, false
          in
          if store && stop_on_find then
            Some (true, out', next')
          else
            look_each_site x (pred site) (out', next')
        )
    )
  in
  (* depth = number of edges between root and node *)
  let rec aux depth out next = function
    | (((id, _), _) as x) :: todos ->
      (match
         look_each_site x (Array.length (Mods.DynArray.get links id)) (out, next)
       with
      | None -> []
      | Some (stop, out', next') ->
        if stop then
          out'
        else
          aux depth out' next' todos)
    | [] ->
      (match next with
      | [] -> out
      (* end when all graph traversed and return the list of paths *)
      | _ ->
        (match max_distance with
        | Some d when d <= depth -> out
        (* stop when the max distance is reached *)
        | Some _ -> aux (depth + 1) out [] next
        | None -> aux depth out [] next))
  in
  aux 1 out [] todos

(* nodes_x: agent_id list = (int * int) list
   nodes_y: adent_id list = int list *)
let are_connected ?max_distance graph nodes_x nodes_y =
  match graph.tables with
  | None -> assert false
  | Some tables ->
    if
      in_same_connected_component
        (fst (List.hd nodes_x))
        (fst (List.hd nodes_y))
        graph
    then (
      (* look for the closest node in nodes_y *)
      let is_in_nodes_y z =
        if List.mem z nodes_y then
          Some ()
        else
          None
      in
      (* breadth first search is called on a list of sites;
         start the breadth first search with the boundaries of nodes_x,
         that is all sites that are connected to other nodes in x
         and with all nodes in nodes_x marked as done *)
      let prepare =
        List.fold_left
          (fun acc ((id, _) as ag) ->
            let () = Cache.mark (fst tables.caches) id in
            (ag, []) :: acc)
          [] nodes_x
      in
      match
        breadth_first_traversal
          ~looping:((-1, -1), -1)
          ?max_distance true is_in_nodes_y tables.connect (fst tables.caches) []
          prepare
      with
      | [] ->
        let () = Cache.reset (fst tables.caches) in
        None
      | [ (_, p) ] ->
        let () = Cache.reset (fst tables.caches) in
        Some p
      | _ :: _ -> failwith "Edges.are_they_connected completely broken"
    ) else
      None
