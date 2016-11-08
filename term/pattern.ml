type link = UnSpec | Free | Link of int * int (** node_id, site_id *)

(** The link of site k of node i is stored in links(i).(k).

    The internal state of site k of node i is store in internals(i).(k). A
    negative number means UnSpec. *)
type cc = {
  nodes_by_type: int list array;
  links: link array Mods.IntMap.t;
  (*pattern graph id -> [|... link_j...|] i.e agent_id on site_j has a link*)
  internals: int array Mods.IntMap.t;
  (*internal state id -> [|... state_j...|]
    i.e agent_id on site_j has internal state state_j (-1 means any) *)
  recogn_nav: Navigation.step list;
}

type t = cc

type id = int

let compare_canonicals cc cc' = Mods.int_compare cc cc'
let is_equal_canonicals cc cc' = compare_canonicals cc cc' = 0

module Set = Mods.IntSet

module ObsMap = struct
  include Mods.DynArray

  let dummy x = make 0 x
end

let empty_cc sigs =
  let nbt = Array.make (Signature.size sigs) [] in
  {nodes_by_type = nbt; recogn_nav = [];
   links = Mods.IntMap.empty; internals = Mods.IntMap.empty;}

let raw_find_ty tys id =
  let rec aux i =
    assert (i >= 0);
    if List.mem id tys.(i) then i else aux (pred i)
  in aux (Array.length tys - 1)

let find_ty cc id = raw_find_ty cc.nodes_by_type id

let add_origin deps = function
  | None -> deps
  | Some x -> Operator.DepSet.add x deps

(** Errors *)
let already_specified ?sigs x i =
  ExceptionDefn.Malformed_Decl
    (Location.dummy_annot
       (Format.asprintf "Site %a of agent %a already specified"
          (Agent.print_site ?sigs x) i
          (Agent.print ?sigs ~with_id:false) x))

let dangling_node ~sigs tys x =
  ExceptionDefn.Malformed_Decl
    (Location.dummy_annot
       (Format.asprintf
          "Cannot proceed because last declared agent %a/*%i*/%a"
          (Signature.print_agent sigs) (raw_find_ty tys x) x
          Format.pp_print_string " is not linked to its connected component."))

let identity_injection cc =
  Renaming.identity
    (Array.fold_left (fun x y -> List.rev_append y x) [] cc.nodes_by_type)

(** pick a root in the CC. Any root works.
    In this case pick the last node of smallest type *)
let raw_find_root nodes_by_type =
  let rec aux ty =
    if ty = Array.length nodes_by_type
    then None
    else match nodes_by_type.(ty) with
      | [] -> aux (succ ty)
      | h::t ->
        let x = List.fold_left (fun _ x -> x) h t in
        Some(x,ty)
  in aux 0
let find_root cc = raw_find_root cc.nodes_by_type
let find_root_type cc = Tools.option_map snd (find_root cc)

let weight cc =
  let links,double =
    Mods.IntMap.fold
      (fun _ ->
         Array.fold_right
           (fun i (l,d as acc) -> if i <> UnSpec then
               (succ l,if i <> Free then succ d else d)
             else acc))
      cc.links (0,0) in
  Mods.IntMap.fold
    (fun _ ->
       Array.fold_right (fun i acc -> if i <> -1 then succ acc else acc))
    cc.internals (links - double/2)

let are_compatible ?possibilities ~strict root1 cc1 root2 cc2 =
  let tick x =
    match possibilities with
    | None -> ()
    | Some s -> s := Mods.Int2Set.remove x !s in
  let rec aux at_least_one_edge rename = function
    | [] -> if at_least_one_edge then Some rename else None
    | (o,p as pair)::todos ->
      let () = tick pair in
      let int1 = Mods.IntMap.find_default [||] o cc1.internals in
      let int2 = Mods.IntMap.find_default [||] p cc2.internals in
      if
        Tools.array_fold_left2i
          (fun _ b x y -> b && ((not strict && (x = -1||y = -1)) || x = y)) true
          int1 int2 then
        let one_edge =
          at_least_one_edge ||
          Tools.array_fold_left2i
            (fun _ b x y -> b || ((x <> -1||y <> -1) && x = y)) false
            int1 int2 in
        match Tools.array_fold_left2i
                (fun _ c x y ->
                   match c with
                   | None -> c
                   | Some (_,todo,ren) ->
                     match x, y with
                     | (Link _, Free| Free, Link _) -> None
                     | (UnSpec, Free| Free, UnSpec
                       | Link _, UnSpec |UnSpec, Link _) ->
                       if strict then None else c
                     | UnSpec, UnSpec -> c
                     | Free, Free -> Some (true,todo,ren)
                     | Link (n1,s1), Link (n2,s2) ->
                       if s1 = s2 then
                         if Renaming.mem n1 ren then
                           if Renaming.apply ren n1 = n2
                           then Some (true,todo,ren)
                           else None
                         else match Renaming.add n1 n2 ren with
                           | None -> None
                           | Some r' ->
                             if find_ty cc1 n1 = find_ty cc2 n2
                             then Some (true,(n1,n2)::todo,r')
                             else None
                       else None
                )
                (Some (one_edge,todos,rename))
                (Mods.IntMap.find_default [||] o cc1.links)
                (Mods.IntMap.find_default [||] p cc2.links) with
        | None -> None
        | Some (one_edges',todos',ren') -> aux one_edges' ren' todos'
      else None in
  match Renaming.add root1 root2 Renaming.empty with
  | None -> assert false
  | Some r ->
    if
      (* is_single_agent *)
      (Array.fold_left (fun b x -> b && x = -1)
         true (Mods.IntMap.find_default [||] root1 cc1.internals) &&
       Array.fold_left (fun b x -> b && x = UnSpec)
         true (Mods.IntMap.find_default [||] root1 cc1.links)) &&
      (Array.fold_left (fun b x -> b && x = -1)
         true (Mods.IntMap.find_default [||] root2 cc2.internals) &&
       Array.fold_left (fun b x -> b && x = UnSpec)
         true (Mods.IntMap.find_default [||] root2 cc2.links))
    then Some r
    else aux false r [root1,root2]

(** @returns injection from a to b *)
let equal a b =
  match Tools.array_min_equal_not_null
          (Array.map (fun x -> List.length x,x) a.nodes_by_type)
          (Array.map (fun x -> List.length x,x) b.nodes_by_type) with
  | None -> None
  | Some ([],ags) -> if ags = [] then Some Renaming.empty else None
  | Some (h1::_,ags) ->
    List.fold_left
      (fun bool ag ->
         match bool with
         | Some _ -> bool
         | None -> are_compatible ~strict:true h1 a ag b)
      None ags

let automorphisms a =
  match Array.fold_left
          (fun acc x -> Tools.min_pos_int_not_zero acc (List.length x,x))
          (0,[]) a.nodes_by_type with
  | _,[] -> [Renaming.empty]
  | _,(h::_ as l) -> List.fold_left (fun acc ag ->
      match are_compatible ~strict:true h a ag a with
      | None -> acc
      | Some r -> r::acc) [] l

let potential_pairing =
  Tools.array_fold_left2i
    (fun _ acc la -> List.fold_left
        (fun acc b -> List.fold_left
            (fun acc a -> Mods.Int2Set.add (a,b) acc) acc la) acc)
    Mods.Int2Set.empty
let matchings a b =
  let possibilities = ref (potential_pairing a.nodes_by_type b.nodes_by_type) in
  let rec for_one_root acc =
    match Mods.Int2Set.choose !possibilities with
    | None -> acc
    | Some (x,y) ->
      match are_compatible ~possibilities ~strict:false x a y b with
      | None -> for_one_root acc
      | Some r -> for_one_root (r::acc) in
  for_one_root []

(*turns a cc into a path(:list) in the domain*)
let raw_to_navigation (full:bool) nodes_by_type internals links =
  let rec build_for (_,out as acc) don = function
    | [] -> List.rev out
    | h :: t ->
      let first_ints,out_ints =
        Tools.array_fold_lefti
          (fun i (first,out as acc) v ->
             if (full || first) && v >= 0 then
               (false,
                (((if first
                   then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                   else Navigation.Existing h),i),
                 Navigation.ToInternal v)::out)
             else acc)
          acc (Mods.IntMap.find_default [||] h internals)
      in
      let first_lnk,out'',todo =
        Tools.array_fold_lefti
          (fun i (first,ans,re as acc) ->
             function
             | UnSpec -> acc
             | Free ->
               if full || first
               then (false,
                     (((if first
                        then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                        else Navigation.Existing h),i),
                      Navigation.ToNothing)::ans,re)
               else acc
             | Link (n,l) ->
               if List.mem n don || (n = h && i > l) then acc
               else if n = h || List.mem n re
               then
                 if full || first
                 then (false,
                       (((if first
                          then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                          else Navigation.Existing h),i),
                        Navigation.ToNode (Navigation.Existing n,l))::ans ,re)
                 else acc
               else
                 (false,
                  (((if first
                     then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                     else Navigation.Existing h),i),
                   Navigation.ToNode
                     (Navigation.Fresh(n,raw_find_ty nodes_by_type n),l))::ans,
                  n::re))
          (first_ints,out_ints,t) (Mods.IntMap.find_default [||] h links) in
      build_for (first_lnk,out'') (h::don) todo
  in
  match raw_find_root nodes_by_type with
  | None -> [] (*empty path for x0*)
  | Some (x,_) -> (*(ag_sort,ag_id)*)
    build_for (true,[]) (*wip*) [] (*already_done*) [x] (*todo*)

let intersection renaming cc1 cc2 =
  let links,internals,image =
    Renaming.fold
      (fun i j (accl,acci,l as acc) ->
         match Mods.IntMap.find_option i cc1.links with
         | None -> acc
         | Some links1 ->
           match Mods.IntMap.find_option j cc2.links with
           | None -> acc
           | Some links2 ->
             let int1 = Mods.IntMap.find_default [||] i cc1.internals in
             let int2 = Mods.IntMap.find_default [||] j cc2.internals in
             let outl = Array.mapi
                 (fun k x -> if links1.(k) = UnSpec then UnSpec else x) links2 in
             let outi = Array.mapi
                 (fun k x -> if int1.(k) < 0 then -1 else x) int2 in
             if Array.fold_left (fun b x -> b && x = UnSpec) true outl &&
                Array.fold_left (fun b x -> b && x < 0) true outi then acc
             else (Mods.IntMap.add j outl accl, Mods.IntMap.add j outi acci,j::l))
      renaming (Mods.IntMap.empty,Mods.IntMap.empty,[]) in
  let nodes_by_type = Array.map
      (List.filter (fun a -> List.mem a image)) cc2.nodes_by_type in
  { nodes_by_type; links; internals;
    recogn_nav = raw_to_navigation false nodes_by_type internals links; }

let print_cc ?sigs ?cc_id f cc =
  let print_intf (ag_i,_ as ag) link_ids internals neigh =
    snd
      (Tools.array_fold_lefti
         (fun p (not_empty,(free,link_ids as out)) el ->
            let () =
              if internals.(p) >= 0
              then Format.fprintf
                  f "%t%a" (if not_empty then Pp.comma else Pp.empty)
                  (Agent.print_internal ?sigs ag p) internals.(p)
              else
              if  el <> UnSpec then
                Format.fprintf
                  f "%t%a" (if not_empty then Pp.comma else Pp.empty)
                  (Agent.print_site ?sigs ag) p in
            match el with
            | UnSpec ->
              if internals.(p) >= 0
              then let () = Format.fprintf f "?" in (true,out)
              else (not_empty,out)
            | Free -> true,out
            | Link (dst_a,dst_p) ->
              let i,out' =
                match Mods.Int2Map.find_option (dst_a,dst_p) link_ids with
                | Some x -> (x, out)
                | None ->
                  (free,(succ free, Mods.Int2Map.add (ag_i,p) free link_ids)) in
              let () = Format.fprintf f "!%i" i in
              true,out') (false,link_ids) neigh) in
  let () = Format.pp_open_box f 2 in
  let () = match cc_id with
    | None -> ()
    | Some cc_id -> Format.fprintf f "/*cc%i*/@ " cc_id in
  let (_,_) =
    Mods.IntMap.fold
      (fun x el (not_empty,link_ids) ->
         let ag_x = (x,find_ty cc x) in
         let () =
           Format.fprintf
             f "%t@[<h>%a("
             (if not_empty then Pp.comma else Pp.empty)
             (Agent.print ?sigs ~with_id:(cc_id<>None)) ag_x in
         let out = print_intf
             ag_x link_ids (Mods.IntMap.find_default [||] x cc.internals) el in
         let () = Format.fprintf f ")@]" in
         true,out) cc.links (false,(1,Mods.Int2Map.empty)) in
  Format.pp_close_box f ()

let add_fully_specified_to_graph sigs graph cc =
  let e,g =
    Tools.array_fold_lefti
      (fun ty ->
         List.fold_left
           (fun (emb,g) x ->
              let a, g' = Edges.add_agent sigs ty g in
              let emb' = Mods.IntMap.add x (a,ty) emb in
              let g'' =
                Tools.array_fold_lefti
                  (fun s acc i ->
                     if i <> -1 then Edges.add_internal a s i acc else acc)
                  g' (Mods.IntMap.find_default [||] x cc.internals) in
              let g''' =
                Tools.array_fold_lefti
                  (fun s acc -> function
                     | UnSpec | Free -> Edges.add_free a s acc
                     | Link (x',s') ->
                       match Mods.IntMap.find_option x' emb' with
                       | None -> acc
                       | Some ag' -> fst @@ Edges.add_link (a,ty) s ag' s' acc)
                  g'' (Mods.IntMap.find_default [||] x cc.links) in
              (emb',g''')))
      (Mods.IntMap.empty,graph) cc.nodes_by_type in
  let r =
    Mods.IntMap.fold
      (fun i (a,_) r -> Tools.unsome Renaming.empty (Renaming.add i a r))
      e Renaming.empty  in
  (g,r)

let merge_compatible reserved_ids free_id inj1_to_2 cc1 cc2 =
  let img = Renaming.image inj1_to_2 in
  let available_ids =
    Array.map (List.filter (fun id -> not (Mods.IntSet.mem id img)))
      reserved_ids in
  let used_ids =
    Array.map
      (Tools.list_map_option
         (fun id -> if Renaming.mem id inj1_to_2
           then Some (Renaming.apply inj1_to_2 id)
           else None))
      cc1.nodes_by_type in
  let available_in_cc1 =
    Array.mapi
      (fun i l -> Tools.recti
          (fun l _ -> List.tl l) l (List.length cc1.nodes_by_type.(i)))
      reserved_ids in
  let free_id_for_cc1 = ref free_id in
  let get_cc2 j ((inj1,free_id),inj2,(todos1,todos2) as pack) =
    if Renaming.mem j inj2 then (Renaming.apply inj2 j,pack)
    else
      let ty = find_ty cc2 j in
      let img,free_id' =
        match available_ids.(ty) with
        | [] -> free_id,succ free_id
        | h :: t -> let () = available_ids.(ty) <- t in
          h,free_id in
      let () = used_ids.(ty) <- img :: used_ids.(ty) in
      let o =
        match available_in_cc1.(ty) with
        | [] -> let x = !free_id_for_cc1 in let () = incr free_id_for_cc1 in x
        | h :: t -> let () = available_in_cc1.(ty) <- t in h in
      img,
      (((match Renaming.add o img inj1 with Some x -> x | None -> assert false),
        free_id'),
       (match Renaming.add j img inj2 with Some x -> x | None -> assert false),
       (todos1,(j,img)::todos2)) in
  let get_cc1 i ((inj1,free_id),inj2,(todos1,todos2) as pack) =
    if Renaming.mem i inj1 then (Renaming.apply inj1 i,pack)
    else
      let ty = find_ty cc1 i in
      let img,free_id' =
        match available_ids.(ty) with
        | [] -> free_id,succ free_id
        | h :: t -> let () = available_ids.(ty) <- t in
          h,free_id in
      let () = used_ids.(ty) <- img :: used_ids.(ty) in
      img,
      (((match Renaming.add i img inj1 with Some x -> x | None -> assert false),
        free_id'),inj2,((i,img)::todos1,todos2)) in
  let pack',internals,links =
    let rec glue pack inj2 internals links = function
      | [], [] -> (pack,internals,links)
      | [], (i,j) :: todos2 ->
        let linksi = Mods.IntMap.find_default [||] i cc2.links in
        let intso =
          Array.copy (Mods.IntMap.find_default [||] i cc2.internals) in
        let linkso = Array.copy linksi in
        let (pack',inj2',todos') =
        Tools.array_fold_lefti
          (fun k acc -> function
             | (UnSpec | Free) -> acc
             | Link (n,s) ->
               let n',acc' = get_cc2 n acc in
               let () = linkso.(k) <- Link (n',s) in acc')
          (pack,inj2,([],todos2)) linksi in
        glue pack' inj2' (Mods.IntMap.add j intso internals)
          (Mods.IntMap.add j linkso links) todos'
      | (i,j) :: todos1, todos2 ->
        let linksi = Mods.IntMap.find_default [||] i cc1.links in
        let intsi = Mods.IntMap.find_default [||] i cc1.internals in
        let intso = Array.copy intsi in
        let linkso = Array.copy linksi in
        let (pack',inj2',todos') =
          match Mods.IntMap.find_option j cc2.links with
          | None ->
            Tools.array_fold_lefti
              (fun k acc -> function
                 | (UnSpec | Free) -> acc
                 | Link (n,s) ->
                   let n',acc' = get_cc1 n acc in
                   let () = linkso.(k) <- Link (n',s) in acc')
              (pack,inj2,(todos1,todos2)) linksi
          | Some linksj ->
            let intsj = Mods.IntMap.find_default [||] j cc2.internals in
            let () =
              Array.iteri
                (fun k v -> if v  <> -1 then intso.(k) <- v) intsj in
            Tools.array_fold_lefti
              (fun k acc -> function
                 | Free -> acc
                 | Link (n,s) ->
                   let n',acc' = get_cc1 n acc in
                   let () = linkso.(k) <- Link (n',s) in acc'
                 | UnSpec -> match linksj.(k) with
                   | UnSpec -> acc
                   | Free -> let () = linkso.(k) <- Free in acc
                   | Link (n,s) ->
                     let n',acc' = get_cc2 n acc in
                     let () = linkso.(k) <- Link (n',s) in acc')
              (pack,inj2,(todos1,todos2)) linksi in
        glue pack' inj2' (Mods.IntMap.add j intso internals)
          (Mods.IntMap.add j linkso links) todos' in
    glue (inj1_to_2,free_id) (Renaming.identity (Mods.IntSet.elements img))
         Mods.IntMap.empty Mods.IntMap.empty (Renaming.to_list inj1_to_2,[]) in
  let nodes_by_type = Array.map (List.sort Mods.int_compare) used_ids in
  let () =
    Array.iteri
      (fun i x -> reserved_ids.(i) <-
          Tools.list_merge_uniq Mods.int_compare nodes_by_type.(i) x)
      available_ids in
  (pack',
   {
     nodes_by_type; links; internals;
     recogn_nav = raw_to_navigation false nodes_by_type internals links;
   })

let build_navigation_between inj_d_to_o cc_o cc_d =
  let rec handle_links discovered next_round recogn intern = function
    | [] ->
      if next_round = [] then (List.rev_append recogn intern)
      else handle_links discovered [] recogn intern next_round
    | ((i,j,s),(n',s') as h) :: todos ->
      let n = Renaming.apply inj_d_to_o n' in
      match Mods.IntSet.mem j discovered, Mods.IntSet.mem n' discovered with
      | (false, false) ->
        handle_links discovered (h::next_round) recogn intern todos
      | (true, true) ->
        let intern' =
          ((Navigation.Existing i,s),
           Navigation.ToNode (Navigation.Existing n,s'))::intern in
        handle_links discovered next_round recogn intern' todos
      | true, false ->
        let recogn' =
          ((Navigation.Existing i,s),
           Navigation.ToNode
             (Navigation.Fresh (n,find_ty cc_d n'),s'))::recogn in
        handle_links
          (Mods.IntSet.add n' discovered) next_round recogn' intern todos
       | false, true ->
         let recogn' =
           ((Navigation.Existing n,s'),
            Navigation.ToNode
              (Navigation.Fresh (i,find_ty cc_d j),s))::recogn in
         handle_links
           (Mods.IntSet.add j discovered) next_round recogn' intern todos in
    let discov,all_links,intern =
      Renaming.fold
      (fun j i (disc,links,inter) ->
         let linksd = Mods.IntMap.find_default [||] j cc_d.links in
         let intsd = Mods.IntMap.find_default [||] j cc_d.internals in
         let disc',linkso,intso =
           match Mods.IntMap.find_option i cc_o.links with
           | None ->
             disc,
             Array.make (Array.length linksd) UnSpec,
             Array.make (Array.length linksd) (-1)
           | Some linkso ->
             Mods.IntSet.add j disc,
             linkso,Mods.IntMap.find_default [||] i cc_o.internals in
         let inter' =
           Tools.array_fold_left2i
             (fun s acc o d -> if o = -1 && d <> -1
               then ((Navigation.Existing i,s),Navigation.ToInternal d)::acc
               else acc)
             inter intso intsd in
         Tools.array_fold_left2i
           (fun s (dis,li,int as acc) o d ->
              if o <> UnSpec then acc else
                match d with
                | UnSpec -> acc
                | Free ->
                  dis,li,((Navigation.Existing i,s),Navigation.ToNothing)::int
                | Link (n,s') ->
                  if n > (*la*)j || (n = j && s > s') then acc
                  else dis,((i,j,s),(n,s'))::li,int)
         (disc',links,inter') linkso linksd)
      inj_d_to_o (Mods.IntSet.empty,[],[]) in
    handle_links discov [] [] intern all_links

module Env : sig
  type transition = {
    next: Navigation.t;
    dst: id (* id of cc and also address in the Env.domain map*);
    inj: Renaming.t; (* From dst To ("this" cc + extra edge) *)
  }

  type point = {
    content: cc;
    is_obs_of: (Agent.t list * Operator.DepSet.t) option;
    mutable sons: transition list;
  }

  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: point array;
    elementaries: (Navigation.step * id) list array array;
    single_agent_points: (id*Operator.DepSet.t) Mods.IntMap.t;
  }

  val get : t -> int -> point
  val get_single_agent : int -> t -> (id * Operator.DepSet.t) option

  val signatures : t -> Signature.s
  val new_obs_map : t -> (id -> 'a) -> 'a ObsMap.t

  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit
end = struct
  type transition = {
    next: Navigation.t;
    dst: id (* id of cc and also address in the Env.domain map*);
    inj: Renaming.t; (* From dst To ("this" cc + extra edge) *)
  }

  type point = {
    content: cc;
    is_obs_of: (Agent.t list * Operator.DepSet.t) option;
    mutable sons: transition list;
  }

  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: point array;
    elementaries: (Navigation.step * id) list array array;
    single_agent_points: (id*Operator.DepSet.t) Mods.IntMap.t;
  }

  let signatures env = env.sig_decl

  let print f env =
    let pp_point p_id f p =
      Format.fprintf
        f "@[<hov 2>@[<h>%a@]@ %t-> @[(%a)@]@]"
        (print_cc ~sigs:env.sig_decl ~cc_id:p_id) p.content
        (fun f ->
           match p.is_obs_of with
           | None -> ()
           | Some (_,deps)(*TODO*) ->
             Format.fprintf
               f "@[[%a]@]@ "
               (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
               deps)
        (Pp.list
           Pp.space
           (fun f s ->
              Format.fprintf
                f "@[%a(%a)@ %i@]"
                (Navigation.print env.sig_decl (find_ty p.content))
                s.next
                Renaming.print s.inj s.dst))
        p.sons in
    Format.fprintf
      f "@[<v>%a@]"
      (Pp.array Pp.space pp_point)
      env.domain

  let get_single_agent ty env =
    Mods.IntMap.find_option ty env.single_agent_points

  let get env cc_id = env.domain.(cc_id)

  let print_sons_dot sigs cc_id cc f sons =
    Pp.list Pp.space ~trailing:Pp.space
      (fun f son -> Format.fprintf
          f "@[cc%i -> cc%i [label=\"%a %a\"];@]" cc_id son.dst
          (Navigation.print sigs (find_ty cc)) son.next
          Renaming.print son.inj)
      f sons

  let print_point_dot sigs f (id,point) =
    let style =
      match point.is_obs_of with | Some _ -> "octagon" | None -> "box" in
    Format.fprintf f "@[cc%i [label=\"%a\", shape=\"%s\"];@]@,%a"
      id (print_cc ~sigs ?cc_id:None) point.content
      style (print_sons_dot sigs id point.content) point.sons

  let print_dot f env =
    let () = Format.fprintf f "@[<v>strict digraph G {@," in
    let () =
      Pp.array
        ~trailing:Pp.space Pp.space
        (fun i f s -> print_point_dot (env.sig_decl) f (i,s)) f env.domain in
    Format.fprintf f "}@]@."

  let new_obs_map env f = Mods.DynArray.init (Array.length env.domain) f
end

let print ?domain ~with_id f id =
  match domain with
  | None -> Format.pp_print_int f id
  | Some env ->
    let cc_id = if with_id then Some id else None in
    print_cc ~sigs:(Env.signatures env) ?cc_id f env.Env.domain.(id).Env.content

module Matching = struct
  type t = Renaming.t Mods.IntMap.t * Mods.IntSet.t
  (* (map,set)
      map: point_i -> (node_j(i) -> id_node_graph_in_current_matching)
      set:codomain of current matching *)

  let empty = (Mods.IntMap.empty, Mods.IntSet.empty)

  let add_cc (inj,co) id r =
    let c = Renaming.image r in
    match Mods.IntSet.disjoint_union co c with
    | Some co' -> Some (Mods.IntMap.add id r inj, co')
    | None -> None

  let debug_print f (m,_co) =
    Format.fprintf
      f "@[(%a)@]"
      (Pp.set Mods.IntMap.bindings Pp.comma
         (fun f (ccid,nm) ->
            Pp.set Renaming.to_list Pp.comma
              (fun f (node,dst) ->
                 Format.fprintf f "%i:%i->%i" ccid node dst) f nm)) m

  (*- rm - reconstruct: Edges.t -> t -> int -> cc -> int -> t option*)
  let reconstruct domain graph inj id cc_id root =
    let cc = domain.Env.domain.(cc_id).Env.content in
    match find_root cc with
    | None -> failwith "Matching.reconstruct cc error"
    (*- rm - add : int -> int -> Renaming.t -> Renaming.t *)
    | Some (rid,rty) ->
      (* -rm - full_rename: Renaming.t option *)
      let _,full_rename =
        (*- rm - to_navigation: bool -> cc -> list *)
        match cc.recogn_nav with
        | _::_ as nav ->
          List.fold_left
            (fun (root,inj_op) nav ->
               match inj_op with
               | None -> None,None
               | Some inj ->
                 None,Navigation.injection_for_one_more_edge ?root inj graph nav)
            (Some (root,rty),Some Renaming.empty) nav
        (*- rm - find_root: cc -> (type, node) option *)
        | [] -> None, Renaming.add rid root Renaming.empty in
      match full_rename with
      | None -> failwith "Matching.reconstruct renaming error"
      | Some rename ->
        match Mods.IntSet.disjoint_union (Renaming.image rename) (snd inj) with
        | None -> None
        | Some co -> Some (Mods.IntMap.add id rename (fst inj),co)

  let rec aux_is_root_of graph root inj = function
    | [] -> true
    | h :: t ->
      match Navigation.injection_for_one_more_edge ?root inj graph h with
      | None -> false
      | Some inj' -> aux_is_root_of graph None inj' t
  let is_root_of domain graph (_,rty as root) cc_id =
    let cc = domain.Env.domain.(cc_id).Env.content in
    match cc.recogn_nav with
    | [] ->
      (match find_root cc with
       | Some (_,rty') -> rty = rty'
       | None -> false)
    | nav -> aux_is_root_of graph (Some root) Renaming.empty nav

  let roots_of domain graph cc =
    Edges.all_agents_where (fun x -> is_root_of domain graph x cc) graph

  (* get : (ContentAgent.t * int) -> t -> int *)
  let get ((node,_),id) (t,_) =
    Renaming.apply (Mods.IntMap.find_default Renaming.empty id t) node

  let elements_with_types domain ccs (t,_) =
    let out = Array.make (Mods.IntMap.size t) [] in
    let () =
      Mods.IntMap.iter
        (fun id map ->
           out.(id) <- Renaming.fold
               (fun i out acc ->
                  (out,find_ty domain.Env.domain.(ccs.(id)).Env.content i)::acc)
               map [])
        t in
    out

  module Cache =
  struct
    type t = int * (int * int) option
    let compare (a,a') (b,b') =
      let c = Mods.int_compare a b in
      if c = 0 then
        match a',b' with
        | None, None -> 0
        | None,Some _ -> 1
        | Some _, None -> -1
        | Some x, Some y -> Mods.int_pair_compare x y
      else c
    let print f (a,a') =
      Format.fprintf f "%i%a"
        a (Pp.option (Pp.pair Format.pp_print_int Format.pp_print_int)) a'
  end
  module CacheSetMap = SetMap.Make(Cache)

  type cache = CacheSetMap.Set.t
  let empty_cache = CacheSetMap.Set.empty

  let survive_nav inj graph =
    List.fold_left
      (fun inj step ->
         match inj with
         | None -> inj
         | Some inj ->
           Navigation.injection_for_one_more_edge inj graph step)
      (Some inj)

  (*edges: list of concrete edges,
    returns the roots of observables that are above in the domain*)
  let from_edge domain graph (out,cache as acc) edge =
    let rec aux_from_edges cache (obs,rev_deps as acc) = function
      | [] -> acc,cache
      | (pid,point,inj_point2graph) :: remains ->
        let acc' =
          match point.Env.is_obs_of with
          | None -> acc
          | Some (roots,ndeps) ->
            (List.fold_left
               (fun acc (id,ty) ->
                  (pid,(Renaming.apply inj_point2graph id,ty))::acc)
               obs roots,
             Operator.DepSet.union rev_deps ndeps) in
        let remains' =
          List.fold_left
            (fun re son ->
               match survive_nav inj_point2graph graph son.Env.next with
               | None -> re
               | Some inj' ->
                 let p' = Env.get domain son.Env.dst in
                 let rename = Renaming.compose false son.Env.inj inj' in
                 let next = (son.Env.dst,p',rename) in
                 if CacheSetMap.Set.mem
                     (son.Env.dst,Renaming.min_elt rename) cache
                 then re
                 else next::re)
            remains point.Env.sons in
        aux_from_edges
          (CacheSetMap.Set.add (pid,Renaming.min_elt inj_point2graph) cache)
          acc' remains' in
    match edge with
    | (Navigation.Existing _,_),_ -> assert false
    | (Navigation.Fresh (_,ty),s),_ ->
      let sa = domain.Env.elementaries.(ty) in
      let rec find_good_edge = function (*one should use a hash here*)
        | [] -> acc
        | (st,cc_id) :: tail ->
          match Navigation.compatible_point Renaming.empty st edge with
          | None ->  find_good_edge tail
          | Some inj' ->
            let dst = domain.Env.domain.(cc_id) in
            aux_from_edges cache out [(cc_id,dst,inj')] in
      find_good_edge sa.(s)

  let observables_from_agent
      domain graph ((obs,rdeps),cache as acc) (_,ty as node) =
    if Edges.is_agent node graph
    then match Env.get_single_agent ty domain with
      | Some (cc,deps) ->
        ((cc,node)::obs,Operator.DepSet.union rdeps deps),cache
      | None -> acc
    else acc

  let observables_from_free domain graph acc node site =
    from_edge domain graph acc
      ((Navigation.Fresh node,site),Navigation.ToNothing)
  let observables_from_internal domain graph acc node site id =
    from_edge domain graph acc
      ((Navigation.Fresh node,site),Navigation.ToInternal id)
  let observables_from_link domain graph acc n site  n' site' =
    from_edge domain graph acc
      ((Navigation.Fresh n,site),
       Navigation.ToNode (Navigation.Fresh n',site'))
end

let embeddings_to_fully_specified domain a_id b =
  let a = domain.Env.domain.(a_id).Env.content in
  match find_root a with
  | None -> [Renaming.empty]
  | Some (h,ty) ->
    List.fold_left (fun acc ag ->
      match are_compatible ~strict:false h a ag b with
      | None -> acc
      | Some r -> r::acc) [] b.nodes_by_type.(ty)

type prepoint = {
  p_id: id;
  element: cc;
  depending: Operator.DepSet.t;
  roots: Agent.t list;
}

type work = {
  sigs: Signature.s;
  cc_env: prepoint list Mods.IntMap.t;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_id: int;
  cc_links: link array Mods.IntMap.t;
  cc_internals: int array Mods.IntMap.t;
  dangling: int; (* node_id *)
}

module PreEnv : sig
  type t

  val empty : Signature.s -> t
  val fresh :
    Signature.s -> int list array -> int -> prepoint list Mods.IntMap.t -> t
  val to_work : t -> work

  val add_cc :
    ?origin:Operator.DepSet.elt -> prepoint list Mods.IntMap.t -> id -> cc ->
    prepoint list Mods.IntMap.t * Renaming.t * id
  val get : t -> id -> cc

  val sigs : t -> Signature.s

  val finalize : t -> Env.t
  val of_env : Env.t -> t
end = struct
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: prepoint list Mods.IntMap.t;
    mutable used_by_a_begin_new: bool;
  }

  let fresh sigs id_by_type nb_id domain =
    {
      sig_decl = sigs;
      id_by_type = id_by_type;
      nb_id = nb_id;
      domain = domain;
      used_by_a_begin_new = false;
    }

  let empty sigs =
    let nbt' = Array.make (Signature.size sigs) [] in
    fresh sigs nbt' 1 Mods.IntMap.empty

  let get env id =
    Mods.IntMap.fold
      (fun _ l acc ->
         List.fold_left (fun acc p -> if p.p_id = id then p.element else acc)
           acc l)
      env.domain
      (empty_cc env.sig_decl)

  let fresh_id env =
    succ
      (Mods.IntMap.fold
         (fun _ x acc ->
            List.fold_left (fun acc p -> max acc p.p_id) acc x)
         env.domain 0)

  let check_vitality env = assert (env.used_by_a_begin_new = false)

  let to_work env =
    let () = check_vitality env in
    let () = env.used_by_a_begin_new <- true in
    {
      sigs = env.sig_decl;
      cc_env = env.domain;
      reserved_id = env.id_by_type;
      used_id = Array.make (Array.length env.id_by_type) [];
      free_id = env.nb_id;
      cc_id = fresh_id env;
      cc_links = Mods.IntMap.empty;
      cc_internals = Mods.IntMap.empty;
      dangling = 0;
    }

  let sigs env = env.sig_decl

  let empty_point sigs =
    {Env.content = empty_cc sigs; Env.is_obs_of = None; Env.sons = [];}

  let fill_elem sigs bottom =
    let elementaries =
      Array.init (Signature.size sigs)
        (fun i -> Array.make (Signature.arity sigs i) []) in
    let () =
      List.iter (fun p ->
          match p.element.recogn_nav with
          | [] | ((Navigation.Existing _,_),_) :: _ -> assert false
          | ((Navigation.Fresh _,_),_) :: _ :: _ -> ()
          | [(Navigation.Fresh (_,ty1),s1),arr as step] ->
            let sa1 = elementaries.(ty1) in
            let () = sa1.(s1) <- (step,p.p_id) :: sa1.(s1) in
            match arr with
            | Navigation.ToNode (Navigation.Fresh (_,ty2),s2) ->
              if ty1 = ty2 && s1 <> s2 then
                sa1.(s2) <- (step,p.p_id) :: sa1.(s2)
              else
                let sa2 = elementaries.(ty2) in
                sa2.(s2) <- (step,p.p_id) :: sa2.(s2)
            | Navigation.ToNode (Navigation.Existing _,s2) ->
              sa1.(s2) <- (step,p.p_id) :: sa1.(s2)
            | Navigation.ToNothing | Navigation.ToInternal _ -> ())
    bottom in
    elementaries

  let insert_navigation domain dst inj_dst2nav p_id nav =
    let rec insert_nav_aux inj_point2nav p_id nav =
      let point = domain.(p_id) in
      let rec insert_nav_sons = function
        | [] ->
          let full_inj_n2p,next =
            Navigation.rename (Renaming.inverse inj_point2nav) nav in
          let inj = Renaming.compose false inj_dst2nav full_inj_n2p in
          point.Env.sons <- {Env.dst; Env.inj; Env.next} :: point.Env.sons
        | h :: t ->
          match Navigation.is_subnavigation inj_point2nav nav h.Env.next with
          | None -> insert_nav_sons t
          | Some (inj,nav') ->
            if h.Env.dst <> dst then
              insert_nav_aux
                (Renaming.compose false h.Env.inj inj) h.Env.dst nav' in
        insert_nav_sons point.Env.sons in
    insert_nav_aux (identity_injection domain.(p_id).Env.content) p_id nav

  let add_cc ?origin env p_id element =
    let w = weight element in
    let rec aux = function
      | [] ->
        let roots =
          match find_root element with
          | None -> assert false
          | Some (rid,rty) ->
            List.sort Agent.compare
              (List.map
                 (fun r -> (Renaming.apply r rid,rty))
                 (automorphisms element)) in
        [{p_id; element;roots;
          depending=add_origin Operator.DepSet.empty origin}],
        identity_injection element,p_id
      | h :: t -> match equal element h.element with
        | None -> let a,b,c = aux t in h::a,b,c
        | Some r ->
          {p_id=h.p_id;
           element=h.element;
           depending=add_origin h.depending origin;
           roots=h.roots}::t,r,h.p_id in
    let env_w,r,out = aux (Mods.IntMap.find_default [] w env) in
    Mods.IntMap.add w env_w env,r,out

  let rec saturate_one this max_l level (_,domain as acc) = function
    | [] -> if level < max_l then
        saturate_one this max_l (succ level) acc
          (Mods.IntMap.find_default [] (succ level) domain)
      else acc
    | h :: t ->
      let acc' =
        match matchings this.element h.element with
        | [] -> acc
        | list ->
          List.fold_left
            (fun (mid,acc) r ->
               let id' = succ mid in
               let x,_,id =
                 add_cc acc id' (intersection r this.element h.element) in
              ((if id = id' then id else mid),x))
            acc list in
       saturate_one this max_l level acc' t
  let rec saturate_level max_l level (_,domain as acc) =
    match Mods.IntMap.find_option level domain with
    | None -> if level <= 0 then acc else saturate_level max_l (pred level) acc
    | Some list ->
      let rec aux acc = function
        | [] -> saturate_level max_l (pred level) acc
        | h::t -> aux (saturate_one h max_l level acc t) t in
      aux acc list
  let saturate domain =
    match Mods.IntMap.max_key domain with
    | None -> 1,domain
    | Some l ->
      let si =
        Mods.IntMap.fold
          (fun _ l m -> List.fold_left (fun m p -> max m p.p_id) m l)
          domain 0 in
      saturate_level l l (si,domain)

  let finalize env =
    let si,complete_domain = saturate env.domain in
    let domain = Array.make (succ si) (empty_point env.sig_decl) in
    let singles = (Mods.IntMap.find_default [] 1 complete_domain) in
    let elementaries = fill_elem env.sig_decl singles in
    let () =
      List.iter
        (fun x ->
           domain.(x.p_id) <-
             { Env.content = x.element; Env.sons = [];
               Env.is_obs_of = Some (x.roots,x.depending); })
        singles in
    let () =
      Mods.IntMap.iter
        (fun level l ->
           if level > 1 then
             List.iter (fun x ->
                 let () =  domain.(x.p_id) <-
                     { Env.content = x.element; Env.sons = [];
                       Env.is_obs_of = Some (x.roots,x.depending);} in
                 List.iter (fun e ->
                     match matchings e.element x.element with
                     | [] -> ()
                     | injs ->
                       List.iter
                         (fun inj_e_x ->
                            let (inj_e2sup,_),sup =
                              merge_compatible env.id_by_type env.nb_id
                                inj_e_x e.element x.element in
                            match equal sup x.element with
                            | None -> assert false
                            | Some inj_sup2x ->
                              let inj =
                                Renaming.inverse
                                  (Renaming.compose
                                     false inj_e2sup inj_sup2x) in
                              let nav = build_navigation_between
                                  inj e.element x.element in
                              insert_navigation domain x.p_id inj e.p_id nav
                         )
                           injs
                   ) singles) l)
        complete_domain in
    let level0 = Mods.IntMap.find_default [] 0 complete_domain in
    let single_agent_points =
      List.fold_left
        (fun acc p ->
           match find_root p.element with
           | None -> acc
           | Some (_,ty) ->
             let () = domain.(p.p_id) <-
                 { Env.content = p.element;
                   Env.is_obs_of = Some (p.roots,p.depending);
                   Env.sons = []; } in
             Mods.IntMap.add ty (p.p_id,p.depending) acc)
        Mods.IntMap.empty level0 in
    {
      Env.sig_decl = env.sig_decl;
      Env.id_by_type =env.id_by_type;
      Env.nb_id = env.nb_id;
      Env.domain;
      Env.elementaries;
      Env.single_agent_points;
    }

  let of_env env =
    let add_cc acc p =
      let w = weight p.element in
      Mods.IntMap.add
        w (p::Mods.IntMap.find_default [] w acc) acc in
    let domain' =
      Tools.array_fold_lefti (fun p_id acc p ->
          match p.Env.is_obs_of with
          | None -> acc
          | Some (roots,depending) ->
            add_cc acc {p_id; element=p.Env.content;depending;roots;})
        Mods.IntMap.empty env.Env.domain in
    {
      sig_decl = env.Env.sig_decl;
      nb_id = env.Env.nb_id;
      id_by_type = env.Env.id_by_type;
      domain = domain';
      used_by_a_begin_new = false;
    }
end

(** Operation to create cc *)
let check_dangling wk =
  if wk.dangling <> 0 then
    raise (dangling_node ~sigs:wk.sigs wk.used_id wk.dangling)

let begin_new env = PreEnv.to_work env

let finish_new ?origin wk =
  let () = check_dangling wk in
  (* rebuild env *)
  let () =
    Tools.iteri
      (fun i -> wk.reserved_id.(i) <-
          List.rev_append wk.used_id.(i) wk.reserved_id.(i))
      (Array.length wk.used_id) in
  let cc_candidate =
    { nodes_by_type = wk.used_id;
      links = wk.cc_links; internals = wk.cc_internals;
      recogn_nav =
        raw_to_navigation false wk.used_id wk.cc_internals wk.cc_links} in
  let preenv,r,out = PreEnv.add_cc ?origin wk.cc_env wk.cc_id cc_candidate in
  PreEnv.fresh wk.sigs wk.reserved_id wk.free_id preenv,r,out

let new_link wk ((x,_ as n1),i) ((y,_ as n2),j) =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_links in
  let y_n = Mods.IntMap.find_default [||] y wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n1 i)
  else if y_n.(j) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n2 j)
  else
    let () = x_n.(i) <- Link (y,j) in
    let () = y_n.(j) <- Link (x,i) in
    if wk.dangling = x || wk.dangling = y
    then { wk with dangling = 0 }
    else wk

let new_free wk ((x,_ as n),i) =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- Free in
    wk

let new_internal_state wk ((x,_ as n), i) va =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_internals in
  if x_n.(i) >= 0 then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- va in
    wk

let new_node wk type_id =
  let () = check_dangling wk in
  let arity = Signature.arity wk.sigs type_id in
  match wk.reserved_id.(type_id) with
  | h::t ->
    let () = wk.used_id.(type_id) <- h :: wk.used_id.(type_id) in
    let () = wk.reserved_id.(type_id) <- t in
    let node = (h,type_id) in
    (node,
     { wk with
       dangling = if Mods.IntMap.is_empty wk.cc_links then 0 else h;
       cc_links = Mods.IntMap.add h (Array.make arity UnSpec) wk.cc_links;
       cc_internals = Mods.IntMap.add h (Array.make arity (-1)) wk.cc_internals;
     })
  | [] ->
    let () = wk.used_id.(type_id) <- wk.free_id :: wk.used_id.(type_id) in
    let node = (wk.free_id, type_id) in
    (node,
     { wk with
       free_id = succ wk.free_id;
       dangling = if Mods.IntMap.is_empty wk.cc_links then 0 else wk.free_id;
       cc_links =
         Mods.IntMap.add wk.free_id (Array.make arity UnSpec) wk.cc_links;
       cc_internals =
         Mods.IntMap.add wk.free_id (Array.make arity (-1)) wk.cc_internals;
     })

let minimal_env sigs contact_map =
  Tools.array_fold_lefti
    (fun ty ->
       Tools.array_fold_lefti
         (fun s acc (ints,links) ->
            let w = begin_new acc in
            let n,w = new_node w ty in
            let w = new_free w (n,s) in
            let acc',_,_cc = finish_new w in
            let acc'' =
              List.fold_left
                (fun acc i ->
                   let w = begin_new acc in
                   let n,w = new_node w ty in
                   let w = new_internal_state w (n,s) i in
                   let out,_,_cc = finish_new w in
                   out) acc' ints in
            List.fold_left
              (fun acc (ty',s') ->
                 let w = begin_new acc in
                 let n,w = new_node w ty in
                 let n',w = new_node w ty' in
                 let w = new_link w (n,s) (n',s') in
                 let out,_,_cc = finish_new w in
                 if ty = ty' && s < s' then
                   let w = begin_new out in
                   let n,w = new_node w ty in
                   let w = new_link w (n,s) (n,s') in
                   let out',_,_cc' = finish_new w in
                   out'
                 else out) acc'' links
         ))
    (PreEnv.empty sigs) contact_map
