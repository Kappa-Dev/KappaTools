(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type link = UnSpec | Free | Link of int * int (** node_id, site_id *)

(** The link of site k of node i is [fst nodes(i).(k)].

    The internal state of site k of node i is [snd nodes(i).(k)]. A
    negative number means UnSpec. *)
type cc =
  {
    nodes_by_type: int list array;
    nodes: (link * int) array Mods.IntMap.t;
  (*pattern graph id -> [|... (link_j,state_j)...|] i.e agent_id on site_j has
    a link link_j and internal state state_j (-1 means any) *)
    recogn_nav: Navigation.abstract Navigation.t;
  }

type t = cc

type id = int

let debug_print_id fmt id = Format.fprintf fmt "%d" id

let size_of_cc cc = Mods.IntMap.size cc.nodes

let compare_canonicals cc cc' = Mods.int_compare cc cc'

let is_equal_canonicals cc cc' = compare_canonicals cc cc' = 0

let id_to_yojson cc = `Int cc

let id_of_yojson = function
  | `Int cc -> cc
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Not a pattern id",x))

module Set = Mods.IntSet

module Map = Mods.IntMap

module ObsMap = struct
  include Mods.DynArray
  let dummy x = make 0 x
end

let empty_cc sigs =
  let nbt = Array.make (Signature.size sigs) [] in
  {nodes_by_type = nbt; recogn_nav = [];
   nodes = Mods.IntMap.empty;}

let raw_find_ty tys id =
  let rec aux i =
    assert (i >= 0);
    if List.mem id tys.(i) then i else aux (pred i)
  in aux (Array.length tys - 1)

let find_ty cc id = raw_find_ty cc.nodes_by_type id

let add_origin deps = function
  | None -> deps
  | Some x -> Operator.DepSet.add x deps

let reconstruction_navigation cc = cc.recogn_nav

(** Errors *)
let already_specified ?sigs x i =
  ExceptionDefn.Malformed_Decl
    (Locality.dummy_annot
       (Format.asprintf "Site %a of agent %a already specified"
          (Agent.print_site ?sigs x) i
          (Agent.print ?sigs ~with_id:false) x))

let dangling_node ~sigs tys x =
  ExceptionDefn.Malformed_Decl
    (Locality.dummy_annot
       (Format.asprintf
          "Cannot proceed because last declared agent %a/*%i*/%a"
          (Signature.print_agent sigs) (raw_find_ty tys x) x
          Format.pp_print_string " is not linked to its connected component."))

let identity_injection cc =
  Renaming.identity
    (Array.fold_left (fun x y -> List.rev_append y x) [] cc.nodes_by_type)

(** pick a root in the CC. Any root works.
    In this case pick the first node of smallest type *)
let raw_find_root nodes_by_type =
  let rec aux ty =
    if ty = Array.length nodes_by_type
    then None
    else match nodes_by_type.(ty) with
      | [] -> aux (succ ty)
      | h::_ -> Some(h,ty)
  in aux 0

let find_root cc = raw_find_root cc.nodes_by_type

let is_single_agent cc =
  Mods.IntMap.for_all
    (fun _ -> Array.for_all (fun (i,s) -> i = UnSpec && s = -1))
    cc.nodes

let are_compatible ~debugMode ?possibilities ~strict root1 cc1 root2 cc2 =
  let tick x =
    match possibilities with
    | None -> ()
    | Some s -> s := Mods.Int2Set.remove x !s in
  let rec aux at_least_one_edge rename = function
    | [] -> if at_least_one_edge then Ok rename else Error None
    | (o,p as pair)::todos ->
      let () = tick pair in
      match Tools.array_fold_left2i
              (fun i c (lx,ix) (ly,iy) ->
                 match c with
                 | Error _ -> c
                 | Ok (one_edge,todo,ren) ->
                   if ((not strict && (ix = -1||iy = -1)) || ix = iy) then
                     match lx, ly with
                     | (Link _, Free| Free, Link _) ->
                       Error (o,p,i,false)
                     | (UnSpec, Free| Free, UnSpec
                       | Link _, UnSpec |UnSpec, Link _) ->
                       if strict then
                         Error (o,p,i,false)
                       else
                         Ok (one_edge || (ix <> -1 && ix = iy),todo,ren)
                     | UnSpec, UnSpec ->
                       Ok (one_edge || (ix <> -1 && ix = iy),todo,ren)
                     | Free, Free -> Ok (true,todo,ren)
                     | Link (n1,s1), Link (n2,s2) ->
                       if s1 = s2 then
                         if Renaming.mem n1 ren then
                           if Renaming.apply ~debugMode ren n1 = n2
                           then Ok (true,todo,ren)
                           else
                             Error (o,p,i,false)
                         else match Renaming.add ~debugMode n1 n2 ren with
                           | None ->
                             Error (o,p,i,false)
                           | Some r' ->
                             if find_ty cc1 n1 = find_ty cc2 n2
                             then Ok (true,(n1,n2)::todo,r')
                             else
                               Error (o,p,i,false)
                       else
                         Error (o,p,i,false)
                   else
                     Error (o,p,i,true)
              )
              (Ok (at_least_one_edge,todos,rename))
              (Mods.IntMap.find_default [||] o cc1.nodes)
              (Mods.IntMap.find_default [||] p cc2.nodes) with
      | Error conflict -> Error (Some conflict)
      | Ok (one_edges',todos',ren') -> aux one_edges' ren' todos' in
  match Renaming.add ~debugMode root1 root2 (Renaming.empty ()) with
  | None -> assert false
  | Some r ->
    let a_single_agent =
      Array.fold_left (fun b (l,i) -> b && i = -1 && l = UnSpec)
        true (Mods.IntMap.find_default [||] root1 cc1.nodes) ||
      Array.fold_left (fun b (l,i) -> b && i = -1 && l = UnSpec)
        true (Mods.IntMap.find_default [||] root2 cc2.nodes) in
    aux a_single_agent r [root1,root2]

(** @returns injection from a to b *)
let equal ~debugMode a b =
  match Tools.array_min_equal_not_null
          (Array.map (fun x -> List.length x,x) a.nodes_by_type)
          (Array.map (fun x -> List.length x,x) b.nodes_by_type) with
  | None -> None
  | Some ([],ags) -> if ags = [] then Some (Renaming.empty ()) else None
  | Some (h1::_,ags) ->
    List.fold_left
      (fun bool ag ->
         match bool with
         | Some _ -> bool
         | None ->
           match are_compatible ~debugMode ~strict:true h1 a ag b with
           | Ok rename -> Some rename
           | Error _ -> None)
      None ags

let automorphisms ~debugMode a =
  match
    Array.fold_left
      (fun acc x -> Tools.min_pos_int_not_zero acc (List.length x,x))
      (0,[]) a.nodes_by_type
  with
  | _, [] -> [Renaming.empty ()]
  | _, (h :: _ as l) ->
    List.fold_left (fun acc ag ->
        match are_compatible ~debugMode ~strict:true h a ag a with
        | Error _ -> acc
        | Ok r -> r::acc) [] l

let potential_pairing =
  Tools.array_fold_left2i
    (fun _ acc la -> List.fold_left
        (fun acc b -> List.fold_left
            (fun acc a -> Mods.Int2Set.add (a,b) acc) acc la) acc)
    Mods.Int2Set.empty

let matchings ~debugMode a b =
  let possibilities = ref (potential_pairing a.nodes_by_type b.nodes_by_type) in
  let rec for_one_root acc =
    match Mods.Int2Set.choose !possibilities with
    | None -> acc
    | Some (x,y) ->
      match are_compatible ~debugMode ~possibilities ~strict:false x a y b with
      | Error _ -> for_one_root acc
      | Ok r -> for_one_root (r::acc) in
  for_one_root []

(*turns a cc into a path(:list) in the domain*)
let raw_to_navigation (full:bool) nodes_by_type nodes =
  let rec build_for (first,out) don = function
    | [] -> List.rev out
    | h :: t ->
      let first',out',todo =
        Tools.array_fold_lefti
          (fun i (first,ans,re as acc) (l,s) ->
             let (first',ans',_ as acc') =
               if (full || first) && s >= 0 then
                 (false,
                  (((if first
                     then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                     else Navigation.Existing h),i),
                   Navigation.ToInternal s)::ans,re)
               else acc in
             match l with
             | UnSpec -> acc'
             | Free ->
               if full || first'
               then (false,
                     (((if first'
                        then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                        else Navigation.Existing h),i),
                      Navigation.ToNothing)::ans',re)
               else acc'
             | Link (n,l) ->
               if List.mem n don || (n = h && i > l) then acc'
               else if n = h || List.mem n re
               then
                 if full || first'
                 then (false,
                       (((if first'
                          then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                          else Navigation.Existing h),i),
                        Navigation.ToNode (Navigation.Existing n,l))::ans',re)
                 else acc'
               else
                 (false,
                  (((if first'
                     then Navigation.Fresh (h,raw_find_ty nodes_by_type h)
                     else Navigation.Existing h),i),
                   Navigation.ToNode
                     (Navigation.Fresh(n,raw_find_ty nodes_by_type n),l))::ans',
                  n::re))
          (first,out,t) (Mods.IntMap.find_default [||] h nodes) in
      build_for (first',out') (h::don) todo
  in
  match raw_find_root nodes_by_type with
  | None -> [] (*empty path for x0*)
  | Some (x,_) -> (*(ag_sort,ag_id)*)
    build_for (true,[]) (*wip*) [] (*already_done*) [x] (*todo*)

let rec sub_minimize_renaming ~debugMode r = function
  | [], _ -> r
  | _::_, [] -> assert false
  | x::q as l,y::q' ->
    if x = y then
      match Renaming.add ~debugMode x y r with
      | Some r' -> sub_minimize_renaming ~debugMode r' (q,q')
      | None -> assert false
    else
      let fsts,lst = List_util.pop_last l in
      match Renaming.add ~debugMode lst y r with
      | Some r' -> sub_minimize_renaming ~debugMode r' (fsts,q')
      | None -> assert false

let minimize_renaming ~debugMode dst_nbt ref_nbt =
  let re = Renaming.empty () in
  Tools.array_fold_lefti
    (fun ty r ids -> sub_minimize_renaming ~debugMode r (ids,ref_nbt.(ty)))
    re dst_nbt

let minimize ~debugMode cand_nbt cand_nodes ref_nbt =
  let re = minimize_renaming ~debugMode cand_nbt ref_nbt in
  let re_img = Renaming.image re in
  let nodes_by_type =
    Array.map (List.filter (fun a -> Mods.IntSet.mem a re_img)) ref_nbt in
  let nodes =
    Mods.IntMap.fold
      (fun id sites acc ->
         let sites' = Array.map (function
             | Link (n,s),i -> Link (Renaming.apply ~debugMode re n,s),i
             | (UnSpec|Free),_ as x -> x) sites in
         Mods.IntMap.add (Renaming.apply ~debugMode re id) sites' acc)
      cand_nodes Mods.IntMap.empty in
  { nodes_by_type; nodes;
    recogn_nav =
      raw_to_navigation false nodes_by_type nodes; }

(* returns a list of cc where each cc is included in cc1*)
let infs ~debugMode ?rooted cc1 cc2 =
  let possibilities =
    ref (match rooted with
        | None -> potential_pairing cc1.nodes_by_type cc2.nodes_by_type
        | Some x -> Mods. Int2Set.singleton x) in
  let rec aux rename nodes = function
    | [] -> nodes
    | (o,p as pair)::todos ->
      let () = possibilities := Mods.Int2Set.remove pair !possibilities in
      let lnk1 = Mods.IntMap.find_default [||] o cc1.nodes in
      let (todos',ren'),outl =
        Tools.array_fold_left_mapi
          (fun k (todo,ren as acc) (ly,iy) ->
             let (lx,ix) = lnk1.(k) in
             match lx, ly with
             | (Link _, Free| Free, Link _
               | Link _, UnSpec |UnSpec, Link _
               | UnSpec, Free| Free, UnSpec
               | UnSpec, UnSpec) ->
               acc,(UnSpec,if ix = iy then iy else -1)
             | Free, Free -> acc,(Free,if ix = iy then iy else -1)
             | Link (n1,s1) as x, Link (n2,s2) ->
               if s1 = s2 then
                 if Renaming.mem n1 ren then
                   (acc,
                    ((if Renaming.apply ~debugMode ren n1 = n2
                      then x else UnSpec),
                     if ix = iy then iy else -1))
                 else match Renaming.add ~debugMode n1 n2 ren with
                   | None -> acc,(UnSpec,if ix = iy then iy else -1)
                   | Some r' ->
                     if find_ty cc1 n1 = find_ty cc2 n2
                     then ((n1,n2)::todo,r'),(x,if ix = iy then iy else -1)
                     else acc,(UnSpec,if ix = iy then iy else -1)
               else (acc,(UnSpec,if ix = iy then iy else -1))
          )
          (todos,rename)
          (Mods.IntMap.find_default [||] p cc2.nodes) in
      if Array.fold_left (fun b (l,i) -> b && l = UnSpec && i < 0) true outl
      then aux ren' nodes todos'
      else aux ren' (Mods.IntMap.add o outl nodes) todos' in

  let rec for_one_root acc =
    match Mods.Int2Set.choose !possibilities with
    | None -> acc
    | Some (root1,root2) ->
      match Renaming.add ~debugMode root1 root2 (Renaming.empty ()) with
      | None -> assert false
      | Some r ->
        let nodes = aux r Mods.IntMap.empty [root1,root2] in
        let acc' =
          if Mods.IntMap.is_empty nodes then acc else
            let nodes_by_type = Array.map
                (List.filter (fun a -> Mods.IntMap.mem a nodes))
                cc1.nodes_by_type in
            (minimize ~debugMode nodes_by_type nodes cc1.nodes_by_type)::acc in
        for_one_root acc'
  in for_one_root []

let rec counter_value nodes (nid,sid) count =
  match Mods.IntMap.find_option nid nodes with
  | None -> count
  | Some ag ->
     Tools.array_fold_lefti
       (fun id acc (el,_) ->
         if (id = sid) then acc
         else
           match el with
           | UnSpec | Free -> acc
           | Link (dn,di) -> counter_value nodes (dn,di) (acc+1)) count ag

let counter_value_cc cc (nid,sid) count =
  let nodes = cc.nodes in
  counter_value nodes (nid,sid) count

let dotcomma dotnet =
  if dotnet
  then (fun fmt -> Format.fprintf fmt ",")
  else  Pp.space
let print_cc
    ~noCounters ?dotnet:(dotnet=false)
    ?(full_species=false) ?sigs ?cc_id ~with_id f cc =
  let print_intf (ag_i, _ as ag) link_ids neigh =
    snd
      (Tools.array_fold_lefti
         (fun p (not_empty, (free, link_ids as out)) (el, st) ->
            let () =
              if st >= 0
              then Format.fprintf
                  f "%t%a"
                  (if not_empty then dotcomma dotnet
                   else Pp.empty)
                  (Agent.print_internal ?sigs ag p) st
              else if  el <> UnSpec then
                Format.fprintf
                  f "%t%a"
                  (if not_empty then dotcomma dotnet
                   else Pp.empty)
                  (Agent.print_site ?sigs ag) p in
            match el with
            | UnSpec ->
              if st >= 0 then
                let () = if full_species then Format.pp_print_string f "[.]" in
                (true,out)
              else (not_empty,out)
            | Free ->
              let () = Format.pp_print_string f "[.]" in
              (true,out)
            | Link (dst_a,dst_p) ->
              let dst_ty = find_ty cc dst_a in
              if match sigs with
                | None -> false
                | Some sigs -> Signature.is_counter_agent sigs dst_ty
                               && not noCounters then
                let counter = counter_value cc.nodes (dst_a,dst_p) 0 in
                let () = Format.fprintf f "{=%d}" counter in
                true,out
              else
              let i,out' =
                match
                  Mods.Int2Map.find_option (dst_a,dst_p) link_ids
                with
                | Some x -> (x, out)
                | None ->
                  (free, (succ free,
                          Mods.Int2Map.add (ag_i,p) free link_ids))
              in
              let () = Format.fprintf f "[%i]" i in
              true, out')
         (false, link_ids) neigh)
  in
  let () = match cc_id with
    | None -> ()
    | Some cc_id -> Format.fprintf f "/*cc%i*/@ " cc_id in
  let (_, _) =
    Mods.IntMap.fold
      (fun x el (not_empty,link_ids) ->
         let ag_x = (x,find_ty cc x) in
         if match sigs with
           | None -> true
           | Some sigs -> not (Signature.is_counter_agent sigs (snd ag_x))
                          || noCounters then
         let () =
           Format.fprintf
             f "%t@[<h>%a("
             (if not_empty
              then
                begin
                  if dotnet then
                  (fun fmt -> Format.fprintf fmt ".")
                  else Pp.comma
                end
              else Pp.empty)
             (Agent.print ?sigs ~with_id) ag_x in
         let out = print_intf ag_x link_ids el in
         let () = Format.fprintf f ")@]" in
         true, out
         else not_empty,link_ids)
      cc.nodes (false, (1, Mods.Int2Map.empty))
  in
  ()

let print_cc_as_id sigs f cc =
  let print_intf (ag_i, _ as ag) link_ids neigh =
    snd
      (Tools.array_fold_lefti
         (fun p (not_empty, (free, link_ids as out)) (el, st) ->
            let () =
              if  el <> UnSpec || st >= 0 then
                Format.fprintf
                  f "%t%a"
                  (if not_empty then fun f -> Format.pp_print_string f "_"
                   else Pp.empty)
                  (Agent.print_site ~sigs ag) p in
            let () =
              if st >= 0 then Format.fprintf
                  f "~%a" (Agent.print_raw_internal ~sigs ag p) st in
            match el with
            | UnSpec -> (not_empty||st >= 0,out)
            | Free -> (true,out)
            | Link (dst_a,dst_p) ->
              let dst_ty = find_ty cc dst_a in
              if Signature.is_counter_agent sigs dst_ty then
                let counter = counter_value cc.nodes (dst_a,dst_p) 0 in
                let () = Format.fprintf f "~+%d" counter in
                true,out
              else
              let i,out' =
                match
                  Mods.Int2Map.find_option (dst_a,dst_p) link_ids
                with
                | Some x -> (x, out)
                | None ->
                  (free, (succ free,
                          Mods.Int2Map.add (ag_i,p) free link_ids))
              in
              let () = Format.fprintf f "~%i" i in
              true, out')
         (false, link_ids) neigh)
  in
  let (_, _) =
    Mods.IntMap.fold
      (fun x el (not_empty,link_ids) ->
         let ag_x = (x,find_ty cc x) in
         if not (Signature.is_counter_agent sigs (snd ag_x)) then
         let () =
           Format.fprintf
             f "%t@[<h>%a__"
             (if not_empty
              then fun f -> Format.pp_print_string f "__"
              else Pp.empty)
             (Agent.print ~sigs ~with_id:false) ag_x in
         let out = print_intf ag_x link_ids el in
         let () = Format.fprintf f "@]" in
         true, out
         else not_empty,link_ids)
      cc.nodes (false, (1, Mods.Int2Map.empty))
  in
  ()

let to_yojson cc =
  match Mods.IntMap.max_key cc.nodes with
  | None -> `Null
  | Some m ->
    let s = succ m in
    let sorts = Array.make s None in
    let () =
      Array.iteri
        (fun ty -> List.iter (fun id -> sorts.(id) <- Some ty))
        cc.nodes_by_type in
    `Assoc [
      "sorts",
      `List
        (Array.fold_right
           (fun x acc -> (match x with None -> `Null | Some i -> `Int i)::acc)
           sorts []);
      "nodes",
      `List (Tools.recti
               (fun acc i -> (match Mods.IntMap.find_option i cc.nodes with
                    | None -> `Null
                    | Some a ->
                      `List (Array.fold_right
                               (fun (l,s) acc ->
                                  `List [(match l with
                                      | Free -> `Bool true
                                      | Link (n,s) ->
                                        `Assoc ["node",`Int n;"site",`Int s]
                                      | UnSpec -> `Bool false);
                                     if s < 0 then `Null else `Int s]::acc)
                               a []))::acc)
               [] s);
    ]

let of_yojson sig_decl = function
  | `Assoc ["sorts",`List s;"nodes",`List n;]
  | `Assoc ["nodes",`List n;"sorts",`List s] ->
    let _,nodes =
      List.fold_left
        (fun (i,acc) -> function
           | `Null -> (succ i,acc)
           | `List l ->
             (succ i,
              Mods.IntMap.add i
                (Tools.array_map_of_list (function
                     | `List [`Bool b;`Null] -> (if b then Free else UnSpec),-1
                     | `List [`Assoc ["node",`Int n;"site",`Int s]
                             | `Assoc ["site",`Int s;"node",`Int n]; `Null] ->
                       Link (n,s),-1
                     | `List [`Bool b;`Int s] -> (if b then Free else UnSpec),s
                     | `List [`Assoc ["node",`Int n;"site",`Int s]
                             | `Assoc ["site",`Int s;"node",`Int n]; `Int st] ->
                       Link (n,s),st
                     | x ->
                       raise (Yojson.Basic.Util.Type_error ("Invalid node",x))
                   ) l) acc)
           | x -> raise (Yojson.Basic.Util.Type_error ("Invalid node links",x)))
    (0,Mods.IntMap.empty) n in
    let nodes_by_type = Array.make (Signature.size sig_decl) [] in
    let () =
      List.iteri (fun i -> function
          | `Null -> ()
          | `Int ty -> nodes_by_type.(ty) <- i :: nodes_by_type.(ty)
          | x -> raise (Yojson.Basic.Util.Type_error ("Wrong node type",x)))
        s in
    {nodes_by_type;nodes;
     recogn_nav = raw_to_navigation false nodes_by_type nodes}
  | `Null -> empty_cc sig_decl
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a pattern",x))

let merge_compatible ~debugMode reserved_ids free_id inj1_to_2 cc1 cc2 =
  let img = Renaming.image inj1_to_2 in
  let available_ids =
    Array.map (List.filter (fun id -> not (Mods.IntSet.mem id img)))
      reserved_ids in
  let used_ids =
    Array.map
      (List_util.map_option
         (fun id -> if Renaming.mem id inj1_to_2
           then Some (Renaming.apply ~debugMode inj1_to_2 id)
           else None))
      cc1.nodes_by_type in
  let available_in_cc1 =
    Array.mapi
      (fun i l ->
         List.filter (fun x -> not (List.mem x cc1.nodes_by_type.(i))) l)
      reserved_ids in
  let free_id_for_cc1 = ref free_id in

  let get_cc2 j ((inj1,free_id),inj2,(todos1,todos2) as pack) =
    if Renaming.mem j inj2 then (Renaming.apply ~debugMode inj2 j,pack)
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
      (((match Renaming.add ~debugMode o img inj1 with
           | Some x -> x
           | None -> assert false),
        free_id'),
       (match Renaming.add ~debugMode j img inj2 with
        | Some x -> x
        | None -> assert false),
       (todos1,(j,img)::todos2)) in

  let get_cc1 i ((inj1,free_id),inj2,(todos1,todos2) as pack) =
    if Renaming.mem i inj1 then (Renaming.apply ~debugMode inj1 i,pack)
    else
      let ty = find_ty cc1 i in
      let img,free_id' =
        match available_ids.(ty) with
        | [] -> free_id,succ free_id
        | h :: t -> let () = available_ids.(ty) <- t in
          h,free_id in
      let () = used_ids.(ty) <- img :: used_ids.(ty) in
      img,
      (((match Renaming.add ~debugMode i img inj1 with
           | Some x -> x
           | None -> assert false),
        free_id'),inj2,((i,img)::todos1,todos2)) in
  let pack',nodes =
    let rec glue pack inj2 nodes = function
      | [], [] -> (pack,nodes)
      | [], (i,j) :: todos2 ->
        let nodesi = Mods.IntMap.find_default [||] i cc2.nodes in
        let nodeso = Array.copy nodesi in
        let (pack',inj2',todos') =
        Tools.array_fold_lefti
          (fun k acc -> function
             | (UnSpec | Free), _ -> acc
             | Link (n,s),st ->
               let n',acc' = get_cc2 n acc in
               let () = nodeso.(k) <- (Link (n',s),st) in acc')
          (pack,inj2,([],todos2)) nodesi in
        glue pack' inj2' (Mods.IntMap.add j nodeso nodes) todos'
      | (i,j) :: todos1, todos2 ->
        let nodesi = Mods.IntMap.find_default [||] i cc1.nodes in
        let nodeso = Array.copy nodesi in
        let (pack',inj2',todos') =
          match Mods.IntMap.find_option j cc2.nodes with
          | None ->
            Tools.array_fold_lefti
              (fun k acc -> function
                 | (UnSpec | Free),_ -> acc
                 | Link (n,s),st ->
                   let n',acc' = get_cc1 n acc in
                   let () = nodeso.(k) <- (Link (n',s),st) in acc')
              (pack,inj2,(todos1,todos2)) nodesi
          | Some nodesj ->
            Tools.array_fold_lefti
              (fun k acc -> function
                 | Free,_ ->
                   let _,stj = nodesj.(k) in
                   let () = if stj  <> -1 then nodeso.(k) <- (Free,stj) in acc
                 | Link (n,s),sti ->
                   let _,stj = nodesj.(k) in
                   let sto = if stj  <> -1 then stj else sti in
                   let n',acc' = get_cc1 n acc in
                   let () = nodeso.(k) <- (Link (n',s),sto) in acc'
                 | UnSpec,sti -> match nodesj.(k) with
                   | UnSpec,stj ->
                     let () =
                       if stj  <> -1 then nodeso.(k) <- (UnSpec,stj) in acc
                   | Free,stj ->
                     let () =
                       nodeso.(k) <- (Free,if stj <> -1 then stj else sti) in
                     acc
                   | Link (n,s),stj ->
                     let sto = if stj  <> -1 then stj else sti in
                     let n',acc' = get_cc2 n acc in
                     let () = nodeso.(k) <- (Link (n',s),sto) in acc')
              (pack,inj2,(todos1,todos2)) nodesi in
        glue pack' inj2' (Mods.IntMap.add j nodeso nodes) todos' in
    glue (inj1_to_2,free_id) (Renaming.identity (Mods.IntSet.elements img))
         Mods.IntMap.empty (Renaming.to_list inj1_to_2,[]) in
  let nodes_by_type = Array.map (List.sort Mods.int_compare) used_ids in
  let () =
    Array.iteri
      (fun i x -> reserved_ids.(i) <-
          List_util.merge_uniq Mods.int_compare nodes_by_type.(i) x)
      available_ids in
  (pack',
   {
     nodes_by_type; nodes;
     recogn_nav = raw_to_navigation false nodes_by_type nodes;
   })

let build_navigation_between ~debugMode inj_d_to_o cc_o cc_d =
  let rec handle_links discovered next_round recogn intern = function
    | [] ->
      if next_round = [] then (List.rev_append recogn intern)
      else handle_links discovered [] recogn intern next_round
    | ((i,j,s),(n',s') as h) :: todos ->
      let n = Renaming.apply ~debugMode inj_d_to_o n' in
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
           let nodesd = Mods.IntMap.find_default [||] j cc_d.nodes in
         let disc',nodeso =
           match Mods.IntMap.find_option i cc_o.nodes with
           | None ->
             disc,
             Array.make (Array.length nodesd) (UnSpec,-1)
           | Some nodeso ->
             Mods.IntSet.add j disc,nodeso in
         Tools.array_fold_left2i
           (fun s (dis,li,int as acc) (ol,os) (dl,ds) ->
              let (_,_, int' as acc') =
                if os = -1 && ds <> -1
                then (dis,li,((Navigation.Existing i,s),Navigation.ToInternal ds)::int)
                else acc in
              if ol <> UnSpec then acc' else
                match dl with
                | UnSpec -> acc'
                | Free ->
                  dis,li,((Navigation.Existing i,s),Navigation.ToNothing)::int'
                | Link (n,s') ->
                  if n > (*la*)j || (n = j && s > s') then acc'
                  else dis,((i,j,s),(n,s'))::li,int')
         (disc',links,inter) nodeso nodesd)
      inj_d_to_o (Mods.IntSet.empty,[],[]) in
    handle_links discov [] [] intern all_links

module Env : sig
  type transition = {
    next: Navigation.abstract Navigation.t;
    dst: id (* id of cc and also address in the Env.domain map*);
    inj: Renaming.t; (* From dst To ("this" cc + extra edge) *)
  }

  type point = {
    content: cc;
    roots: (int * int list (*ids*) * int (*ty*)) list;
    deps: Operator.DepSet.t;
    mutable sons: transition list;
  }

   val content: point -> cc
   val roots: point -> (id * int list (*ag_ids*) * int (*ag_ty*)) list
   val deps: point -> Operator.DepSet.t
   val sons: point -> transition list

  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    max_obs: int;
    domain: point array;
    elementaries: (Navigation.abstract Navigation.step * id) list array array;
    single_agent_points: (id*Operator.DepSet.t) option array;
  }

  val get : t -> id -> point
  val get_single_agent : int -> t -> (id * Operator.DepSet.t) option

  val to_navigation : t -> id -> Navigation.abstract Navigation.t

  val get_elementary :
    debugMode:bool -> t -> Agent.t -> int ->
    Navigation.abstract Navigation.arrow -> (point * Renaming.t) option

  val signatures : t -> Signature.s
  val new_obs_map : t -> (id -> 'a) -> 'a ObsMap.t

  val print_point :
    noCounters:bool -> Signature.s -> id -> Format.formatter -> point -> unit
  val print : noCounters:bool -> Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Basic.t
  val of_yojson : Yojson.Basic.t -> t
end = struct
  type transition = {
    next: Navigation.abstract Navigation.t;
    dst: id (* id of cc and also address in the Env.domain map*);
    inj: Renaming.t; (* From dst To ("this" cc + extra edge) *)
  }

  type point = {
    content: cc;
    roots: (int (*cc_id *) * int list (*ag_ids*) * int (*ag_ty*)) list;
    deps: Operator.DepSet.t;
    mutable sons: transition list;
  }

  let content p = p.content
  let roots p = p.roots
  let deps p = p.deps
  let sons p = p.sons

  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    max_obs: int;
    domain: point array;
    elementaries: (Navigation.abstract Navigation.step * id) list array array;
    single_agent_points: (id*Operator.DepSet.t) option array;
  }

  let signatures env = env.sig_decl

  let print_point ~noCounters sigs p_id f p =
    Format.fprintf
      f "@[<hov 2>@[<h>%a@]@ %t-> @[(%a)@]@]"
      (fun x ->
         print_cc ~noCounters ~sigs ~cc_id:p_id ~with_id:true x)
      p.content
      (fun f -> if p.roots <> [] then
          Format.fprintf
              f "@[[%a]@]@ "
              (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
              p.deps)
      (Pp.list
         Pp.space
         (fun f s ->
            Format.fprintf
              f "@[%a(%a)@ %i@]"
              (Navigation.print sigs (find_ty p.content))
              s.next
              Renaming.print s.inj s.dst))
      p.sons


  let print ~noCounters f env =
    Format.fprintf
      f "@[<v>%a@]"
      (Pp.array Pp.space (print_point ~noCounters env.sig_decl))
      env.domain

  let get_single_agent ty env = env.single_agent_points.(ty)

  let get env cc_id = env.domain.(cc_id)

  let to_navigation env id =
    let cc = (get env id).content in
    raw_to_navigation true cc.nodes_by_type cc.nodes

  let transition_to_yojson t =
    `Assoc [
      "dst", `Int t.dst;
      "inj", Renaming.to_yojson t.inj;
      "nav", Navigation.to_yojson t.next;
    ]
  let transition_of_yojson = function
    | `Assoc [ "dst", `Int dst; "inj", r; "nav", n ]
    | `Assoc [ "dst", `Int dst; "nav", n; "inj", r ]
    | `Assoc [ "inj", r; "nav", n; "dst", `Int dst ]
    | `Assoc [ "nav", n; "inj", r; "dst", `Int dst ]
    | `Assoc [ "inj", r; "dst", `Int dst; "nav", n ]
    | `Assoc [ "nav", n; "dst", `Int dst; "inj", r ] ->
      { dst; inj = Renaming.of_yojson r; next = Navigation.of_yojson n; }
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Incorrect transition",x))

  let point_to_yojson p =
    `Assoc [
      "content",to_yojson p.content;
      "roots", JsonUtil.of_list
        (fun (cc_id,ids,ty) ->
           `List [`Int cc_id; `List (List.map JsonUtil.of_int ids); `Int ty]) p.roots;
      "deps", Operator.depset_to_yojson p.deps;
      "sons", `List (List.map transition_to_yojson p.sons);
    ]

  let point_of_yojson sig_decl = function
    | `Assoc l as x when List.length l = 4 ->
      begin
        try {
          content = of_yojson sig_decl (List.assoc "content" l);
          roots = (match List.assoc "roots" l with
              | `Null -> []
              | `List l ->
                List.map (function
                    | `List [ `Int cc_id; `List ids; `Int ty ] ->
                      (cc_id,List.map Yojson.Basic.Util.to_int ids,ty)
                    | _ -> raise Not_found)
                  l
              | _ -> raise Not_found);
          deps = Operator.depset_of_yojson (List.assoc "deps" l);
          sons = (match List.assoc "sons" l with
              | `List l -> List.map transition_of_yojson l
              | _ -> raise Not_found);
        }
        with Not_found ->
          raise (Yojson.Basic.Util.Type_error ("Incorrect domain point",x))
      end
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Incorrect domain point",x))

  let to_yojson env =
    `Assoc [
      "signatures", Signature.to_json env.sig_decl;
      "single_agents", `List
        (Array.fold_right (fun x acc ->
             (match x with None -> `Null | Some (id,_deps) -> `Int id)::acc)
            env.single_agent_points []);
      "elementaries", `List
        (Array.fold_right (fun x acc ->
             `List (Array.fold_right (fun x acc ->
                 `List (List.map (fun (st,d) ->
                     `List [Navigation.step_to_yojson st; `Int d]) x)
                 ::acc) x [])
             ::acc)
            env.elementaries []);
      "dag", `List
        (Array.fold_right (fun x acc ->
             (point_to_yojson x)::acc) env.domain []);
      "id_by_type", `List
         (Array.fold_right (fun x acc ->
             `List (List.map (fun i -> `Int i) x)::acc) env.id_by_type []);
      "max_obs", `Int env.max_obs
    ]

  let of_yojson = function
    | `Assoc l as x when List.length l = 6 ->
      begin
        let sig_decl = Signature.of_json (List.assoc "signatures" l) in
        try
          {
            sig_decl;
            single_agent_points = (match List.assoc "single_agents" l with
                | `List l  ->
                  Tools.array_map_of_list
                    (Yojson.Basic.Util.to_option
                       (function `Int i -> (i,Operator.DepSet.empty)
                               | x ->
                                 raise (Yojson.Basic.Util.Type_error
                                          ("Wrong single_agent",x)))
                    ) l
                | _ -> raise Not_found);
            elementaries = (match List.assoc "elementaries" l with
                | `List l  ->
                  Tools.array_map_of_list (function
                      | `List l -> Tools.array_map_of_list (function
                          | `List l -> List.map (function
                              | `List [s; `Int d] ->
                                (Navigation.step_of_yojson s,d)
                              | _ -> raise Not_found) l
                          | _ -> raise Not_found) l
                      | _ -> raise Not_found) l
                | _ -> raise Not_found);
            domain =  (match List.assoc "dag" l with
                | `List l  ->
                  Tools.array_map_of_list (point_of_yojson sig_decl) l
                | _ -> raise Not_found);
            id_by_type = (match List.assoc "id_by_type" l with
                | `List l ->
                   Tools.array_map_of_list (function
                       | `List l -> List.map (function
                           | `Int i -> i
                           | _ -> raise Not_found) l
                       | _ -> raise Not_found) l
                | _ -> raise Not_found);
            max_obs = (match List.assoc "max_obs" l with
                        | `Int i -> i
                        | _ -> raise Not_found)
          }
        with Not_found ->
          raise (Yojson.Basic.Util.Type_error ("Incorrect update domain",x))
      end
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Incorrect update domain",x))

  let new_obs_map env f = Mods.DynArray.init env.max_obs f

  let get_elementary ~debugMode domain (_,ty as node) s arrow =
    let sa = domain.elementaries.(ty) in
    let rec find_good_edge = function (*one should use a hash here*)
      | [] -> None
      | (st,cc_id) :: tail ->
        match Navigation.compatible_fresh_point ~debugMode st node s arrow with
        | None ->  find_good_edge tail
        | Some inj' ->
          let dst = get domain cc_id in
          Some (dst,inj') in
    find_good_edge sa.(s)

end

let print ~noCounters ?domain ~with_id f id =
  match domain with
  | None -> Format.pp_print_int f id
  | Some env ->
    let cc_id = if with_id then Some id else None in
    print_cc
      ~noCounters ~sigs:(Env.signatures env) ?cc_id ~with_id
      f env.Env.domain.(id).Env.content

let embeddings_to_fully_specified ~debugMode domain a_id b =
  let a = domain.Env.domain.(a_id).Env.content in
  match find_root a with
  | None -> [Renaming.empty ()]
  | Some (h,ty) ->
    List.fold_left (fun acc ag ->
      match are_compatible ~debugMode ~strict:false h a ag b with
      | Error _ -> acc
      | Ok r -> r::acc) [] b.nodes_by_type.(ty)

type work = {
  sigs: Signature.s;
  cc_env: Env.point Mods.IntMap.t;
  singles : (Env.point * id) list;
  agent_points: (id*Operator.DepSet.t) option array;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_nodes: (link*int) array Mods.IntMap.t;
  dangling: int; (* node_id *)
}

let fresh_cc_id domain =
  succ (Option_util.unsome (-1) (Mods.IntMap.max_key domain))

module PreEnv = struct
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: Env.point Mods.IntMap.t;
    elementaries : (Env.point * id) list;
    single_agent_points: (id*Operator.DepSet.t) option array;
    mutable used_by_a_begin_new: bool;
  }

  type stat = { stat_nodes: int; stat_nav_steps: int }

  let fresh sig_decl id_by_type nb_id domain elementaries single_agent_points =
    {
      sig_decl;
      id_by_type;
      nb_id;
      domain;
      elementaries;
      single_agent_points;
      used_by_a_begin_new = false;
    }

  let empty sigs =
    let nbt' = Array.make (Signature.size sigs) [] in
    let sap = Array.make (Signature.size sigs) None in
    fresh sigs nbt' 1 Mods.IntMap.empty [] sap

  let check_vitality env = assert (env.used_by_a_begin_new = false)

  let to_work env =
    let () = check_vitality env in
    let () = env.used_by_a_begin_new <- true in
    {
      sigs = env.sig_decl;
      cc_env = env.domain;
      singles = env.elementaries;
      agent_points = env.single_agent_points;
      reserved_id = env.id_by_type;
      used_id = Array.make (Array.length env.id_by_type) [];
      free_id = env.nb_id;
      cc_nodes = Mods.IntMap.empty;
      dangling = 0;
    }

  let of_env env =
    let domain =
      Tools.array_fold_lefti
        (fun id acc point -> Mods.IntMap.add id point acc)
        Mods.IntMap.empty env.Env.domain in
    let elementaries =
      Array.fold_left
        (Array.fold_left
           (List.fold_left (fun acc (_,id) -> (env.Env.domain.(id), id)::acc)))
        [] env.Env.elementaries in {
    sig_decl = env.Env.sig_decl;
    id_by_type = env.Env.id_by_type;
    nb_id = succ (Array.fold_left (List.fold_left max) 0 env.Env.id_by_type);
    domain;
    elementaries;
    single_agent_points = env.Env.single_agent_points;
    used_by_a_begin_new = false;
    }

  let sigs env = env.sig_decl

  let empty_point sigs =
    {Env.content = empty_cc sigs; Env.roots = [];
     Env.deps = Operator.DepSet.empty; Env.sons = [];}

  let fill_elem sigs bottom =
    let elementaries =
      Array.init (Signature.size sigs)
        (fun i -> Array.make (Signature.arity sigs i) []) in
    let () =
      List.iter (fun (p,p_id) ->
          match p.Env.content.recogn_nav with
          | [] | ((Navigation.Existing _,_),_) :: _ -> assert false
          | ((Navigation.Fresh _,_),_) :: _ :: _ -> ()
          | [(Navigation.Fresh (_,ty1),s1),arr as step] ->
            let sa1 = elementaries.(ty1) in
            let () = sa1.(s1) <- (step,p_id) :: sa1.(s1) in
            match arr with
            | Navigation.ToNode (Navigation.Fresh (_,ty2),s2) ->
              if ty1 = ty2 && s1 <> s2 then
                sa1.(s2) <- (step,p_id) :: sa1.(s2)
              else
                let sa2 = elementaries.(ty2) in
                sa2.(s2) <- (step,p_id) :: sa2.(s2)
            | Navigation.ToNode (Navigation.Existing _,s2) ->
              sa1.(s2) <- (step,p_id) :: sa1.(s2)
            | Navigation.ToNothing | Navigation.ToInternal _ -> ())
        bottom in
    elementaries

  let present_in_dst ~debugMode dst inj2dst nav =
    let rec aux_present_in_dst inj' = function
      | [] -> Some inj'
      | ((Navigation.Fresh _, _), _) :: _ -> assert false
      | ((Navigation.Existing ag, si), Navigation.ToNothing) :: t ->
        begin match Mods.IntMap.find_option (Renaming.apply ~debugMode inj' ag) dst.nodes with
          | None -> assert false
          | Some n -> if fst n.(si) = Free then aux_present_in_dst inj' t else None
        end
      | ((Navigation.Existing ag, si), Navigation.ToInternal i) :: t ->
        begin match Mods.IntMap.find_option (Renaming.apply ~debugMode inj' ag) dst.nodes with
          | None -> assert false
          | Some n -> if snd n.(si) = i then aux_present_in_dst inj' t else None
        end
      | ((Navigation.Existing ag, si), Navigation.ToNode (Navigation.Existing ag',si')) :: t ->
        begin match Mods.IntMap.find_option (Renaming.apply ~debugMode inj' ag) dst.nodes with
          | None -> assert false
          | Some n ->
            if fst n.(si) = Link (Renaming.apply ~debugMode inj' ag',si')
            then aux_present_in_dst inj' t
            else None
        end
      | ((Navigation.Existing ag, si), Navigation.ToNode (Navigation.Fresh (ag',ty'),si')) :: t ->
        begin match Mods.IntMap.find_option (Renaming.apply ~debugMode inj' ag) dst.nodes with
          | None -> assert false
          | Some n -> match n.(si) with
            | Link (agl,sil), _ ->
              if List.mem agl dst.nodes_by_type.(ty') && si' = sil
              then match Renaming.add ~debugMode ag' agl inj' with
                | None -> None
                | Some inj' -> aux_present_in_dst inj' t
              else None
            | (Free | UnSpec) , _ -> None
        end in
    aux_present_in_dst inj2dst nav

  let ensure_point_is_obs ~debugMode ?origin env p_id point =
    let roots =
      if List.exists (fun (x,_,_) -> x = p_id) point.Env.roots then point.Env.roots
      else
        match find_root point.Env.content with
        | None -> point.Env.roots
        | Some (rid,rty) ->
          ( p_id,
            List.sort Mods.int_compare
              (List.map
                 (fun r -> Renaming.apply ~debugMode r rid)
                 (automorphisms ~debugMode point.Env.content)),rty )
          :: point.Env.roots in
    let point' = {
      Env.content = point.Env.content;
      Env.sons = point.Env.sons;
      Env.deps = add_origin point.Env.deps origin;
      Env.roots } in {
        used_by_a_begin_new = false;
        sig_decl = env.sig_decl;
        id_by_type = env.id_by_type;
        nb_id = env.nb_id;
        elementaries = env.elementaries;
        single_agent_points = env.single_agent_points;
        domain = Mods.IntMap.add p_id point' env.domain;
      }

  let put_fresh_point_in_domain
      ~debugMode ~midpoint ?origin dst_id content sons domain =
    let dst_point = {
      Env.content;
      Env.sons;
      Env.roots =
        (if midpoint then [] else
           match find_root content with
           | None -> []
         | Some (rid,rty) ->
           [ dst_id,
             List.sort Mods.int_compare
               (List.map
                  (fun r -> Renaming.apply ~debugMode r rid)
                  (automorphisms ~debugMode content)),rty ]);
      Env.deps = add_origin Operator.DepSet.empty origin;
    } in
    Mods.IntMap.add dst_id dst_point domain

  let build_son ~debugMode id_by_type nb_id base inj2dst dst_id dst =
    let (inj_e2sup,_),sup =
      merge_compatible ~debugMode id_by_type nb_id inj2dst base dst in
    match equal ~debugMode sup dst with
    | None -> assert false
    | Some inj_sup2dst ->
      let inj_dst2p =
        Renaming.inverse
          (Renaming.compose ~debugMode false inj_e2sup inj_sup2dst) in
      let nav = build_navigation_between ~debugMode inj_dst2p base dst in
      {Env.dst = dst_id; Env.inj = inj_dst2p; Env.next = nav}

  let  add_cc_to_sons ~debugMode ?origin env inj2dst dst_id dst point sons =
      let () =
        point.Env.sons <- List.rev_append
            sons
            [ build_son
                ~debugMode env.id_by_type env.nb_id
                point.Env.content inj2dst dst_id dst ] in {
        used_by_a_begin_new = false;
        sig_decl = env.sig_decl;
        id_by_type = env.id_by_type;
        nb_id = env.nb_id;
        elementaries = env.elementaries;
        single_agent_points = env.single_agent_points;
        domain = put_fresh_point_in_domain
            ~debugMode ~midpoint:false ?origin dst_id dst [] env.domain;
      }

  let insert_mid_point
      ~debugMode ?origin env point visited sons
      inf son_id cc_son inj2cand cand_id cc_cand =
    let domain_with_dst = put_fresh_point_in_domain
        ~debugMode ~midpoint:false ?origin cand_id cc_cand [] env.domain in
    let inf_id = fresh_cc_id domain_with_dst in
    let inf_sons = [
      build_son ~debugMode env.id_by_type env.nb_id inf inj2cand cand_id cc_cand;
      build_son
        ~debugMode env.id_by_type env.nb_id
        inf (identity_injection cc_son) son_id cc_son ] in
    let domain = put_fresh_point_in_domain
        ~debugMode ~midpoint:true inf_id inf inf_sons domain_with_dst in
      let () =
        point.Env.sons <- List.rev_append
            visited
            (build_son
               ~debugMode env.id_by_type env.nb_id
               point.Env.content (identity_injection inf) inf_id inf
             :: sons) in
    {
        used_by_a_begin_new = false;
        sig_decl = env.sig_decl;
        id_by_type = env.id_by_type;
        nb_id = env.nb_id;
        elementaries = env.elementaries;
        single_agent_points = env.single_agent_points;
        domain;
      }

  let rec insert_in_domain
      ~debugMode ?origin env base_id point inj2cand cand_id cc_cand =
    match Renaming.min_elt inj2cand with
    | None -> assert false
    | Some (b,c) ->
      match are_compatible
              ~debugMode ~strict:true b point.Env.content c cc_cand with
      | Ok _ ->
        let env' = ensure_point_is_obs ~debugMode ?origin env base_id point in
        Error (env', inj2cand, point.Env.content, base_id)
      | Error _ ->
        let rec insert_domain_sons visited = function
          | [] ->
            let env' = add_cc_to_sons
                ~debugMode ?origin env inj2cand cand_id cc_cand point visited in
            Ok env'
          | h :: sons ->
            match Mods.IntMap.find_option h.Env.dst env.domain with
            | None -> assert false
            | Some point' ->
              match present_in_dst ~debugMode cc_cand inj2cand h.Env.next with
              | Some inj'2dst ->
                insert_in_domain
                  ~debugMode ?origin env h.Env.dst point'
                  (Renaming.compose ~debugMode false h.Env.inj inj'2dst)
                  cand_id cc_cand
              | None ->
                match Renaming.min_elt h.Env.inj with
                | None -> assert false
                | Some (s,b') ->
                  let c' = Renaming.apply ~debugMode inj2cand b' in
                  (*match are_compatible
                          ~debugMode ~strict:false s point'.Env.content c' cc_cand with
                  | Error _ ->*) begin
                      match infs ~debugMode ~rooted:(s,c') point'.Env.content cc_cand with
                      | [] | _ :: _ :: _ -> assert false
                      | [ inf ] ->
                        match are_compatible
                                ~debugMode ~strict:true s inf b' point.Env.content with
                        | Ok _ -> insert_domain_sons (h::visited) sons
                        | Error _ ->
                          let env' = insert_mid_point
                              ~debugMode env point visited sons
                              inf h.Env.dst point'.Env.content
                              inj2cand cand_id cc_cand in
                          Ok env'
                    end
                  (*| Ok inj_p'2cand -> match insert_domain_sons sons with
                    | Error _ as out -> out
                    | Ok result ->
                      let (inj2sup,fid),sup =
                        merge_compatible
                          ~debugMode env.id_by_type env.nb_id inj_p'2cand point'.Env.content cc_cand in
                insert_in_domain
                  ~debugMode result h.Env.dst point'
                    inj2sup sup*) in
        insert_domain_sons [] point.Env.sons

  let add_single_agent ~debugMode ?origin p_id element env =
    match find_root element with
    | None -> assert false
    | Some (ag_id,ty) ->
      let point,r,cc,cc_id =
        match env.single_agent_points.(ty) with
        | None ->
          let deps = add_origin Operator.DepSet.empty origin in
          let () = env.single_agent_points.(ty) <- Some (p_id,deps) in
          ({ Env.content = element; Env.roots = [p_id,[ag_id],ty];
             Env.deps; Env.sons = []; },
           identity_injection element,
           element,
           p_id)
        | Some (id',deps) ->
          let deps = add_origin deps origin in
          let () = env.single_agent_points.(ty) <- Some (id',deps) in
          match Mods.IntMap.find_option id' env.domain with
          | None -> assert false
          | Some point ->
            match find_root point.Env.content with
            | None -> assert false
            | Some (r_id,ty') ->
              let () = assert (ty = ty') in
              ({ Env.content = point.Env.content; Env.roots = point.Env.roots;
                 Env.deps; Env.sons = point.Env.sons; },
               Option_util.unsome
                 Renaming.dummy
                 (Renaming.add ~debugMode ag_id r_id (Renaming.empty ())),
               point.Env.content,
               id') in
      ({
        used_by_a_begin_new = false;
        nb_id = env.nb_id;
        sig_decl = env.sig_decl;
        id_by_type = env.id_by_type;
        elementaries = env.elementaries;
        single_agent_points = env.single_agent_points;
        domain = Mods.IntMap.add p_id point env.domain;
      },r,cc,cc_id)

  let debug_print f env =
    Pp.array
      Pp.comma
      (fun ty f l ->
        Format.fprintf f "%d: %t" ty
                       (fun f ->
                         Format.fprintf f "[";
                         Pp.list
                           Pp.comma
                           (fun f a -> Format.fprintf f "%d" a) f l;
                         Format.fprintf f "]"))
      f env.id_by_type;
    Format.fprintf f "used_by_a_begin_new = %B@,%a@," (env.used_by_a_begin_new)
      (Mods.IntMap.print (Env.print_point ~noCounters:true env.sig_decl 0))
      env.domain

  let add_cc ~debugMode ?origin env p_id element =
    let rec add_cc_from_elems env = function
      | [] -> (env, identity_injection element, element, p_id)
      | (_, elem_id) :: elems ->
        match Mods.IntMap.find_option elem_id env.domain with
        | None -> assert false
        | Some elem_point ->
          let rec add_cc_from_matchings env = function
            | [] -> add_cc_from_elems env elems
            | inj2element::injs ->
              (*let () = Format.printf "@[%a@]@." debug_print env
                              (print_cc ~noCounters:true ~dotnet:false ~full_species:false
                                ~with_id:true ~cc_id:elem_id ~sigs:env.sig_decl)
                                elem_point.Env.content
                                (print_cc ~noCounters:true ~dotnet:false ~full_species:false
                                ~with_id:true ~cc_id:p_id ~sigs:env.sig_decl)
                                element in*)
              match insert_in_domain
                      ~debugMode ?origin
                      env elem_id elem_point inj2element p_id element with
              | Ok env' -> add_cc_from_matchings env' injs
              | Error (env',_,_,p_id' as x) ->
                if p_id <> p_id' then x
                else add_cc_from_matchings env' injs in
          add_cc_from_matchings
            env (matchings ~debugMode elem_point.Env.content element) in
    if is_single_agent element
    then add_single_agent ~debugMode ?origin p_id element env
    else add_cc_from_elems env env.elementaries

end

(** Operation to create cc *)
let check_dangling wk =
  if wk.dangling <> 0 then
    raise (dangling_node ~sigs:wk.sigs wk.used_id wk.dangling)

let begin_new env = PreEnv.to_work env

let finish_elementary wk =
  let () = check_dangling wk in
  (* rebuild env *)
  let () =
    Tools.iteri
      (fun i -> wk.reserved_id.(i) <-
          List.rev_append wk.used_id.(i) wk.reserved_id.(i))
      (Array.length wk.used_id) in
  let nodes_by_type = Array.map List.rev wk.used_id in
  let content =
    { nodes_by_type; nodes = wk.cc_nodes;
      recogn_nav = raw_to_navigation false nodes_by_type wk.cc_nodes} in
  let point ={
    Env.content;
    Env.roots = [];
    Env.deps = Operator.DepSet.empty;
    Env.sons = []
  } in
  let cc_id = fresh_cc_id wk.cc_env in
  let preenv = Mods.IntMap.add cc_id point wk.cc_env in
  PreEnv.fresh
    wk.sigs wk.reserved_id wk.free_id preenv
    ((point,cc_id)::wk.singles) wk.agent_points

let finish_new ~debugMode ?origin wk =
  let () = check_dangling wk in
  (* rebuild env *)
  let () =
    Tools.iteri
      (fun i -> wk.reserved_id.(i) <-
          List.rev_append wk.used_id.(i) wk.reserved_id.(i))
      (Array.length wk.used_id) in
  let nodes_by_type = Array.map List.rev wk.used_id in
  let cc_candidate =
    { nodes_by_type; nodes = wk.cc_nodes;
      recogn_nav = raw_to_navigation false nodes_by_type wk.cc_nodes} in
  let preenv =
    PreEnv.fresh
      wk.sigs wk.reserved_id wk.free_id wk.cc_env wk.singles wk.agent_points in
  PreEnv.add_cc
    ~debugMode ?origin preenv (fresh_cc_id wk.cc_env) cc_candidate

let new_link wk ((x,_ as n1),i) ((y,_ as n2),j) =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_nodes in
  let y_n = Mods.IntMap.find_default [||] y wk.cc_nodes in
  match x_n.(i), y_n.(j) with
  | (UnSpec, stx), (UnSpec,sty) ->
    let () = x_n.(i) <- (Link (y,j),stx) in
    let () = y_n.(j) <- (Link (x,i),sty) in
    if wk.dangling = x || wk.dangling = y
    then { wk with dangling = 0 }
    else wk
  | ((Free | Link _),_), _ ->
    raise (already_specified ~sigs:wk.sigs n1 i)
  | _, ((Free | Link _),_) ->
    raise (already_specified ~sigs:wk.sigs n2 j)

let new_free wk ((x,_ as n),i) =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_nodes in
  match x_n.(i) with
  | UnSpec,st -> let () = x_n.(i) <- (Free,st) in wk
  | (Free | Link _),_ -> raise (already_specified ~sigs:wk.sigs n i)

let new_internal_state wk ((x,_ as n), i) va =
  let x_n = Mods.IntMap.find_default [||] x wk.cc_nodes in
  let (l,s) = x_n.(i) in
  if s >= 0 then raise (already_specified ~sigs:wk.sigs n i)
  else let () = x_n.(i) <- (l,va) in wk

let new_node wk type_id =
  let () = check_dangling wk in
  let arity = Signature.arity wk.sigs type_id in
  match wk.reserved_id.(type_id) with
  | h::t ->
    let () = wk.used_id.(type_id) <- h :: wk.used_id.(type_id) in
    let () = wk.reserved_id.(type_id) <- t in
    let node = (h,type_id) in
    (node,
     {
       sigs = wk.sigs; cc_env = wk.cc_env; singles = wk.singles;
       agent_points = wk.agent_points; reserved_id = wk.reserved_id;
       used_id = wk.used_id; free_id = wk.free_id;
       dangling = if Mods.IntMap.is_empty wk.cc_nodes then 0 else h;
       cc_nodes = Mods.IntMap.add h (Array.make arity (UnSpec,-1)) wk.cc_nodes;
     })
  | [] ->
    let () = wk.used_id.(type_id) <- wk.free_id :: wk.used_id.(type_id) in
    let node = (wk.free_id, type_id) in
    (node,
     {
       sigs = wk.sigs; cc_env = wk.cc_env; singles = wk.singles;
       agent_points = wk.agent_points; reserved_id = wk.reserved_id;
       used_id = wk.used_id; free_id = succ wk.free_id;
       dangling = if Mods.IntMap.is_empty wk.cc_nodes then 0 else wk.free_id;
       cc_nodes =
         Mods.IntMap.add wk.free_id (Array.make arity (UnSpec,-1)) wk.cc_nodes;
     })

let minimal_env sigs contact_map =
  Tools.array_fold_lefti
    (fun ty ->
       Tools.array_fold_lefti
         (fun s acc (ints,links) ->
            let w = begin_new acc in
            let n,w = new_node w ty in
            let w = new_free w (n,s) in
            let acc' = finish_elementary w in
            let acc'' =
              Mods.IntSet.fold
                (fun i acc ->
                   let w = begin_new acc in
                   let n,w = new_node w ty in
                   let w = new_internal_state w (n,s) i in
                   finish_elementary w) ints acc' in
            Mods.Int2Set.fold
              (fun (ty',s') acc ->
                 let out =
                   if ty < ty' || (ty = ty' && s <= s') then
                     let w = begin_new acc in
                     let n,w = new_node w ty in
                     let n',w = new_node w ty' in
                     let w = new_link w (n,s) (n',s') in
                     finish_elementary w
                   else acc in
                 if ty = ty' && s < s' then
                   let w = begin_new out in
                   let n,w = new_node w ty in
                   let w = new_link w (n,s) (n,s') in
                   finish_elementary w
                 else out) links acc''
         ))
    (PreEnv.empty sigs) contact_map


let fold_by_type f cc acc =
  Tools.array_fold_lefti
    (fun agent_type acc list_pos ->
       List.fold_left
         (fun acc pos ->
            let intf = Mods.IntMap.find_default [||] pos cc.nodes in
            f ~pos ~agent_type intf acc)
         acc
         list_pos)
    acc
    cc.nodes_by_type

let fold f cc acc = Mods.IntMap.fold f cc.nodes acc

let finalize env =
  let max_obs = fresh_cc_id env.PreEnv.domain in
  let domain = Array.make max_obs (PreEnv.empty_point env.PreEnv.sig_decl) in
  let () =
    Mods.IntMap.iter (fun id point -> domain.(id) <- point) env.PreEnv.domain in
  let elementaries =
    PreEnv.fill_elem env.PreEnv.sig_decl env.PreEnv.elementaries in
  {
    Env.sig_decl = env.PreEnv.sig_decl;
    Env.id_by_type = env.PreEnv.id_by_type;
    Env.max_obs;
    Env.domain;
    Env.elementaries;
    Env.single_agent_points = env.PreEnv.single_agent_points;
  },{ stat_nodes = max_obs; PreEnv.stat_nav_steps = (*TODO*) 0 }

let merge_on_inf ~debugMode env m g1 g2 =
  let m_list = Renaming.to_list m in
  let (root1,root2) = List.hd m_list in
  let pairing =
    List.fold_left
      (fun acc (a,b) -> Mods.Int2Set.add (a,b) acc)
      Mods.Int2Set.empty m_list in
  let possibilities = ref pairing in
  match are_compatible
          ~debugMode ~possibilities ~strict:false root1 g1 root2 g2 with
  | Ok m' ->
     let (_,pushout) =
       merge_compatible
         ~debugMode env.PreEnv.id_by_type env.PreEnv.nb_id m' g1 g2 in
     Ok pushout
  | Error conflict -> Error conflict

let length cc = Mods.IntMap.size (cc.nodes)
