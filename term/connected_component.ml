type link = UnSpec | Free | Link of int * int (** node_id, site_id *)

(** The link of site k of node i is stored in links(i).(k).

    The internal state of site k of node i is store in internals(i).(k). A
    negative number means UnSpec. *)
type cc = {
  id: int;
  nodes_by_type: int list array;
  links: link array Mods.IntMap.t;
  (*pattern graph id -> [|... link_j...|] i.e agent_id on site_j has a link*)
  internals: int array Mods.IntMap.t;
  (*internal state id -> [|... state_j...|]
    i.e agent_id on site_j has internal state state_j (-1 means any) *)
  recogn_nav: Navigation.step list;
  discover_nav: Navigation.step list;
}

type t = cc

type transition = {
  next: Navigation.step;
  dst: int (* id of cc and also address in the Env.domain map*);
  inj: Renaming.t list; (* From dst To ("this" cc + extra edge) *)
  above_obs: Mods.IntSet.t;
}

type point = {
  content: cc;
  is_obs_of: Operator.DepSet.t option;
  fathers: int (* t.id *) list;
  sons: transition list;
}

type work = {
  sigs: Signature.s;
  cc_env: (cc * Operator.DepSet.t) list Mods.IntMap.t;
  reserved_id: int list array;
  used_id: int list array;
  free_id: int;
  cc_id: int;
  cc_links: link array Mods.IntMap.t;
  cc_internals: int array Mods.IntMap.t;
  dangling: int; (* node_id *)
}

module ContentAgent = struct
  type t = int * int * int (** (cc_id,type_id,node_id) *)

  let compare (cc,_,n) (cc',_,n') =
    let c = Mods.int_compare cc cc' in
    if c = 0 then Mods.int_compare n n' else c

  let rename wk cc inj (n_cc,n_ty,n_id as node) =
    if wk.cc_id = n_cc then (cc.id,n_ty, Renaming.apply inj n_id)
    else node

  let get_sort (_,ty,_) = ty

  let print ?sigs ?with_id f (cc,ty,i) =
    match sigs with
    | Some sigs ->
      Format.fprintf f "%a%t" (Signature.print_agent sigs) ty
        (fun f -> match with_id with
           | None -> ()
           | Some () -> Format.fprintf f "/*%i*/" i)
    | None -> Format.fprintf f "cc%in%i" cc i

  let print_site ?sigs (cc,agent,i) f id =
    match sigs with
    | Some sigs ->
      Signature.print_site sigs agent f id
    | None -> Format.fprintf f "cc%in%is%i" cc i id

  let print_internal ?sigs (cc,agent,i) site f id =
    match sigs with
    | Some sigs ->
      Signature.print_site_internal_state sigs agent site f (Some id)
    | None -> Format.fprintf f "cc%in%is%i~%i" cc i site id
end

let empty_cc sigs =
  let nbt = Array.make (Signature.size sigs) [] in
  {id = 0; nodes_by_type = nbt; recogn_nav = []; discover_nav = [];
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
          (ContentAgent.print_site ?sigs x) i
          (ContentAgent.print ?sigs ?with_id:None) x))

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
  let ints =
    Mods.IntMap.fold
      (fun _ ->
         Array.fold_right (fun i acc -> if i <> -1 then succ acc else acc))
      cc.internals 0 in
  Mods.IntMap.fold
    (fun _ ->
       Array.fold_right (fun i acc -> if i <> UnSpec then succ acc else acc))
    cc.links ints

let are_compatible ~strict root1 links1 ints1 root2 links2 ints2 =
  let rec aux rename = function
    | [] -> Some rename
    | (o,p)::todos ->
      if
        Tools.array_fold_left2i
          (fun _ b x y -> b && ((not strict && (x = -1||y = -1)) || x = y)) true
          (Mods.IntMap.find_default [||] o ints1)
          (Mods.IntMap.find_default [||] p ints2) then
        match Tools.array_fold_left2i
                (fun _ c x y ->
                   match c with
                   | None -> c
                   | Some (todo,ren) ->
                     match x, y with
                     | (Link _, Free| Free, Link _) -> None
                     | (UnSpec, Free| Free, UnSpec
                       |UnSpec, Link _| Link _, UnSpec) ->
                       if strict then None else c
                     | UnSpec, UnSpec -> c
                     | Free, Free -> c
                     | Link (n1,s1), Link (n2,s2) ->
                       if s1 = s2 then
                         if Renaming.mem n1 ren then
                           if Renaming.apply ren n1 = n2 then c else None
                         else match Renaming.add n1 n2 ren with
                           | None -> None
                           | Some r' -> Some ((n1,n2)::todo,r')
                       else None
                )
                (Some (todos,rename))
                (Mods.IntMap.find_default [||] o links1)
                (Mods.IntMap.find_default [||] p links2) with
        | None -> None
        | Some (todos',ren') -> aux ren' todos'
      else None in
  match Renaming.add root1 root2 Renaming.empty with
  | None -> assert false
  | Some r -> aux r [root1,root2]

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
         | None -> are_compatible
                     ~strict:true h1 a.links a.internals ag b.links b.internals)
      None ags

let automorphisms a =
  match Array.fold_left
          (fun acc x -> Tools.min_pos_int_not_zero acc (List.length x,x))
          (0,[]) a.nodes_by_type with
  | _,[] -> [Renaming.empty]
  | _,(h::_ as l) -> List.fold_left (fun acc ag ->
      match are_compatible
              ~strict:true h a.links a.internals ag a.links a.internals with
      | None -> acc
      | Some r -> r::acc) [] l

let embeddings_to_fully_specified a b =
  match find_root a with
  | None -> [Renaming.empty]
  | Some (h,ty) ->
    List.fold_left (fun acc ag ->
      match are_compatible
              ~strict:false h a.links a.internals ag b.links b.internals with
      | None -> acc
      | Some r -> r::acc) [] b.nodes_by_type.(ty)

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
               if List.mem n don then acc
               else if n = h || List.mem n re
               then
                 if full
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

let to_navigation cc =
  raw_to_navigation true cc.nodes_by_type cc.internals cc.links

let print ?sigs ?with_id f cc =
  let print_intf (_,_,ag_i as ag) link_ids internals neigh =
    snd
      (Tools.array_fold_lefti
         (fun p (not_empty,(free,link_ids as out)) el ->
            let () =
              if internals.(p) >= 0
              then Format.fprintf
                  f "%t%a" (if not_empty then Pp.comma else Pp.empty)
                  (ContentAgent.print_internal ?sigs ag p) internals.(p)
              else
              if  el <> UnSpec then
                Format.fprintf
                  f "%t%a" (if not_empty then Pp.comma else Pp.empty)
                  (ContentAgent.print_site ?sigs ag) p in
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
  let () = match with_id with
    | Some () -> Format.fprintf f "/*cc%i*/@ " cc.id
    | None -> () in
  let (_,_) =
    Mods.IntMap.fold
      (fun x el (not_empty,link_ids) ->
         let ag_x = (cc.id,find_ty cc x,x) in
         let () =
           Format.fprintf
             f "%t@[<h>%a("
             (if not_empty then Pp.comma else Pp.empty)
             (ContentAgent.print ?sigs ?with_id) ag_x in
         let out = print_intf
             ag_x link_ids (Mods.IntMap.find_default [||] x cc.internals) el in
         let () = Format.fprintf f ")@]" in
         true,out) cc.links (false,(1,Mods.Int2Map.empty)) in
  Format.pp_close_box f ()

let print_dot sigs f cc =
  let pp_one_node x i f = function
    | UnSpec -> ()
    | Free ->
       let n = (cc.id,find_ty cc x,x) in
       let () = Format.fprintf
           f "@[%a@ [label=\"%t\",@ height=\".1\",@ width=\".1\""
           (ContentAgent.print_site ?sigs:None n) i Pp.bottom in
       let () =
         Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
       let () = Format.fprintf
           f "@[<b>%a ->@ %a@ @[[headlabel=\"%a\",@ weight=\"25\""
           (ContentAgent.print_site ?sigs:None n) i
           (ContentAgent.print ?sigs:None ?with_id:None) n
           (ContentAgent.print_site ~sigs n) i in
       Format.fprintf f",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@,"
    | Link (y,j) ->
      let n = (cc.id,find_ty cc x,x) in
      let n' = (cc.id,find_ty cc y,y) in
      if x<y || (x=y && i<j) then
        let () = Format.fprintf
            f
            "@[<b>%a ->@ %a@ @[[taillabel=\"%a\",@ headlabel=\"%a\""
            (ContentAgent.print ?sigs:None ?with_id:None) n
            (ContentAgent.print ?sigs:None ?with_id:None) n'
            (ContentAgent.print_site ~sigs n) i
            (ContentAgent.print_site ~sigs n') j in
        Format.fprintf
          f ",@ arrowhead=\"odot\",@ arrowtail=\"odot\",@ dir=\"both\"]@];@]@,"
  in
  let pp_one_internal x i f k =
    let n = (cc.id,find_ty cc x,x) in
    if k >= 0 then
      let () = Format.fprintf
          f "@[%ai@ [label=\"%a\",@ height=\".1\",@ width=\".1\""
          (ContentAgent.print_site ?sigs:None n) i
          (ContentAgent.print_internal ~sigs n i) k in
      let () =
        Format.fprintf f ",@ margin=\".05,.02\",@ fontsize=\"11\"];@]@," in
      let () = Format.fprintf
          f "@[<b>%ai ->@ %a@ @[[headlabel=\"%a\",@ weight=25"
          (ContentAgent.print_site ?sigs:None n) i
          (ContentAgent.print ?sigs:None ?with_id:None) n
          (ContentAgent.print_site ~sigs n) i in
      Format.fprintf f ",@ arrowhead=\"odot\",@ minlen=\".1\"]@];@]@," in
  let pp_slot pp_el f (x,a) =
    Pp.array (fun _ -> ()) (pp_el x) f a in
  Format.fprintf
    f "@[<v>subgraph %i {@,%a%a%a}@]" cc.id
    (Pp.array (fun f -> Format.pp_print_cut f ())
             (fun _ -> Pp.list
                 (fun f -> Format.pp_print_cut f ())
                 (fun f x ->
                    let n = (cc.id,find_ty cc x,x) in
                    Format.fprintf
                      f "@[%a [label=\"%a\"]@];@,"
                      (ContentAgent.print ?sigs:None ?with_id:None) n
                      (ContentAgent.print ~sigs ~with_id:()) n)))
    cc.nodes_by_type
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
       Mods.IntMap.bindings (fun f -> Format.pp_print_cut f ())
       (pp_slot pp_one_node)) cc.links
    (Pp.set ~trailing:(fun f -> Format.pp_print_cut f ())
       Mods.IntMap.bindings (fun f -> Format.pp_print_cut f ())
       (pp_slot pp_one_internal)) cc.internals

let print_sons_dot sigs cc_id cc f sons =
  Pp.list Pp.space ~trailing:Pp.space
    (fun f son -> Format.fprintf
        f "@[cc%i -> cc%i [label=\"%a %a\"];@]" cc_id son.dst
        (Navigation.print_step sigs (find_ty cc)) son.next
        (Pp.list Pp.space Renaming.print) son.inj)
    f sons

let print_point_dot sigs f (id,point) =
  let style =
    match point.is_obs_of with | Some _ -> "octagon" | None -> "box" in
  Format.fprintf f "@[cc%i [label=\"%a\", shape=\"%s\"];@]@,%a"
    point.content.id (print ~sigs  ?with_id:None) point.content
    style (print_sons_dot sigs id point.content) point.sons

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
                     | UnSpec -> assert false
                     | Free -> Edges.add_free a s acc
                     | Link (x',s') ->
                       match Mods.IntMap.find_option x' emb' with
                       | None -> acc
                       | Some ag' -> Edges.add_link (a,ty) s ag' s' acc)
                  g'' (Mods.IntMap.find_default [||] x cc.links) in
              (emb',g''')))
      (Mods.IntMap.empty,graph) cc.nodes_by_type in
  let r =
    Mods.IntMap.fold
      (fun i (a,_) r -> Tools.unsome Renaming.empty (Renaming.add i a r))
      e Renaming.empty  in
  (g,r)

module Env : sig
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: point array;
    single_agent_points: (cc*Operator.DepSet.t) Mods.IntMap.t;
  }

  val navigate :
    t -> Navigation.t -> (int * Renaming.t list * point) option
  val get : t -> int -> point
  val get_single_agent : int -> t -> (cc * Operator.DepSet.t) option
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit
end = struct
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: point array;
    single_agent_points: (cc*Operator.DepSet.t) Mods.IntMap.t;
  }

  let print f env =
    let pp_point f p =
      Format.fprintf
        f "@[<hov 2>(%a)@ -> @[<h>%a@]@ %t-> @[(%a)@]@]"
        (Pp.list Pp.space Format.pp_print_int) p.fathers
        (print ~sigs:env.sig_decl ~with_id:()) p.content
        (fun f ->
           match p.is_obs_of with
           | None -> ()
           | Some deps ->
             Format.fprintf
               f "@[[%a]@]@ "
               (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
               deps)
        (Pp.list
           Pp.space
           (fun f s ->
              Format.fprintf
                f "@[%a%a@ %i[@[%a@]]@]"
                (Navigation.print_step env.sig_decl (find_ty p.content)) s.next
                (Pp.list
                   Pp.space
                   (fun f -> Format.fprintf f "(@[%a@])" Renaming.print))
                s.inj s.dst
                (Pp.set Mods.IntSet.elements Pp.space Format.pp_print_int)
                s.above_obs))
        p.sons in
    Format.fprintf
      f "@[<v>%a%a@]"
      (Pp.set Mods.IntMap.bindings Pp.space ~trailing:Pp.space
         (fun f (_,(cc,deps)) ->
            Format.fprintf
              f "@[<h>%a@] @[[%a]@]" (print ~sigs:env.sig_decl ~with_id:()) cc
              (Pp.set Operator.DepSet.elements Pp.space Operator.print_rev_dep)
              deps))
      env.single_agent_points
      (Pp.array Pp.space (fun _ -> pp_point))
      env.domain

  let get_single_agent ty env =
    Mods.IntMap.find_option ty env.single_agent_points

  let get env cc_id = env.domain.(cc_id)

  let navigate env nav =
    let rec aux injs_dst2nav pt_i = function
      | [] -> Some (pt_i,injs_dst2nav, get env pt_i)
      | e :: t ->
        let rec find_good_edge = function (*one should use a hash here*)
          | [] -> None
          | s :: tail ->
            match Navigation.compatible_point injs_dst2nav s.next e with
            | [] ->  find_good_edge tail
            | inj' ->
              aux (Tools.list_map_flatten
                     (fun x -> List.map (Renaming.compose false x) inj')
                     s.inj) s.dst t
        in find_good_edge (get env pt_i).sons
    in aux [Renaming.empty] 0 nav

  let print_dot f env =
    let () = Format.fprintf f "@[<v>strict digraph G {@," in
    let () =
      Pp.array
        ~trailing:Pp.space Pp.space
        (fun i f s -> print_point_dot (env.sig_decl) f (i,s)) f env.domain in
    Format.fprintf f "}@]@."

end

let propagate_add_obs obs_id env cc_id =
  let rec aux son_id domain cc_id =
    match Mods.IntMap.find_option cc_id domain with
    | None -> assert false
    | Some cc ->
      let sons' =
        Tools.list_smart_map
          (fun s ->
             if s.dst = son_id && not (Mods.IntSet.mem obs_id s.above_obs)
             then {s with above_obs = Mods.IntSet.add obs_id s.above_obs}
             else s) cc.sons in
      if sons' == cc.sons then domain
      else
        let env' =
          Mods.IntMap.add cc_id {cc with sons = sons'} domain in
        List.fold_left (aux cc_id) env' cc.fathers in
  match Mods.IntMap.find_option cc_id env with
  | None -> assert false
  | Some cc -> List.fold_left (aux cc_id) env cc.fathers

exception Found

let remove_ag_cc inj2cc cc_id cc ag_id =
  let ty = find_ty cc ag_id in
  match cc.nodes_by_type.(ty) with
  | [] -> assert false
  | max :: tail as list ->
    let cycle =
      Renaming.cyclic_permutation_from_list
        ~stop_at:ag_id list in
    let to_subst = Renaming.compose true inj2cc cycle in
    let new_nbt =
      Array.mapi (fun i l -> if i = ty then tail else l) cc.nodes_by_type in
    let new_ints,prelinks =
      if Renaming.is_identity to_subst then
        Mods.IntMap.remove ag_id cc.internals, Mods.IntMap.remove ag_id cc.links
      else
        let swip map =
          let map' =
            List.fold_right
              (fun (s,d) map ->
                 if s = max || s = d then map else
                   let tmp = Mods.IntMap.find_default [||] max map in
                   let map' = Mods.IntMap.add
                       max (Mods.IntMap.find_default [||] s map) map in
                   Mods.IntMap.add s tmp map') (Renaming.to_list cycle) map in
          Mods.IntMap.remove max map' in
        swip cc.internals,swip cc.links in
    let new_links =
      Mods.IntMap.map
        (fun a -> Array.map (function
             | (UnSpec | Free) as x -> x
             | Link (n,_) when n = ag_id -> UnSpec
             | Link (n,s) as x ->
               try Link (Renaming.apply cycle n,s)
               with Renaming.Undefined -> x) a) prelinks in
    { id = cc_id; nodes_by_type = new_nbt;
      links = new_links; internals = new_ints;
      recogn_nav = raw_to_navigation false new_nbt new_ints new_links;
      discover_nav = raw_to_navigation true new_nbt new_ints new_links},
    to_subst

let update_cc inj2cc cc_id cc ag_id links internals =
  if
    Array.fold_left
      (fun x -> function UnSpec -> x | (Free | Link _) -> false) true links
    && Array.fold_left (fun x i -> x && i < 0) true internals
  then true,remove_ag_cc inj2cc cc_id cc ag_id
  else
    let new_ints = Mods.IntMap.add ag_id internals cc.internals in
    let new_links = Mods.IntMap.add ag_id links cc.links in
    false,({ id = cc_id;
             nodes_by_type = cc.nodes_by_type;
             internals = new_ints;
             links = new_links;
             recogn_nav =
               raw_to_navigation false cc.nodes_by_type new_ints new_links;
             discover_nav =
               raw_to_navigation true cc.nodes_by_type new_ints new_links},
           inj2cc)

let compute_cycle_edges cc =
  let rec aux don acc path ag_id =
    Tools.array_fold_lefti
      (fun i (don,acc as out) ->
         function
         | UnSpec | Free -> out
         | Link (n',i') ->
       if List.mem n' don then out
       else
         let edge = ((Navigation.Existing ag_id,i),
                     Navigation.ToNode(Navigation.Existing n',i')) in
         if ag_id = n' then (don, edge::acc)
         else
           let rec extract_cycle acc' = function
             | ((Navigation.Existing n,i),_ as e) :: t ->
               if n' = n then
                 if i' = i then out
                 else (don,edge::e::acc')
               else extract_cycle (e::acc') t
             | ((Navigation.Fresh _,_),_) :: _ -> assert false
             | [] ->
               let (don',acc') = aux don acc (edge::path) n' in
               (n'::don',acc') in
           extract_cycle acc path)
      (don,acc) (Mods.IntMap.find_default [||] ag_id cc.links) in
  let rec element i t =
    if i = Array.length t then [] else
      match t.(i) with
      | [] -> element (succ i) t
      | h :: _ -> snd (aux [] [] [] h) in
  element 0 cc.nodes_by_type

let remove_cycle_edges complete_domain_with obs_id dst env free_id cc =
  let rec aux ((f_id,env'),out as acc) = function
    | ((Navigation.Existing n,i),
       Navigation.ToNode(Navigation.Existing n',i')) :: q ->
      let links = Mods.IntMap.find_default [||] n cc.links in
      let int = Mods.IntMap.find_default [||] n cc.internals in
      let links' = Array.copy links in
      let () = links'.(i) <- UnSpec in
      let has_removed,(cc_tmp,inj2cc) =
        update_cc (identity_injection cc) f_id cc n links' int in
      let new_n' = Renaming.apply inj2cc n' in
      let links_dst = Mods.IntMap.find_default [||] new_n' cc_tmp.links in
      let int_dst = Mods.IntMap.find_default [||] new_n' cc_tmp.internals in
      let links_dst' = Array.copy links_dst in
      let () = links_dst'.(i') <- UnSpec in
      let has_removed',(cc',inj2cc') =
        update_cc inj2cc f_id cc_tmp new_n' links_dst' int_dst in
      let e' =
        if n = n' && has_removed'
        then
          (Navigation.Fresh (Renaming.apply inj2cc' n,find_ty cc n),i),
          Navigation.ToNode (Navigation.Existing (Renaming.apply inj2cc' n),i')
        else
          ((if has_removed
            then Navigation.Fresh (Renaming.apply inj2cc' n,find_ty cc n)
            else Navigation.Existing (Renaming.apply inj2cc' n)),i),
          Navigation.ToNode
            ((if has_removed'
              then Navigation.Fresh (Renaming.apply inj2cc' n',find_ty cc n')
              else Navigation.Existing (Renaming.apply inj2cc' n')),i') in
      let pack,ans =
        complete_domain_with obs_id dst env' f_id cc' e' inj2cc' in
      aux (pack,ans::out) q
    | [] -> acc
    | (((Navigation.Existing _,_),Navigation.ToNode(Navigation.Fresh _,_)) |
       ((Navigation.Existing _,_),
        (Navigation.ToInternal _ | Navigation.ToNothing)) |
       ((Navigation.Fresh _,_),_))::_ -> assert false in
  aux ((free_id,env),[]) (compute_cycle_edges cc)

let compute_father_candidates complete_domain_with obs_id dst env free_id cc =
  let agent_is_removable lp links internals =
    try
      let () = Array.iter (fun el -> if el >= 0 then raise Found) internals in
      let () =
        Array.iteri
          (fun i el -> if i<>lp && el<>UnSpec then raise Found) links in
      true
    with Found -> false in
  let remove_one_internal acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'), out as acc) el ->
         if el >= 0 then
           let int' = Array.copy internals in
           let () = int'.(i) <- -1 in
           let has_removed,(cc',inj2cc') =
             update_cc (identity_injection cc) f_id cc ag_id links int' in
           let pack,ans =
             complete_domain_with
               obs_id dst env' f_id cc'
               (((if has_removed
                  then Navigation.Fresh
                      (Renaming.apply inj2cc' ag_id, find_ty cc ag_id)
                  else Navigation.Existing (Renaming.apply inj2cc' ag_id)),i),
                Navigation.ToInternal el)
               inj2cc' in
           (pack,ans::out)
         else acc)
      acc internals in
  let remove_one_frontier acc ag_id links internals =
    Tools.array_fold_lefti
      (fun i ((f_id,env'),out as acc) ->
         function
         | UnSpec -> acc
         | Free ->
           let links' = Array.copy links in
           let () = links'.(i) <- UnSpec in
           let has_removed,(cc',inj2cc') =
             update_cc (identity_injection cc) f_id cc ag_id links' internals in
           let pack,ans =
             complete_domain_with
               obs_id dst env' f_id cc'
               (((if has_removed
                  then Navigation.Fresh
                      (Renaming.apply inj2cc' ag_id, find_ty cc ag_id)
                  else Navigation.Existing (Renaming.apply inj2cc' ag_id)),i),
                Navigation.ToNothing)
               inj2cc' in
           (pack,ans::out)
         | Link (n',i') ->
           if not (agent_is_removable i links internals) then acc else
             let links_dst = Mods.IntMap.find_default [||] n' cc.links in
             let int_dst = Mods.IntMap.find_default [||] n' cc.internals in
             let links_dst' = Array.copy links_dst in
             let () = links_dst'.(i') <- UnSpec in
             let has_removed,(cc',inj2cc') =
               update_cc (identity_injection cc) f_id cc n' links_dst' int_dst in
             let cc'',inj2cc'' =
               remove_ag_cc inj2cc' f_id cc' (Renaming.apply inj2cc' ag_id) in
             let pack,ans =
               complete_domain_with
                 obs_id dst env' f_id cc''
                 (((if has_removed
                    then Navigation.Fresh
                        (Renaming.apply inj2cc'' n', find_ty cc n')
                    else Navigation.Existing (Renaming.apply inj2cc'' n')),i'),
                  Navigation.ToNode
                    (Navigation.Fresh(Renaming.apply inj2cc'' ag_id,find_ty cc ag_id),i))
                 inj2cc'' in
             (pack,ans::out))
      (remove_one_internal acc ag_id links internals) links in
  let remove_or_remove_one acc ag_id links internals =
    remove_one_frontier acc ag_id links internals in
  Mods.IntMap.fold (fun i links acc ->
      remove_or_remove_one
        acc i links (Mods.IntMap.find_default [||] i cc.internals))
    cc.links
    (remove_cycle_edges complete_domain_with obs_id dst env free_id cc)

let get_domain domain id =
  match Mods.IntMap.find_option id domain with
  | None -> assert false
  | Some x -> x

let navigate_domain env nav =
  let rec aux injs_dst2nav pt_i = function
    | [] -> Some (pt_i,injs_dst2nav, get_domain env pt_i)
    | e :: t ->
      let rec find_good_edge = function (*one should use a hash here*)
        | [] -> None
        | s :: tail ->
          match Navigation.compatible_point injs_dst2nav s.next e with
          | [] ->  find_good_edge tail
          | inj' ->
            aux (Tools.list_map_flatten
                   (fun x -> List.map (Renaming.compose false x) inj') s.inj)
              s.dst t
      in find_good_edge (get_domain env pt_i).sons
  in aux [Renaming.empty] 0 nav

let find_domain env cc =
  let nav = to_navigation cc in
  (* let () = Format.eprintf *)
  (*       "@[[%a]@]@,%a@." (Pp.list Pp.space (print_edge (sigs env) cc)) nav *)
  (*       print env in *)
  navigate_domain env nav

let rec complete_domain_with obs_id dst env free_id cc edge inj_dst2cc =
  let rec new_son inj_cc2found = function
    | [] ->
      [{ dst = dst;
         next = Navigation.rename_step inj_cc2found edge;
         inj = [Renaming.compose true inj_dst2cc inj_cc2found];
         above_obs = Mods.IntSet.singleton obs_id;}]
    | h :: t when
        h.dst = dst && (h.next = Navigation.rename_step inj_cc2found edge) ->
      {h with inj = (Renaming.compose true inj_dst2cc inj_cc2found)::h.inj} :: t
    | h :: t -> h :: new_son inj_cc2found t in
  let known_cc = find_domain env cc in
  match known_cc with
  | Some (cc_id, inj_cc_id2cc, point') ->
    let point'' =
      {point' with
       sons = new_son (Renaming.inverse (List.hd inj_cc_id2cc)) point'.sons} in
    let completed =
      propagate_add_obs obs_id (Mods.IntMap.add cc_id point'' env) cc_id in
    (free_id,completed), cc_id
  | None ->
    let son = new_son (identity_injection cc) [] in
    add_new_point ?deps:None obs_id env free_id son cc
and add_new_point ?deps obs_id env free_id sons cc =
  let (free_id'',env'),fathers =
    compute_father_candidates
      complete_domain_with obs_id cc.id env (succ free_id) cc in
  let completed =
    Mods.IntMap.add
      cc.id
      {content = cc;sons=sons; fathers = fathers;
       is_obs_of = if cc.id = obs_id then deps else None;}
      env' in
  ((free_id'',completed),cc.id)

let add_domain ~deps (free_id,singles,env) cc =
  let nav = to_navigation cc in
  if nav = [] then
    match find_root cc with
    | None -> assert false
    | Some (_,ty) -> (free_id,Mods.IntMap.add ty (cc,deps) singles,env)
  else
    let (free_id',env'),_ = add_new_point ~deps cc.id env free_id [] cc in
    (free_id',singles,env')

(** Operation to create cc *)
let check_dangling wk =
  if wk.dangling <> 0 then
    raise (dangling_node ~sigs:wk.sigs wk.used_id wk.dangling)
let check_node_adequacy ~pos wk cc_id =
  if wk.cc_id <> cc_id then
    raise (
      ExceptionDefn.Malformed_Decl
        (Format.asprintf
           "A node from a different connected component has been used."
        ,pos))

module PreEnv : sig
  type t

  val empty : Signature.s -> t
  val fresh :
    Signature.s -> int list array -> int ->
    (cc * Operator.DepSet.t) list Mods.IntMap.t -> t
  val to_work : t -> work

  val sigs : t -> Signature.s

  val finalize : t -> Env.t
  val of_env : Env.t -> t
end = struct
  type t = {
    sig_decl: Signature.s;
    id_by_type: int list array;
    nb_id: int;
    domain: (cc * Operator.DepSet.t) list Mods.IntMap.t;
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

  let fresh_id env =
    succ
      (Mods.IntMap.fold
         (fun _ x acc ->
            List.fold_left (fun acc (cc,_) -> max acc cc.id) acc x)
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
    {content = empty_cc sigs; is_obs_of = None; fathers = []; sons = [];}

  let rec remove_from_sons level1 todos set rfathers cc_id = function
    | [] -> todos,rfathers,set
    | id :: tail ->
      match Mods.IntMap.find_option id set with
      | None -> remove_from_sons level1 todos set rfathers cc_id tail
      | Some cc ->
        match List.partition (fun t -> t.dst = cc_id) cc.sons with
        | [], _ -> remove_from_sons level1 todos set rfathers cc_id tail
        | _x::_,sons ->
          if sons <> [] || cc.is_obs_of <> None || List.mem id level1 then
            (*let missings = (*TODO*)
              List.fold_left
              (fun acc t -> IntSet.minus acc t.above_obs) x.above_obs
              sons in*)
            let sons'= (*if IntSet.is_empty missings then sons else*) cc.sons in
            let rfathers'' =
              (*if IntSet.is_empty missings then rfathers else*) id::rfathers in
            remove_from_sons
              level1 (Mods.IntMap.add id cc.fathers todos)
              (Mods.IntMap.add id {cc with sons = sons'} set)
              rfathers'' cc_id tail
          else
            let todos',rfathers',set' =
              remove_from_sons
            level1 (Mods.IntMap.remove id todos) set [] id cc.fathers in
            let rfathers,set'' =
              match rfathers' with
              | _ :: _ ->
                id::rfathers,
                Mods.IntMap.add id {cc with fathers = rfathers'} set'
              | [] -> rfathers,Mods.IntMap.remove id set' in
            remove_from_sons level1 todos' set'' rfathers cc_id tail
  let rec scan_sons level1 todos set (injs,rfathers,cands) cc_id p = function
    | [] ->
      let rfathers' =
        List.fold_left (fun acc (_,i) -> i::acc) rfathers cands in
      todos,Mods.IntMap.add cc_id {p with fathers = rfathers'} set
    | id :: tail ->
      match Mods.IntMap.find_option id set with
      | None -> scan_sons level1 todos set (injs,rfathers,cands) cc_id p tail
      | Some cc ->
        let injs',cands',tail' =
          List.fold_left
            (fun acc s ->
               if s.dst = cc_id then
                 List.fold_left
                   (fun (l,c,t) r ->
                      let oui,non =
                        List.partition
                          (fun (x,_) -> Renaming.equal x r) c in
                      r::l,non,Tools.list_rev_map_append snd oui t)
                   acc s.inj
               else acc)
            ([],cands,tail) cc.sons in
        if List.length injs' > 1 || List.length cc.sons > 1
           || cc.is_obs_of <> None || List.mem id level1
        then
          scan_sons level1 (Mods.IntMap.add id cc.fathers todos) set
            (injs'@injs,id::rfathers,cands') cc_id p tail'
        else
          let rroots =
            List.filter
              (fun r -> not (List.exists (Renaming.equal r) injs)) injs' in
          let cands'' =
            List.fold_left
              (fun c r ->
                 if List.exists (fun (x,_) -> Renaming.equal x r) c
                 then c else (r,id)::c)
              cands rroots in
          if cands == cands'' then
            let todos',rfathers',set' =
              remove_from_sons
                level1 (Mods.IntMap.remove id todos) set [] id cc.fathers in
            let rr,set'',tail'' =
              match rfathers' with
              | _ :: _ ->
                (injs'@injs,id::rfathers,cands'),
                Mods.IntMap.add id {cc with fathers = rfathers'} set',tail'
              | [] -> (injs,rfathers,cands),Mods.IntMap.remove id set',tail in
            scan_sons level1 todos' set'' rr cc_id p tail''
          else
            scan_sons level1 todos set (injs,rfathers,cands'') cc_id p tail

  let finalize env =
    let _,singles,domain =
      Mods.IntMap.fold
        (fun _ x acc ->
           List.fold_left
             (fun acc (cc,deps) -> add_domain ~deps acc cc) acc x)
        env.domain
        (fresh_id env,Mods.IntMap.empty,
         Mods.IntMap.add 0 (empty_point env.sig_decl) Mods.IntMap.empty) in
    let level1 = match Mods.IntMap.find_option 0 domain with
      | None -> assert false
      | Some zero -> List.map (fun p -> p.dst) zero.sons in
    let rec iter (todos,env) =
      if Mods.IntMap.is_empty todos then env else
        let out =
          Mods.IntMap.fold
            (fun id fa (todos,s) ->
               match Mods.IntMap.find_option id s with
               | None -> (todos,s)
               | Some p -> scan_sons level1 todos s ([],[],[]) id p fa)
            todos (Mods.IntMap.empty,env) in
        iter out in
    let tops =
      Mods.IntMap.fold
        (fun id p s ->
           if p.sons = [] && not (List.mem id level1)
           then Mods.IntMap.add id p.fathers s else s)
        domain Mods.IntMap.empty in
    let s' = iter (tops,domain) in
    let si = match Mods.IntMap.max_key s' with Some i -> succ i | None -> 0 in
    let out = Array.make si (empty_point env.sig_decl) in
    let () = Mods.IntMap.iter (fun i p -> out.(i) <- p) s' in
    {
      Env.sig_decl = env.sig_decl;
      Env.id_by_type =env.id_by_type;
      Env.nb_id = env.nb_id;
      Env.domain = out;
      Env.single_agent_points = singles;
    }

  let of_env env =
    let add_cc acc (cc,_ as p) =
      let w = weight cc in
      Mods.IntMap.add
        w (p::Mods.IntMap.find_default [] w acc) acc in
    let domain =
      Mods.IntMap.fold
        (fun _ p acc -> add_cc acc p) env.Env.single_agent_points
        Mods.IntMap.empty in
    let domain' =
      Array.fold_left (fun acc p ->
          match p.is_obs_of with
          | None -> acc
          | Some deps -> add_cc acc (p.content,deps))
        domain env.Env.domain in
    {
      sig_decl = env.Env.sig_decl;
      nb_id = env.Env.nb_id;
      id_by_type = env.Env.id_by_type;
      domain = domain';
      used_by_a_begin_new = false;
    }
end

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
    { id = wk.cc_id; nodes_by_type = wk.used_id;
      links = wk.cc_links; internals = wk.cc_internals;
      recogn_nav =
        raw_to_navigation false wk.used_id wk.cc_internals wk.cc_links;
      discover_nav =
        raw_to_navigation true wk.used_id wk.cc_internals wk.cc_links} in
  let w = weight cc_candidate in
  let env_w,r,out =
    let rec aux = function
      | [] -> [cc_candidate,add_origin Operator.DepSet.empty origin],
              identity_injection cc_candidate,cc_candidate
      | (h,deps) :: t -> match equal cc_candidate h with
        | None -> let a,b,c = aux t in (h,deps)::a,b,c
        | Some r -> (h,add_origin deps origin)::t,r,h in
    aux (Mods.IntMap.find_default [] w wk.cc_env) in
  PreEnv.fresh wk.sigs wk.reserved_id wk.free_id
    (Mods.IntMap.add w env_w wk.cc_env),r,out

let new_link wk ((cc1,_,x as n1),i) ((cc2,_,y as n2),j) =
  let pos = Location.dummy in
  let () = check_node_adequacy ~pos wk cc1 in
  let () = check_node_adequacy ~pos wk cc2 in
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

let new_free wk ((cc,_,x as n),i) =
  let () = check_node_adequacy ~pos:Location.dummy wk cc in
  let x_n = Mods.IntMap.find_default [||] x wk.cc_links in
  if x_n.(i) <> UnSpec then
    raise (already_specified ~sigs:wk.sigs n i)
  else
    let () = x_n.(i) <- Free in
    wk

let new_internal_state wk ((cc,_,x as n), i) va =
  let () = check_node_adequacy ~pos:Location.dummy wk cc in
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
    let node = (wk.cc_id,type_id,h) in
    (node,
     { wk with
       dangling = if Mods.IntMap.is_empty wk.cc_links then 0 else h;
       cc_links = Mods.IntMap.add h (Array.make arity UnSpec) wk.cc_links;
       cc_internals = Mods.IntMap.add h (Array.make arity (-1)) wk.cc_internals;
     })
  | [] ->
    let () = wk.used_id.(type_id) <- wk.free_id :: wk.used_id.(type_id) in
    let node = (wk.cc_id, type_id, wk.free_id) in
    (node,
     { wk with
       free_id = succ wk.free_id;
       dangling = if Mods.IntMap.is_empty wk.cc_links then 0 else wk.free_id;
       cc_links =
         Mods.IntMap.add wk.free_id (Array.make arity UnSpec) wk.cc_links;
       cc_internals =
         Mods.IntMap.add wk.free_id (Array.make arity (-1)) wk.cc_internals;
     })

module NodeSetMap = SetMap.Make(struct type t = ContentAgent.t
    let compare = ContentAgent.compare
    let print = ContentAgent.print ?sigs:None ~with_id:()
  end)
module NodeMap = NodeSetMap.Map

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
  let reconstruct graph inj id cc root =
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
  let is_root_of graph (_,rty as root) cc =
    match cc.discover_nav with
    | [] ->
      (match find_root cc with
       | Some (_,rty') -> rty = rty'
       | None -> false)
    | nav -> aux_is_root_of graph (Some root) Renaming.empty nav

  (* get : (ContentAgent.t * int) -> t -> int *)
  let get ((_,_,node),id) (t,_) =
    Renaming.apply (Mods.IntMap.find_default Renaming.empty id t) node

  let elements_with_types ccs (t,_) =
    let out = Array.make (Mods.IntMap.size t) [] in
    let () =
      Mods.IntMap.iter
        (fun id map ->
           out.(id) <- Renaming.fold
               (fun i out acc -> (out,find_ty ccs.(id) i)::acc) map [])
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

  (*edges: list of concrete edges,
    returns the roots of observables that are above in the domain*)
  let from_edge domain graph acc edges =
    let get_root inj point =
      match find_root point.content with
      | None -> assert false
      | Some (root,root_type) -> Renaming.apply inj root,root_type in
    let rec aux_from_edges cache (obs,rev_deps as acc) = function
      | [] -> acc,cache
      | (pid,point,inj_point2graph) :: remains ->
        let root_bundle =
          get_root inj_point2graph point in
        let acc' =
          match point.is_obs_of with
          | None -> acc
          | Some ndeps ->
            ((point.content,root_bundle) :: obs,
             Operator.DepSet.union rev_deps ndeps) in
        let remains' =
          List.fold_left
            (fun re son ->
               match Navigation.injection_for_one_more_edge
                       inj_point2graph graph son.next with
               | None -> re
               | Some inj' ->
                 let p' = Env.get domain son.dst in
                 List.fold_left
                   (fun remains renaming ->
                      let rename = Renaming.compose false renaming inj' in
                      let next = (son.dst,p',rename) in
                      if CacheSetMap.Set.mem
                          (son.dst,Renaming.min_elt rename) cache
                      then remains
                      else next::remains) re son.inj)
            remains point.sons in
        aux_from_edges
          (CacheSetMap.Set.add (pid,Renaming.min_elt inj_point2graph) cache)
          acc' remains' in
    match Env.navigate domain edges with
    | None -> acc
    | Some (pid,injs,point) ->
      List.fold_left
        (fun (out,cache) inj -> aux_from_edges cache out [(pid,point,inj)])
        acc injs

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
      [(Navigation.Fresh node,site),Navigation.ToNothing]
  let observables_from_internal domain graph acc node site id =
    from_edge domain graph acc
      [(Navigation.Fresh node,site),Navigation.ToInternal id]
  let observables_from_link domain graph acc n site  n' site' =
    from_edge domain graph acc
      [(Navigation.Fresh n,site),
       Navigation.ToNode (Navigation.Fresh n',site')]
end

let compare_canonicals cc cc' = Mods.int_compare cc.id cc'.id
let is_equal_canonicals cc cc' = compare_canonicals cc cc' = 0

module ForState = struct
  type t = cc
  let compare = compare_canonicals
  let print = print ?sigs:None ~with_id:()
end

module SetMap = SetMap.Make(ForState)
module Set = SetMap.Set
module Map = SetMap.Map
