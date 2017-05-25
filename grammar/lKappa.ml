(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type switching =
  | Linked of int Locality.annot | Freed | Maintained | Erased

type rule_internal =
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int

type rule_agent =
  {
    ra_type: int;
    ra_erased: bool;
    ra_ports:
      ((int,int*int) Ast.link Locality.annot * switching) array;
    ra_ints: rule_internal array;
    ra_syntax:
      (((int,int*int) Ast.link Locality.annot * switching) array *
       rule_internal array) option;
  }

type rule_mixture = rule_agent list

type rule_agent_counters =
  {
    ra : rule_agent;
    ra_counters : (Ast.counter * switching) array;
    ra_created : bool;
  }

type 'agent rule =
  {
    r_mix: 'agent list;
    r_created: Raw_mixture.t;
    r_delta_tokens :
      ((rule_mixture,int) Alg_expr.e Locality.annot * int) list;
    r_rate : (rule_mixture,int) Alg_expr.e Locality.annot;
    r_un_rate : ((rule_mixture,int) Alg_expr.e Locality.annot
                 * (rule_mixture,int) Alg_expr.e Locality.annot option) option;
    r_editStyle: bool;
  }

let print_link_annot ~ltypes sigs f (s,a) =
  if ltypes then
    Format.fprintf f "(*%a.%a*)"
      (Signature.print_site sigs a) s
      (Signature.print_agent sigs) a

let print_rule_internal sigs ~show_erased ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j ->
    Format.fprintf f "/~%a" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED -> if show_erased then Format.fprintf f "~--"
  | I_VAL_CHANGED (i,j) ->
    if i <> j then
      Format.fprintf
        f "~%a/~%a" (Signature.print_internal_state sigs ag_ty site) i
        (Signature.print_internal_state sigs ag_ty site) j
    else
      Format.fprintf f "~%a" (Signature.print_internal_state sigs ag_ty site) i
  | I_VAL_ERASED i ->
    Format.fprintf
      f "~%a%t" (Signature.print_internal_state sigs ag_ty site) i
      (fun f -> if show_erased then Format.pp_print_string f "--")

let rule_internal_to_json = function
  | I_ANY -> `Null
  | I_ANY_CHANGED j -> `List [`String "ANY"; `Int j]
  | I_ANY_ERASED -> `String "ERASED"
  | I_VAL_CHANGED (i,j) -> `List [`Int i; `Int j]
  | I_VAL_ERASED i -> `List [ `Int i; `String "ERASED"]

let rule_internal_of_json = function
  | `Null -> I_ANY
  | `List [`String "ANY"; `Int j] -> I_ANY_CHANGED j
  | `String "ERASED" -> I_ANY_ERASED
  | `List [`Int i; `Int j] -> I_VAL_CHANGED (i,j)
  | `List [ `Int i; `String "ERASED"] -> I_VAL_ERASED i
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect rule_internal",x))

let print_switching ~show_erased f = function
  | Linked (i,_) -> Format.fprintf f "/!%i" i
  | Freed -> Format.pp_print_string f "/!."
  | Maintained -> ()
  | Erased -> if show_erased then Format.pp_print_string f "--"

let switching_to_json = function
  | Freed -> `String "Freed"
  | Maintained -> `String "Maintained"
  | Erased -> `String "Erased"
  | Linked i -> Locality.annot_to_json JsonUtil.of_int i

let switching_of_json = function
  | `String "Freed" -> Freed
  | `String "Maintained" -> Maintained
  | `String "Erased"-> Erased
  | x -> Linked (Locality.annot_of_json
                   (JsonUtil.to_int~error_msg:"Invalid Switching") x)

let print_rule_link sigs ~show_erased ~ltypes f ((e,_),s) =
  Format.fprintf
    f "%a%a"
    (Ast.print_link
       ~syntax_version:Ast.V4 (Signature.print_site sigs)
       (Signature.print_agent sigs) (print_link_annot ~ltypes sigs))
    e
    (print_switching ~show_erased) s

let print_rule_intf sigs ~show_erased ~ltypes ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
         | (Ast.LNK_ANY, _), Maintained ->  ints.(i) <> I_ANY
         | ((Ast.LNK_ANY, _), (Erased | Freed | Linked _) |
            ((Ast.LNK_SOME | Ast.ANY_FREE | Ast.LNK_FREE |
              Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true) then
        let () = Format.fprintf
            f "%t%a%a%a" (if empty then Pp.empty else Pp.comma)
            (Signature.print_site sigs ag_ty) i
            (print_rule_internal sigs ~show_erased ag_ty i)
            ints.(i) (print_rule_link sigs ~show_erased ~ltypes) ports.(i) in
        aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_rule_agent sigs ~ltypes f ag =
  Format.fprintf f "%t%a(@[<h>%a@])"
    (fun f -> if ag.ra_erased then Format.pp_print_string f "-")
    (Signature.print_agent sigs) ag.ra_type
    (print_rule_intf sigs ~show_erased:false ~ltypes ag.ra_type)
    (ag.ra_ports,ag.ra_ints)

let print_rule_mixture sigs ~ltypes f mix =
  Pp.list Pp.comma (print_rule_agent sigs ~ltypes) f mix

let print_internal_lhs sigs ag_ty site f = function
  | (I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED) -> ()
  | (I_VAL_CHANGED (i,_) | I_VAL_ERASED i) ->
    Format.fprintf f "~%a" (Signature.print_internal_state sigs ag_ty site) i

let print_internal_rhs sigs ag_ty site f = function
  | I_ANY -> ()
  | (I_ANY_CHANGED j | I_VAL_CHANGED (_,j)) ->
    Format.fprintf f "~%a" (Signature.print_internal_state sigs ag_ty site) j
  | (I_ANY_ERASED | I_VAL_ERASED _) -> assert false

let print_link_lhs ~ltypes sigs f ((e,_),_) =
  Ast.print_link
    ~syntax_version:Ast.V3 (Signature.print_site sigs)
    (Signature.print_agent sigs) (print_link_annot ~ltypes sigs)
    f e

let print_link_rhs ~ltypes sigs f ((e,_),s) =
  match s with
  | Linked (i,_) ->
    Ast.print_link
      ~syntax_version:Ast.V3 (Signature.print_site sigs) (Signature.print_agent sigs)
      (fun _ () -> ()) f (Ast.LNK_VALUE (i,()))
  | Freed -> ()
  | Maintained ->
    Ast.print_link
      ~syntax_version:Ast.V3 (Signature.print_site sigs)
      (Signature.print_agent sigs) (print_link_annot ~ltypes sigs)
      f e
  | Erased -> assert false

let print_intf_lhs ~ltypes sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
          | (((Ast.LNK_SOME | Ast.LNK_FREE | Ast.ANY_FREE |
               Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true
          | (Ast.LNK_ANY, _), _ ->
            match ints.(i) with
            | (I_ANY | I_ANY_ERASED | I_ANY_CHANGED _) -> false
            | ( I_VAL_CHANGED _ | I_VAL_ERASED _) -> true) then
        let () = Format.fprintf
            f "%t%a%a%a"
            (if empty then Pp.empty else Pp.compact_comma)
            (Signature.print_site sigs ag_ty) i
            (print_internal_lhs sigs ag_ty i)
            ints.(i) (print_link_lhs ~ltypes sigs) ports.(i) in
        aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_intf_rhs ~ltypes sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
          | (((Ast.LNK_SOME | Ast.LNK_FREE |  Ast.ANY_FREE |
               Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true
          | ((Ast.LNK_ANY, _), (Erased | Freed | Linked _)) -> true
          | ((Ast.LNK_ANY, _), Maintained) ->
            match ints.(i) with
            | I_ANY -> false
            | I_VAL_CHANGED (i,j) -> i <> j
            | (I_ANY_ERASED | I_ANY_CHANGED _ | I_VAL_ERASED _) -> true) then
        let () = Format.fprintf
            f "%t%a%a%a"
            (if empty then Pp.empty else Pp.compact_comma)
            (Signature.print_site sigs ag_ty) i
            (print_internal_rhs sigs ag_ty i)
            ints.(i) (print_link_rhs ~ltypes sigs) ports.(i) in
        aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_agent_lhs ~ltypes sigs f ag =
  Format.fprintf
    f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.ra_type
    (print_intf_lhs ~ltypes sigs ag.ra_type) (ag.ra_ports,ag.ra_ints)

let print_agent_rhs ~ltypes sigs f ag =
  if not ag.ra_erased then
    Format.fprintf
      f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.ra_type
      (print_intf_rhs ~ltypes sigs ag.ra_type) (ag.ra_ports,ag.ra_ints)

let print_rhs ~ltypes sigs created f mix =
  let rec aux empty = function
    | [] ->
      Format.fprintf f "%t%a"
        (if empty || created = [] then Pp.empty else Pp.comma)
        (Raw_mixture.print ~explicit_free:false ~compact:true ~created:false ~sigs)
        created
    | h :: t ->
      if h.ra_erased then aux empty t
      else
        let () = Format.fprintf f "%t%a"
            (if empty then Pp.empty else Pp.comma)
            (print_agent_rhs ~ltypes sigs) h in
        aux false t in
  aux true mix

let print_rates sigs pr_tok pr_var f r =
  let ltypes = false in
  Format.fprintf
    f " @@ %a%t"
    (Alg_expr.print
       (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs ~ltypes) m)
       pr_tok pr_var) (fst r.r_rate)
    (fun f ->
       match r.r_un_rate with
       | None -> ()
       | Some ((ra,_),max_dist) ->
         Format.fprintf
           f " {%a%a}"
           (Alg_expr.print
              (fun f m -> Format.fprintf f "|%a|"
                  (print_rule_mixture sigs ~ltypes) m)
              pr_tok pr_var) ra
           (Pp.option
              (fun f (md,_) ->
                Format.fprintf f ":%a"
                (Alg_expr.print
                   (fun f m -> Format.fprintf f "|%a|"
                       (print_rule_mixture sigs ~ltypes) m)
                   pr_tok pr_var) md)) max_dist)

let print_rule ~full sigs pr_tok pr_var f r =
  Format.fprintf
    f "@[<h>%t%t%a%t@]"
    (fun f ->
       if full || r.r_editStyle then
         Format.fprintf f "%a%t%a"
           (print_rule_mixture sigs ~ltypes:false) r.r_mix
           (fun f -> if r.r_mix <> [] && r.r_created <> [] then Pp.comma f)
           (Raw_mixture.print ~explicit_free:true ~compact:false ~created:true ~sigs)
           r.r_created
       else Format.fprintf f "%a -> %a"
           (Pp.list Pp.comma (print_agent_lhs ~ltypes:false sigs)) r.r_mix
           (print_rhs ~ltypes:false sigs r.r_created) r.r_mix)
    (fun f ->
       match r.r_delta_tokens with [] -> ()
                                 | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (fun f ((nb,_),tk) ->
          Format.fprintf
            f "%a %a"
            (Alg_expr.print
               (fun f m -> Format.fprintf
                   f "|%a|" (print_rule_mixture sigs ~ltypes:false) m)
               pr_tok pr_var) nb
            pr_tok tk))
    r.r_delta_tokens
    (fun f -> if full then print_rates sigs pr_tok pr_var f r)

let rule_agent_to_json a =
  `Assoc [
    "type", `Int a.ra_type;
    "bindings",
    `List (Array.fold_right
             (fun (e,s) c ->
                (`List [
                    Locality.annot_to_json
                      (Ast.link_to_json (fun _ i -> `Int i) (fun i -> `Int i)
                         (fun (s,a) -> [`Int s;`Int a])) e;
                       switching_to_json s])::c)
             a.ra_ports []);
    "states",
    `List (Array.fold_right
             (fun x c -> rule_internal_to_json x :: c) a.ra_ints []);
    "erased", `Bool a.ra_erased;
  ]
let rule_agent_of_json = function
  | `Assoc l as x when List.length l = 4 ->
    begin
      try
        let ports =
          match List.assoc "bindings" l with
          | `List s ->
            Tools.array_map_of_list
              (function
                | `List [e;s] ->
                  (Locality.annot_of_json
                     (Ast.link_of_json
                        (fun _ -> Yojson.Basic.Util.to_int)
                        Yojson.Basic.Util.to_int
                        (function
                          | [`Int s; `Int a] -> (s,a)
                          | _ -> raise Not_found)) e,
                  switching_of_json s)
                | _ -> raise Not_found) s
          | _ -> raise Not_found in
        let ints =
          match List.assoc "states" l with
          | `List s ->
            Tools.array_map_of_list rule_internal_of_json s
          | _ -> raise Not_found in
        {
          ra_type = Yojson.Basic.Util.to_int (List.assoc "type" l);
          ra_ports = ports;
          ra_ints = ints;
          ra_erased = Yojson.Basic.Util.to_bool (List.assoc "erased" l);
          ra_syntax = Some (ports,ints);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent",x))

let rule_mixture_to_json = JsonUtil.of_list rule_agent_to_json
let rule_mixture_of_json = JsonUtil.to_list rule_agent_of_json

let lalg_expr_to_json =
  Alg_expr.e_to_yojson rule_mixture_to_json JsonUtil.of_int
let lalg_expr_of_json =
  Alg_expr.e_of_yojson rule_mixture_of_json (JsonUtil.to_int ?error_msg:None)

let rule_to_json r =
  `Assoc
    [
      "mixture", rule_mixture_to_json r.r_mix;
      "created", Raw_mixture.to_json r.r_created;
      "delta_tokens",
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
           (Locality.annot_to_json lalg_expr_to_json)
           JsonUtil.of_int)
        r.r_delta_tokens;
      "rate", Locality.annot_to_json lalg_expr_to_json r.r_rate;
      "unary_rate",
      JsonUtil.of_option
        (JsonUtil.of_pair
           (Locality.annot_to_json lalg_expr_to_json)
           (JsonUtil.of_option (Locality.annot_to_json lalg_expr_to_json)))
        r.r_un_rate;
      "editStyle", `Bool r.r_editStyle;
    ]
let rule_of_json = function
  | `Assoc l as x when List.length l < 7 ->
    begin
      try
        {
          r_mix = rule_mixture_of_json (List.assoc "mixture" l);
          r_created = Raw_mixture.of_json (List.assoc "created" l);
          r_delta_tokens =
            JsonUtil.to_list
              (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                 (Locality.annot_of_json lalg_expr_of_json)
                 (JsonUtil.to_int ?error_msg:None))
              (List.assoc "delta_tokens" l);
          r_rate =
            Locality.annot_of_json lalg_expr_of_json (List.assoc "rate" l);
          r_un_rate =
            (try
               JsonUtil.to_option
                 (JsonUtil.to_pair
                    (Locality.annot_of_json lalg_expr_of_json)
                    (JsonUtil.to_option (Locality.annot_of_json
                                           lalg_expr_of_json)))
                 (List.assoc "unary_rate" l)
             with Not_found -> None);
           r_editStyle = Yojson.Basic.Util.to_bool (List.assoc "editStyle" l);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect rule",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect rule",x))

let build_l_type sigs pos dst_ty dst_p switch =
  let ty_id = Signature.num_of_agent dst_ty sigs in
  let p_id = Signature.id_of_site dst_ty dst_p sigs in
  ((Ast.LNK_TYPE (p_id,ty_id),pos),switch)

let add_link_contact_map ?contact_map sty sp dty dp =
  match contact_map with
  | None -> ()
  | Some contact_map ->
    let si, sl = contact_map.(sty).(sp) in
    let di,dl = contact_map.(dty).(dp) in
    let () = contact_map.(sty).(sp) <-
        si, List_util.merge_uniq Mods.int_pair_compare sl [dty,dp] in
    contact_map.(dty).(dp) <-
      di, List_util.merge_uniq Mods.int_pair_compare dl [sty,sp]

let build_link sigs ?contact_map pos i ag_ty p_id switch (links_one,links_two) =
  if Mods.IntMap.mem i links_two then
    raise (ExceptionDefn.Malformed_Decl
             ("This is the third occurence of link '"^string_of_int i
              ^"' in the same mixture.",pos))
  else match Mods.IntMap.pop i links_one with
    | None,one' ->
      let new_link = match switch with
        | Linked (j,_) -> Some j
        | Freed | Erased | Maintained -> None in
      ((Ast.LNK_VALUE (i,(-1,-1)),pos),switch),
      (Mods.IntMap.add i (ag_ty,p_id,new_link,pos) one',links_two)
    | Some (dst_ty,dst_p,dst_id,_),one' ->
      if Signature.allowed_link ag_ty p_id dst_ty dst_p sigs then
        let () = add_link_contact_map ?contact_map ag_ty p_id dst_ty dst_p in
        let maintained = match switch with
          | Linked (j,_) -> Some j = dst_id
          | Freed | Erased | Maintained -> false in
        ((Ast.LNK_VALUE (i,(dst_p,dst_ty)),pos),
         if maintained then Maintained else switch),
        (one',Mods.IntMap.add i (ag_ty,p_id,maintained) links_two)
      else
        raise (ExceptionDefn.Malformed_Decl
                 (Format.asprintf
                    "Forbidden link to a %a.%a from signature declaration"
                    (Signature.print_site sigs dst_ty) dst_p
                    (Signature.print_agent sigs) dst_ty,
                  pos))

let forbid_modification pos = function
  | None -> ()
  | Some _ ->
    raise (ExceptionDefn.Malformed_Decl
             ("A modification is forbidden here.",pos))

let several_internal_states pos =
  raise (ExceptionDefn.Malformed_Decl
           ("In a pattern, a site cannot have several internal states.",pos))

let not_enough_specified agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
           ("The link status of agent '"^agent_name^"', site '"^na
            ^"' on the right hand side is underspecified",pos))

let several_occurence_of_site agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
           ("Site '"^na^
            "' occurs more than once in this agent '"^agent_name^"'",pos))

let link_only_one_occurence i pos =
  raise (ExceptionDefn.Malformed_Decl
           ("The link '"^string_of_int i^
            "' occurs only one time in the mixture.", pos))

let copy_rule_agent a =
  let p = Array.copy a.ra_ports in
  let i = Array.copy a.ra_ints in
  { ra_type = a.ra_type; ra_erased = a.ra_erased; ra_ports = p;
    ra_ints = i;
    ra_syntax =
      Option_util.map (fun _ -> Array.copy p, Array.copy i)
        a.ra_syntax;}

let to_erased sigs x =
  List.map
    (fun r ->
       let ports = Array.map (fun (a,_) -> a,Erased) r.ra_ports in
       let ints =
         Array.mapi (fun j -> function
             | I_VAL_CHANGED (i,_) | I_VAL_ERASED i -> I_VAL_ERASED
                                                         i
             | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED ->
               match Signature.default_internal_state r.ra_type j
                       sigs with
               | Some _ -> I_ANY_ERASED
               | None -> I_ANY) r.ra_ints in
       { ra_type = r.ra_type; ra_erased = true; ra_ports = ports;
         ra_ints =ints;
         ra_syntax =
           match r.ra_syntax with
           | None -> None
           | Some _ -> Some (Array.copy ports,Array.copy ints);})
    x

let to_maintained x =
  List.map
    (fun r ->
       let ports = Array.map (fun (a,_) -> a,Maintained) r.ra_ports in
       let ints =
         Array.map (function
             | I_VAL_CHANGED (i,_) | I_VAL_ERASED i -> I_VAL_CHANGED (i,i)
             | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED -> I_ANY
           ) r.ra_ints in
       { ra_type = r.ra_type; ra_erased = false; ra_ports = ports; ra_ints=ints;
         ra_syntax =
           match r.ra_syntax with None -> None | Some _ -> Some (ports,ints);})
    x

let to_raw_mixture sigs x =
  List.map
    (fun r ->
       let internals =
         Array.mapi
           (fun j -> function
              | I_VAL_CHANGED (i,_) | I_VAL_ERASED i -> Some i
              | (I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED) ->
                Signature.default_internal_state r.ra_type j sigs)
           r.ra_ints in
       let ports =
         Array.mapi
           (fun j -> function
              | ((Ast.LNK_SOME, pos) | (Ast.LNK_TYPE _,pos)),_ ->
                let ag_na =
                  Format.asprintf
                    "%a" (Signature.print_agent sigs) r.ra_type in
                let p_na =
                  Format.asprintf
                    "%a" (Signature.print_site sigs r.ra_type) j in
                not_enough_specified ag_na (p_na,pos)
              | (Ast.LNK_VALUE (i,_), _),_ -> Raw_mixture.VAL i
              | (((Ast.LNK_ANY | Ast.ANY_FREE | Ast.LNK_FREE), _)),_ ->
                Raw_mixture.FREE
           )
           r.ra_ports in
       { Raw_mixture.a_type = r.ra_type;
         Raw_mixture.a_ports = ports; Raw_mixture.a_ints =
                                        internals; })
    x

let of_raw_mixture x =
  List.map
    (fun r ->
       let internals =
         Array.map
           (function
             | Some i -> I_VAL_CHANGED (i,i)
             | None -> I_ANY)
           r.Raw_mixture.a_ints in
       let ports =
         Array.map
           (function
             | Raw_mixture.VAL i ->
               (Locality.dummy_annot (Ast.LNK_VALUE (i,(-1,-1))),
                Maintained)
             | Raw_mixture.FREE ->
               (Locality.dummy_annot Ast.LNK_FREE, Maintained)
           )
           r.Raw_mixture.a_ports in
       { ra_type = r.Raw_mixture.a_type; ra_erased = false;
         ra_ports = ports; ra_ints = internals;
         ra_syntax = Some (Array.copy ports, Array.copy internals); })
    x

let incr_agent sigs =
  let id = Signature.num_of_agent ("__incr",Locality.dummy) sigs in
  let incr = Signature.get sigs id in
  let arity = Signature.arity sigs id in
  let after = Signature.num_of_site ("a",Locality.dummy) incr in
  let before = Signature.num_of_site ("b",Locality.dummy) incr in
  (id,arity,before,after)

let annotate_dropped_agent
    ~syntax_version sigs links_annot (agent_name, _ as ag_ty) intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, Erased) in
  let internals =
    Array.init arity
      (fun i ->
         match Signature.default_internal_state ag_id i sigs with
         | None -> I_ANY | Some _ -> I_ANY_ERASED) in
  let lannot,_ =
    List.fold_left
      (fun (lannot,pset) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in
         let () = forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = forbid_modification p_pos p.Ast.port_int_mod in

         let () = match p.Ast.port_int with
           | [] -> ()
           | [ va ] ->
             internals.(p_id) <-
               I_VAL_ERASED (Signature.num_of_internal_state p_id va sign)
           | _ :: (_, pos) :: _ -> several_internal_states pos in
         match p.Ast.port_lnk with
         | [Ast.LNK_ANY, pos] ->
           let () =
             ports.(p_id) <- ((Ast.ANY_FREE,pos), Erased) in (lannot,pset')
         | [Ast.LNK_SOME, pos_lnk] ->
           let (na,pos) = p.Ast.port_nme in
           let () =
             ExceptionDefn.warning
               ~pos
               (fun f ->
                  Format.fprintf
                    f "breaking a semi-link on site '%s' will induce a side effect"
                    na) in
           let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Erased) in
           (lannot,pset')
         | [Ast.LNK_TYPE (dst_p, dst_ty),pos_lnk] ->
           let (na,pos) = p.Ast.port_nme in
           let () =
             ExceptionDefn.warning
               ~pos
               (fun f ->
                  Format.fprintf
                    f "breaking a semi-link on site '%s' will induce a side effect"
                    na) in
           let () = ports.(p_id) <-
               build_l_type sigs pos_lnk dst_ty dst_p Erased in
           (lannot,pset')
         | [Ast.ANY_FREE,_] | [] when syntax_version = Ast.V4 ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.ANY_FREE, Erased in
           (lannot,pset')
         | [Ast.ANY_FREE,_] | [] ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.LNK_FREE, Erased in
           (lannot,pset')
         | [Ast.LNK_FREE,_] ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.LNK_FREE, Erased in
           (lannot,pset')
         | [Ast.LNK_VALUE (i,()), pos] ->
           let va,lannot' =
             build_link sigs pos i ag_id p_id Erased lannot in
           let () = ports.(p_id) <- va in (lannot',pset')
         | _::(_,pos)::_ ->
           raise (ExceptionDefn.Malformed_Decl
                    ("Several link state for a single site",pos)))
      (links_annot,Mods.IntSet.empty) intf in
  let ra_counters = Array.make arity (Ast.empty_counter, Maintained) in
  let _ =
    List.fold_left
      (fun pset c ->
        let p_na = c.Ast.count_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let pset' = Mods.IntSet.add p_id pset in
        let () = if pset == pset' then
             several_occurence_of_site agent_name c.Ast.count_nme in
        let () = ra_counters.(p_id) <-(c,Erased) in pset')
      Mods.IntSet.empty counts in
  let ra =
    { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = true;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  {ra; ra_counters; ra_created = false},lannot

let annotate_created_agent
    ~syntax_version sigs ?contact_map rannot (agent_name, _ as ag_ty) intf =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Raw_mixture.FREE) in
  let internals =
    Array.init arity
      (fun i ->
         Signature.default_internal_state ag_id i sigs) in
  let _,rannot =
    List.fold_left
      (fun (pset,rannot) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in
         let () = forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = forbid_modification p_pos p.Ast.port_int_mod in
         let () = match p.Ast.port_int with
           | [] -> ()
           | [ va ] ->
             internals.(p_id) <-
               Some (Signature.num_of_internal_state p_id va sign)
           | _ :: (_, pos) :: _ -> several_internal_states pos in
         match p.Ast.port_lnk with
         | ([Ast.LNK_ANY, _] | [Ast.LNK_SOME, _] |
            [Ast.LNK_TYPE _, _] | _::_::_) ->
           not_enough_specified agent_name p_na
         | [Ast.ANY_FREE, _] when syntax_version = Ast.V4 ->
           not_enough_specified agent_name p_na
         | [Ast.LNK_VALUE (i,()), pos] ->
           let () = ports.(p_id) <- Raw_mixture.VAL i in
           let _,rannot' =
             build_link sigs ?contact_map pos i ag_id p_id Freed rannot in
           pset',rannot'
         | [(Ast.ANY_FREE | Ast.LNK_FREE), _] | [] -> pset',rannot
      ) (Mods.IntSet.empty,rannot) intf in
  rannot,
  { Raw_mixture.a_type = ag_id;
    Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; }

let agent_with_counters agent_name ast_sigs =
  let (_,sites,_) =
    List.find
      (fun ((ag_na,_),_,_) -> (String.compare ag_na agent_name) =0) ast_sigs in
  sites,
  List.exists
    (fun site -> match site with Ast.Port _ -> false | Ast.Counter _ -> true)
    sites

let annotate_created_counters
      sigs ast_sigs ?contact_map (agent_name,_ as ag_ty) counts acc =

  let (sites,with_counters) = agent_with_counters agent_name ast_sigs in
  if not(with_counters) then acc
  else
    let ag_id = Signature.num_of_agent ag_ty sigs in
    let sign = Signature.get sigs ag_id in
    let arity = Signature.arity sigs ag_id in
    let ra_counters = Array.make arity (Ast.empty_counter, Maintained) in

    (* register all counters with min value *)
    let () =
      List.iter
        (fun site ->
          match site with
            Ast.Port _ -> ()
          | Ast.Counter c ->
             let p_na = c.Ast.count_nme in
             let p_id = Signature.num_of_site ~agent_name p_na sign in
             ra_counters.(p_id) <-
               {c with Ast.count_delta = (0,Locality.dummy)},Maintained) sites in

    let register_counter_modif c_id =
      let (incr_id,_,incr_b,_) = incr_agent sigs in
      add_link_contact_map ?contact_map ag_id c_id incr_id incr_b in
    let _ =
      List.fold_left
        (fun pset c ->
          let p_na = c.Ast.count_nme in
          let p_id = Signature.num_of_site ~agent_name p_na sign in
          let pset' = Mods.IntSet.add p_id pset in
          let () = if pset == pset' then
                     several_occurence_of_site agent_name c.Ast.count_nme in
          let () = register_counter_modif p_id in
          let () = ra_counters.(p_id) <- c,Maintained in
          pset') Mods.IntSet.empty counts in
    let ra =
      {ra_type = ag_id;ra_ports =[||];ra_ints =[||];ra_erased = false;
       ra_syntax = Some ([||],[||]);} in
    {ra; ra_counters;ra_created = true}::acc

let translate_modification sigs ?contact_map ag_id p_id
    ?warn (lhs_links,rhs_links as links_annot) = function
  | None -> Maintained,links_annot
  | Some x ->
    let () =
      match warn with
      | None -> ()
      | Some (na,pos) ->
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
    match x with
    | None ->  Freed,links_annot
    | Some (j,pos_j) ->
      let _,rhs_links' =
        build_link sigs ?contact_map pos_j j ag_id p_id Freed rhs_links in
      Linked (j,pos_j),(lhs_links,rhs_links')

let annotate_edit_agent
    ~syntax_version ~is_rule sigs ?contact_map (agent_name, _ as ag_ty) links_annot
    intf counts =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let scan_port (links_annot,pset) p =
    let (p_na,_) = p.Ast.port_nme in
    let p_id = Signature.num_of_site ~agent_name p.Ast.port_nme sign in
    let pset' = Mods.IntSet.add p_id pset in
    let () = if pset == pset' then
        several_occurence_of_site agent_name p.Ast.port_nme in
    let links_annot' =
      match p.Ast.port_lnk with
      | [Ast.LNK_SOME, pos as x] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (x, modif) in
        links_annot'
      | [(Ast.LNK_ANY, pos)] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- ((Ast.ANY_FREE,pos), modif) in
        links_annot'
      | ([] | [Ast.ANY_FREE, _]) when syntax_version = Ast.V4 ->
        let (modif,links_annot') = translate_modification
            ~warn:p.Ast.port_nme sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (Locality.dummy_annot Ast.ANY_FREE, modif) in
        links_annot'
      | [Ast.LNK_TYPE (dst_p,dst_ty), pos] ->
        let (modif,links_annot') = translate_modification
            ~warn:(p_na,pos) sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p modif in
        links_annot'
      | ([] | [(Ast.LNK_FREE | Ast.ANY_FREE), _]) ->
        let (modif,links_annot') = translate_modification
            ?warn:None sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let () = ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, modif) in
        links_annot'
      | [Ast.LNK_VALUE (i,()), pos] ->
        let (modif,(lhs_links,rhs_links)) = translate_modification
            ?warn:None sigs ?contact_map ag_id p_id
            links_annot p.Ast.port_lnk_mod in
        let va,lhs_links' =
          build_link
            sigs ?contact_map:(if is_rule then None else contact_map)
            pos i ag_id p_id modif lhs_links in
        let () = ports.(p_id) <- va in
        (lhs_links',rhs_links)
      | _::(_,pos)::_ ->
        raise (ExceptionDefn.Malformed_Decl
                 ("Several link state for a single site",pos)) in
    let () =
      match p.Ast.port_int,p.Ast.port_int_mod with
      | [], None -> ()
      | [ va ], Some va' ->
        internals.(p_id) <-
          I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
                         Signature.num_of_internal_state p_id va' sign)
      | [], Some (_,pos as va) ->
        let () =
          ExceptionDefn.warning
            ~pos
            (fun f ->
               Format.fprintf
                 f
                 "internal state of site '%s' of agent '%s' is modified \
                  although it is left unpecified in the left hand side"
                 p_na agent_name) in
        internals.(p_id) <-
          I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
      | [ va ], None ->
        let i_id = Signature.num_of_internal_state p_id va sign in
        internals.(p_id) <- I_VAL_CHANGED (i_id,i_id)
      | _ :: (_,pos) :: _, _ -> several_internal_states pos in
    (links_annot',pset') in
  let annot',_ =
    List.fold_left scan_port (links_annot,Mods.IntSet.empty) intf in

  let ra_counters = Array.make arity (Ast.empty_counter, Maintained) in
  let register_counter_modif c_id =
    let (incr_id,_,incr_b,_) = incr_agent sigs in
    add_link_contact_map ?contact_map ag_id c_id incr_id incr_b in
  let _ =
    List.fold_left
      (fun pset c ->
        let p_na = c.Ast.count_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let pset' = Mods.IntSet.add p_id pset in
        let () = if pset == pset' then
             several_occurence_of_site agent_name c.Ast.count_nme in
        let () = register_counter_modif p_id in
        let () = ra_counters.(p_id) <- c,Maintained in pset')
      Mods.IntSet.empty counts in
  let ra =
    { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  {ra; ra_counters; ra_created=false},annot'

let annotate_agent_with_diff
    sigs ?contact_map (agent_name, pos as ag_ty) links_annot lp rp lc rc =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let register_port_modif p_id lnk1 p' (lhs_links,rhs_links as links_annot) =
    let () = forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_lnk_mod in
    match lnk1,p'.Ast.port_lnk with
    | [Ast.LNK_ANY,pos], [Ast.LNK_ANY,_] ->
      let () = ports.(p_id) <- ((Ast.ANY_FREE,pos), Maintained) in
      links_annot
    | [Ast.LNK_SOME,pos], [Ast.LNK_SOME,_] ->
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos), Maintained) in
      links_annot
    | [Ast.LNK_TYPE ((dst_p'',_ as dst_p),(dst_ty'',_ as dst_ty)),pos],
      [Ast.LNK_TYPE ((dst_p',_),(dst_ty',_)),_]
      when dst_p'' = dst_p' && dst_ty'' = dst_ty' ->
      let () = ports.(p_id) <- build_l_type sigs pos dst_ty dst_p Maintained in
      links_annot
    | _, ([Ast.LNK_ANY,_] | [Ast.LNK_SOME,_] | [Ast.LNK_TYPE _,_]) ->
      not_enough_specified agent_name p'.Ast.port_nme
    | [Ast.LNK_ANY,pos], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let () = ports.(p_id) <- ((Ast.LNK_ANY,pos), Freed) in
      links_annot
    | [Ast.LNK_SOME,pos_lnk], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Freed) in
      links_annot
    | [Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- build_l_type sigs pos_lnk dst_ty dst_p Freed in
      links_annot
    | ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []), ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let () = ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, Maintained) in
      links_annot
    | [Ast.LNK_VALUE (i,()),pos], ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []) ->
      let va,lhs_links' =
        build_link sigs pos i ag_id p_id Freed lhs_links in
      let () = ports.(p_id) <- va in (lhs_links',rhs_links)
    | [Ast.LNK_ANY,pos_lnk], [Ast.LNK_VALUE (i,()),pos] ->
      let () = ports.(p_id) <- ((Ast.LNK_ANY,pos_lnk), Linked (i,pos)) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos i ag_id p_id Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_SOME,pos_lnk], [Ast.LNK_VALUE (i,()),pos'] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <- ((Ast.LNK_SOME,pos_lnk), Linked (i,pos')) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos' i ag_id p_id Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk], [Ast.LNK_VALUE (i,()),pos'] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f "breaking a semi-link on site '%s' will induce a side effect"
               na) in
      let () = ports.(p_id) <-
          build_l_type sigs pos_lnk dst_ty dst_p (Linked (i,pos')) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos' i ag_id p_id Freed rhs_links in
      lhs_links,rhs_links'
    | ([(Ast.LNK_FREE|Ast.ANY_FREE),_] | []), [Ast.LNK_VALUE (i,()),pos] ->
      let () =
        ports.(p_id) <- (Locality.dummy_annot Ast.LNK_FREE, Linked (i,pos)) in
      let _,rhs_links' =
        build_link sigs ?contact_map pos i ag_id p_id Freed rhs_links in
      lhs_links,rhs_links'
    | [Ast.LNK_VALUE (i,()),pos_i], [Ast.LNK_VALUE (j,()),pos_j] ->
      let va,lhs_links' = build_link
          sigs pos_i i ag_id p_id (Linked (j,pos_j)) lhs_links in
      let _,rhs_links' =
        build_link sigs ?contact_map pos_j j ag_id p_id Freed rhs_links in
      let () = ports.(p_id) <- va in (lhs_links',rhs_links')
    | _::(_,pos)::_, _ ->
      raise (ExceptionDefn.Malformed_Decl
               ("Several link state for a single site",pos))
    | _, _::(_,pos)::_ ->
      raise (ExceptionDefn.Malformed_Decl
               ("Several link state for a single site",pos)) in
  let register_internal_modif p_id int1 p' =
    let () = forbid_modification (snd p'.Ast.port_nme) p'.Ast.port_int_mod in
    match int1,p'.Ast.port_int with
    | [], [] -> ()
    | [ va ], [ va' ] ->
      internals.(p_id) <-
        I_VAL_CHANGED (Signature.num_of_internal_state p_id va sign,
                       Signature.num_of_internal_state p_id va' sign)
    | [], [ va ] ->
      let (na,pos) = p'.Ast.port_nme in
      let () =
        ExceptionDefn.warning
          ~pos
          (fun f ->
             Format.fprintf
               f
               "internal state of site '%s' of agent '%s' is modified \
although it is left unpecified in the left hand side"
               na agent_name) in
      internals.(p_id) <-
        I_ANY_CHANGED (Signature.num_of_internal_state p_id va sign)
    | [ _ ], [] ->
      let (na,pos) = p'.Ast.port_nme in
      raise (ExceptionDefn.Malformed_Decl
               ("The internal state of port '"^na^
                "' is underspecified on the right hand side", pos))
    | (_ :: (_,pos) :: _, _ | _, _ :: (_,pos) :: _) ->
      several_internal_states pos in
  let find_in_r (na,pos) rp =
    let (p',r) =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp in
    match p' with
    | [p'] -> (p',r)
    | [] -> not_enough_specified agent_name (na,pos)
    | _ :: _ -> several_occurence_of_site agent_name (na,pos) in
  let rp_r,annot,_ =
    List.fold_left
      (fun (rp,annot,pset) p ->
         let (_,p_pos as p_na) = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in
         let () = forbid_modification p_pos p.Ast.port_lnk_mod in
         let () = forbid_modification p_pos p.Ast.port_int_mod in

         let p',rp' = find_in_r p_na rp in
         let annot' = register_port_modif
             p_id p.Ast.port_lnk p' annot in
         let () = register_internal_modif p_id p.Ast.port_int p' in
         (rp',annot',pset')) (rp,links_annot,Mods.IntSet.empty) lp in
  let annot' =
    List.fold_left
      (fun annot p ->
         let p_na = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let () = register_internal_modif p_id [] p in
         register_port_modif p_id [Locality.dummy_annot Ast.LNK_ANY] p annot)
      annot rp_r in

  let register_counter_modif c c_id =
    let (incr_id,_,incr_b,_) = incr_agent sigs in
    let () = add_link_contact_map ?contact_map ag_id c_id incr_id incr_b in
    (c, Maintained) in
  let ra_counters = Array.make arity (Ast.empty_counter, Maintained) in
  let rc_r,_ =
    List.fold_left
      (fun (rc,cset) c ->
        let (na,_) as c_na = c.Ast.count_nme in
        let c_id = Signature.num_of_site ~agent_name c_na sign in
        let cset' = Mods.IntSet.add c_id cset in
        let () = if cset == cset' then
                   several_occurence_of_site agent_name c_na in
        let c',rc' =
          List.partition
            (fun p -> String.compare (fst p.Ast.count_nme) na = 0) rc in
        let c'' =
          match c' with
          | _::[] | [] -> register_counter_modif c c_id
          | _ :: _ -> several_occurence_of_site agent_name c_na in
        let () = ra_counters.(c_id) <- c'' in
        (rc',cset')) (rc,Mods.IntSet.empty) lc in
  let _ =
    if not(rc =[]) && not(rc_r =[]) then
      raise (ExceptionDefn.Internal_Error
               ("Counters in "^agent_name^" should have tests by now",pos)) in
  let ra =
    { ra_type = ag_id; ra_ports = ports; ra_ints = internals;ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  {ra; ra_counters; ra_created = false},annot'

let refer_links_annot links_annot mix =
  List.iter
    (fun ra ->
       Array.iteri
         (fun i -> function
            | (Ast.LNK_VALUE (j,(-1,-1)),pos),mods ->
              begin
                match Mods.IntMap.find_option j links_annot with
                | None -> ()
                | Some (dst_ty,dst_p,maintained) ->
                  ra.ra_ports.(i) <-
                    ((Ast.LNK_VALUE (j,(dst_p,dst_ty)),pos),
                     if maintained then Maintained else mods)
              end
            | ((Ast.LNK_VALUE _ | Ast.LNK_ANY | Ast.LNK_SOME
               | Ast.LNK_TYPE _ | Ast.LNK_FREE | Ast.ANY_FREE),_),_ -> ())
         ra.ra_ports) mix

let separate_sites ls =
  let (a,b) =
    List.fold_left
      (fun (ps,cs) -> function
        | Ast.Port p -> (p::ps,cs)
        | Ast.Counter c -> (ps,c::cs)) ([],[]) ls in
  (List.rev a,b)

(*
Is responsible for the check that:
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)
let annotate_lhs_with_diff sigs ast_sigs ?contact_map lhs rhs =
  let rec aux links_annot acc lhs rhs =
    match lhs,rhs with
    | ((lag_na,lpos as ag_ty),lag_s,lmod)::lt, ((rag_na,rpos),rag_s,rmod)::rt
      when String.compare lag_na rag_na = 0 &&
           Ast.no_more_site_on_right true lag_s rag_s ->
      let () = forbid_modification lpos lmod in
      let () = forbid_modification rpos rmod in
      let (lag_p,lag_c) = separate_sites lag_s in
      let (rag_p,rag_c) = separate_sites rag_s in
      let ra,links_annot' =
        annotate_agent_with_diff
          sigs ?contact_map ag_ty links_annot lag_p rag_p lag_c rag_c in
      aux links_annot' (ra::acc) lt rt
    | erased, added ->
      let () =
        if added <> [] then
          List.iter (fun ((lag,pos),lag_p,_) ->
              if List.exists
                  (fun ((rag,_),rag_p,_) ->
                     String.compare lag rag = 0 &&
                     Ast.no_more_site_on_right false lag_p rag_p) added then
                ExceptionDefn.warning ~pos
                  (fun f ->
                     Format.fprintf
                       f "Rule induced deletion AND creation of the agent %s" lag))
            erased in
      let syntax_version=Ast.V3 in
      let mix,(lhs_links_one,lhs_links_two) =
        List.fold_left
          (fun (acc,lannot) ((_,pos as na),sites,modif) ->
             let () = forbid_modification pos modif in
             let intf,counts = separate_sites sites in
             let ra,lannot' =
               annotate_dropped_agent ~syntax_version sigs lannot na intf counts in
             (ra::acc,lannot'))
          (acc,fst links_annot) erased in
      let () =
        match Mods.IntMap.root lhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
      let () = refer_links_annot lhs_links_two (List.map (fun r -> r.ra) mix) in
      let mix,cmix,(rhs_links_one,_) =
        List.fold_left
          (fun (acc',acc,rannot) ((_,pos as na),sites,modif) ->
             let () = forbid_modification pos modif in
             let intf,counts = separate_sites sites in
             let rannot',x' = annotate_created_agent
                 ~syntax_version sigs ?contact_map rannot na intf in
             let acc'' =
               annotate_created_counters sigs ast_sigs ?contact_map na counts acc' in
             acc'',x'::acc,rannot')
          (mix,[],snd links_annot) added in
      let () =
        match Mods.IntMap.root rhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
      List.rev mix, List.rev cmix in
  aux
    ((Mods.IntMap.empty,Mods.IntMap.empty),(Mods.IntMap.empty,Mods.IntMap.empty))
    [] lhs rhs

let annotate_edit_mixture ~syntax_version ~is_rule sigs ?contact_map m =
  let ((lhs_links_one,lhs_links_two),(rhs_links_one,_)),mix,cmix =
    List.fold_left
      (fun (lannot,acc,news) (ty,sites,modif) ->
         let (intf,counts) = separate_sites sites in
         match modif with
         | None ->
           let a,lannot' = annotate_edit_agent
               ~syntax_version ~is_rule sigs ?contact_map ty lannot intf counts in
           (lannot',a::acc,news)
         | Some Ast.Create ->
           let rannot',x' = annotate_created_agent
               ~syntax_version sigs ?contact_map (snd lannot) ty intf in
           ((fst lannot,rannot'),acc,x'::news)
         | Some Ast.Erase ->
           let ra,lannot' = annotate_dropped_agent
               ~syntax_version sigs (fst lannot) ty intf counts in
           ((lannot',snd lannot),ra::acc,news))
      (((Mods.IntMap.empty,Mods.IntMap.empty),
        (Mods.IntMap.empty,Mods.IntMap.empty)),[],[])
      m in
  let () =
    match Mods.IntMap.root lhs_links_one with
    | None -> ()
    | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
  let () = refer_links_annot lhs_links_two (List.map (fun r -> r.ra) mix) in
  let () =
    match Mods.IntMap.root rhs_links_one with
    | None -> ()
    | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
  (List.rev mix, List.rev cmix)

let give_rule_label bidirectional (id,set) printer r = function
  | None ->
    (succ id,set), Format.asprintf "r%i: %a" id printer r
  | Some (lab,pos) ->
    let set' = Mods.StringSet.add lab set in
    if set == set' then
      raise
        (ExceptionDefn.Malformed_Decl
           ("A rule named '"^lab^"' already exists.",pos))
    else if bidirectional then
      let set'' =
        Mods.StringSet.add (Ast.flip_label lab) set' in
      if set' == set'' then
        raise
          (ExceptionDefn.Malformed_Decl
             ("A rule named '"^Ast.flip_label lab^"' already exists.",pos))
      else (id,set''),lab
    else (id,set'),lab

let add_un_variable k_un acc rate_var =
  match k_un with
  | None -> (acc,None)
  | Some (k,dist) ->
    let acc_un,k' = if Alg_expr.has_mix (fst k) then
        ((Locality.dummy_annot rate_var,k)::acc,
         Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
      else (acc,k) in
    (acc_un,Some (k',dist))

let name_and_purify_edit_rule (label_opt,r) (pack,acc,rules) =
  let pack',label =
    give_rule_label false pack Ast.print_ast_edit_rule r label_opt in
  let acc',act =
    if Alg_expr.has_mix (fst r.Ast.act) then
      let rate_var = label^"_rate" in
      ((Locality.dummy_annot rate_var,r.Ast.act)::acc,
       Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
    else (acc,r.Ast.act) in
  let acc'',un_act = add_un_variable r.Ast.un_act acc' (label^"_un_rate") in
  (pack',acc'',
   (label_opt,
    {Ast.mix = r.Ast.mix; Ast.delta_token = r.Ast.delta_token;
     Ast.act; Ast.un_act})::rules)

let name_and_purify_rule (label_opt,(r,r_pos)) (pack,acc,rules) =
  let pack',label =
    give_rule_label r.Ast.bidirectional pack Ast.print_ast_rule r label_opt in
  let acc',k_def =
    if Alg_expr.has_mix (fst r.Ast.k_def) then
      let rate_var = label^"_rate" in
      ((Locality.dummy_annot rate_var,r.Ast.k_def)::acc,
       Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
    else (acc,r.Ast.k_def) in
  let acc'',k_un = add_un_variable r.Ast.k_un acc' (label^"_un_rate") in
  let acc''',rules' =
    match r.Ast.bidirectional,r.Ast.k_op with
    | true, Some k when Alg_expr.has_mix (fst k) ->
      let rate_var = (Ast.flip_label label)^"_rate" in
      let rate_var_un = (Ast.flip_label label)^"_un_rate" in
      let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
      ((Locality.dummy_annot rate_var,k)::acc_un,
       (Option_util.map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        Locality.dummy_annot (Alg_expr.ALG_VAR rate_var),k_op_un,r_pos)::rules)
    | true, Some rate ->
      let rate_var_un = (Ast.flip_label label)^"_un_rate" in
      let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
      (acc_un,
       (Option_util.map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        rate,k_op_un,r_pos)::rules)
    | false, None -> (acc'',rules)
    | (false, Some _ | true, None) ->
       raise
         (ExceptionDefn.Malformed_Decl
            ("Incompatible arrow and kinectic rate for inverse definition",
             r_pos)) in
  (pack',acc''',
   (label_opt,r.Ast.lhs,r.Ast.rhs,r.Ast.rm_token,r.Ast.add_token,
    k_def,k_un,r_pos)
   ::rules')

let make_counter_agent sigs
      (first,dst) (last,equal) i j pos created =
  let (ra_type,arity,incr_b,incr_a) = incr_agent sigs in
  let ra_ports = Array.make arity ((Ast.LNK_FREE,pos), Maintained) in
  let before_switch = if first&&created then Linked (i,pos) else Maintained in
  let before =
    if first then Ast.LNK_VALUE (i,dst), pos
    else Ast.LNK_VALUE (i,(ra_type,incr_a)), pos in
  let () = ra_ports.(incr_b) <- before,before_switch in
  let after =
    if (last&&equal) then Ast.LNK_FREE, pos
    else
      if last then Ast.LNK_ANY, pos
      else Ast.LNK_VALUE (j,(ra_type,incr_b)), pos in
  let () = ra_ports.(incr_a) <- (after,Maintained) in
  let ra_ints = Array.make arity I_ANY in
  {ra_type; ra_erased = false; ra_ports; ra_ints;
   ra_syntax = Some (Array.copy ra_ports,Array.copy ra_ints)}

let raw_counter_agent
      (first,first_lnk) (last,last_lnk) i j sigs equal =
  let (incr_type,arity,incr_b,incr_a) = incr_agent sigs in
  let ports = Array.make arity (Raw_mixture.FREE) in
  let internals =
    Array.init arity
               (fun i ->
                 Signature.default_internal_state incr_type i sigs) in
  let before = if first then Raw_mixture.VAL first_lnk
               else Raw_mixture.VAL i in
  let () = ports.(incr_b) <- before in
  let after =
    if (last&&equal) then Raw_mixture.FREE
    else
      if last then Raw_mixture.VAL last_lnk
      else Raw_mixture.VAL j in
  let () = ports.(incr_a) <- after in
  { Raw_mixture.a_type = incr_type;
    Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; }

let rec add_incr i first_lnk last_lnk delta equal sigs =
  if (i=delta) then []
  else
    let first = (i=0) in
    let last = (i=(delta-1)) in
    let raw_incr =
      raw_counter_agent
        (first,first_lnk) (last,last_lnk)
        (first_lnk+i) (first_lnk+i+1) sigs equal in
    raw_incr::(add_incr (i+1) first_lnk last_lnk delta equal sigs)

let rec link_incr sigs i nb ag_info equal lnk pos delta =
  if (i=nb) then []
  else
    let first = (i=0) in
    let last = (i=(nb-1)) in
    let ra_agent =
      make_counter_agent sigs (first,ag_info) (last,equal)
                         (lnk+i) (lnk+i+1) pos (delta>0) in
    ra_agent::(link_incr sigs (i+1) nb ag_info equal lnk pos delta)

let rec erase_incr sigs i incrs delta lnk =
  let (_,_,incr_b,_) = incr_agent sigs in
  match incrs with
  | hd::tl ->
     if (i = abs(delta)) then
       let (before,_) = hd.ra_ports.(incr_b) in
       let () = hd.ra_ports.(incr_b) <- before, Linked lnk in
       hd::tl
     else
       let ag = {hd with ra_erased = true} in
       ag::(erase_incr sigs (i+1) tl delta lnk)
  | [] -> []

let counter_becomes_port sigs ra p_id (delta,pos') pos equal test start_lnk_nb =
  let (incr_type,_,incr_b,_) = incr_agent sigs in
  let start_lnk_for_created = start_lnk_nb + test +1 in
  let lnk_for_erased = start_lnk_nb + abs(delta) in
  let ag_info = p_id,ra.ra_type in

  let test_incr = link_incr sigs 0 (test+1) ag_info equal start_lnk_nb pos delta in
  let adjust_delta =
    if (delta<0)
    then erase_incr sigs 0 test_incr delta (lnk_for_erased,pos)
    else test_incr in
  let created =
    if (delta>0)
    then add_incr 0 start_lnk_for_created start_lnk_nb delta false sigs
    else [] in

  let () = if (test + delta < 0) then
             raise (ExceptionDefn.Internal_Error
                      ("Counter test should be greater then abs(delta)",pos')) in
  let switch =
    if (delta = 0) then Maintained
    else
      if (delta > 0) then Linked (start_lnk_for_created,pos')
      else Linked (lnk_for_erased,pos') in

  let p = (Ast.LNK_VALUE (start_lnk_nb,(incr_b,incr_type)),pos),switch in
  let () = ra.ra_ports.(p_id) <- p in
  (adjust_delta,created)

(* ag - agent with counters in a rule
   lnk_nb - the max link number used in the rule;
   incr_info - info on the incr agent from the signature
   returns: agent with explicit counters; created incr agents;
            the next link number to use *)
let remove_counter_agent sigs ag lnk_nb =
  let (incrs,lnk_nb') =
    Tools.array_fold_lefti
    (fun id (acc_incrs,lnk_nb) (counter,_) ->
      let (s,pos) = counter.Ast.count_nme in
      if (s = "") then (acc_incrs,lnk_nb)
      else
        match (counter.Ast.count_test,counter.Ast.count_delta) with
        | (None,_) ->
           raise (ExceptionDefn.Internal_Error
                    ("Counter "^s^" should have a test by now",pos))
        | (Some (test,pos')), delta ->
           match test with
           | Ast.CEQ j ->
              (counter_becomes_port
                 sigs ag.ra id delta pos true j lnk_nb)::acc_incrs,
              lnk_nb+1+j+(fst delta)
           | Ast.CGTE j ->
              (counter_becomes_port
                 sigs ag.ra id delta pos false j lnk_nb)::acc_incrs,
              lnk_nb+1+j+(fst delta)
           | Ast.CVAR _ ->
              raise (ExceptionDefn.Internal_Error
                       ("Counter "^s^" should not have a var by now",pos')))
    ([],lnk_nb) ag.ra_counters in
  let (als,bls) =
    List.fold_left (fun (als,bls) (a,b) -> a@als,b@bls) ([],[]) incrs in
  (als,bls,lnk_nb')

let remove_counter_created_agent sigs raw ag lnk_nb =
  let raw_agent =
    List.find (fun rag -> rag.Raw_mixture.a_type = ag.ra.ra_type) raw in
  let ports = raw_agent.Raw_mixture.a_ports in
  let () = Signature.print_agent sigs (Format.str_formatter) ag.ra.ra_type in
  let agent_name = Format.flush_str_formatter () in
  Tools.array_fold_lefti
    (fun p_id (acc,lnk) (c,_) ->
      let (s,_) = c.Ast.count_nme in
      if (s = "") then (acc,lnk)
      else
      match c.Ast.count_test with
      | None -> not_enough_specified agent_name c.Ast.count_nme
      | Some (test,_) ->
         match test with
         | Ast.CEQ j ->
            let p = Raw_mixture.VAL lnk in
            let () = ports.(p_id) <- p in
            let incrs = add_incr 0 lnk (lnk+j) (j+1) true sigs in
            (acc@incrs,(lnk+j))
         | Ast.CGTE _ | Ast.CVAR _ ->
            not_enough_specified agent_name c.Ast.count_nme)
    ([],lnk_nb) ag.ra_counters

(* - adds increment agents to the contact map
   - adds increment agents to the raw mixture
   - links the agents in the mixture(lhs,rhs,mix) or in the raw mixture(created)
     to the increments *)
let remove_counter_rule sigs ?contact_map with_counters mix created =
  if (with_counters) then
    let (incr_id,_,incr_b,incr_a) = incr_agent sigs in
    let () = add_link_contact_map ?contact_map incr_id incr_a incr_id incr_b in
    let lnk_nb =
      List.fold_left
        (fun max ag ->
          Array.fold_left
            (fun max ((lnk,_),switch) ->
              let max' =
                match lnk with
                  Ast.LNK_VALUE (i,_) -> if (max<i) then i else max
                | Ast.ANY_FREE | Ast.LNK_FREE | Ast.LNK_ANY | Ast.LNK_SOME
                  | Ast.LNK_TYPE _ -> max in
              match switch with
              | Linked (i,_) ->  if (max'<i) then i else max'
              | Freed | Maintained | Erased -> max')
            max ag.ra.ra_ports) 0 mix in
    let mix_created,mix' = List.partition (fun ag -> ag.ra_created) mix in
    let (incrs,incrs_created,ra_mix,lnk_nb') =
      List.fold_left
        (fun (a,b,c,lnk) ag ->
            let (a',b',lnk') = remove_counter_agent sigs ag lnk in
            a'@a,b'@b,ag.ra::c,lnk')
        ([],[],[],lnk_nb+1) mix' in
    let incrs_created',_ =
      List.fold_left
        (fun (acc,lnk) ag ->
          let (a,lnk') =
            remove_counter_created_agent sigs created ag lnk in
          (a::acc,lnk'))
        ([],lnk_nb') mix_created in
    (ra_mix@incrs,created@incrs_created@(List.flatten incrs_created'))
  else List.map (fun ag -> ag.ra) mix,created

let remove_counters sigs ?contact_map with_counters rules  =
  List.map
    (fun (s,(r,a)) ->
      let (r_mix,r_created) =
        remove_counter_rule sigs ?contact_map with_counters
                            r.r_mix r.r_created in
      let r' = {r with r_mix;r_created} in
      (s,(r',a))) rules

let mixture_of_ast ~new_syntax sigs ?contact_map ?c pos mix =
  match annotate_edit_mixture ~new_syntax ~is_rule:false sigs ?contact_map mix with
  | r, [] ->
     (match c with
        None -> List.map (fun ag-> ag.ra) r
      | Some with_counters ->
         let mix',_ = remove_counter_rule sigs ?contact_map with_counters r [] in
         mix')
  | _, _ -> raise (ExceptionDefn.Internal_Error
                     ("A mixture cannot create agents",pos))

let convert_alg_var ?max_allowed_var algs lab pos =
  let i =
    match Mods.StringMap.find_option lab algs with
    | Some x -> x
    | None ->
      raise (ExceptionDefn.Malformed_Decl
               (lab ^" is not a declared variable",pos)) in
  let () =
    match max_allowed_var with
    | Some j when j < i ->
      raise (ExceptionDefn.Malformed_Decl
               ("Reference to not yet defined '"^lab ^"' is forbidden.",
                pos))
    | None | Some _ -> ()
  in
  i

let convert_token_name tk_nme tok pos =
  match Mods.StringMap.find_option tk_nme tok with
  | Some x -> x
  | None ->
    raise (ExceptionDefn.Malformed_Decl
             (tk_nme ^ " is not a declared token",pos))

let rec alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var ?c (alg,pos) =
  ((match alg with
      | Alg_expr.KAPPA_INSTANCE ast ->
        Alg_expr.KAPPA_INSTANCE (mixture_of_ast ~new_syntax sigs ?c pos ast)
      | Alg_expr.ALG_VAR lab ->
        Alg_expr.ALG_VAR (convert_alg_var ?max_allowed_var algs lab pos)
      | Alg_expr.TOKEN_ID tk_nme ->
        Alg_expr.TOKEN_ID (convert_token_name tk_nme tok pos)
      | Alg_expr.DIFF_KAPPA_INSTANCE(expr,ast) ->
        Alg_expr.DIFF_KAPPA_INSTANCE
          (alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var expr ?c,
           mixture_of_ast ~new_syntax sigs pos ast ?c)
      | Alg_expr.DIFF_TOKEN(expr,tk_nme) ->
        Alg_expr.DIFF_TOKEN
          (alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var expr ?c,
           convert_token_name tk_nme tok pos)
      | (Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _) as x -> x
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        Alg_expr.BIN_ALG_OP
          (op,
           alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var a ?c,
           alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var b ?c)
      | Alg_expr.UN_ALG_OP (op,a) ->
        Alg_expr.UN_ALG_OP
          (op,alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var a ?c)
      | Alg_expr.IF (cond,yes,no) ->
        Alg_expr.IF
          (bool_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var cond,
           alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var yes ?c,
           alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var no ?c)
    ),
   pos)
and bool_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var ?c = function
  | (Alg_expr.TRUE | Alg_expr.FALSE),_ as x -> x
  | Alg_expr.BOOL_OP (op,x,y),pos ->
    Alg_expr.BOOL_OP
      (op, bool_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var x,
       bool_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var y),
    pos
  | Alg_expr.COMPARE_OP (op,x,y),pos ->
    Alg_expr.COMPARE_OP
      (op,alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var  x ?c,
       alg_expr_of_ast ~new_syntax sigs tok algs ?max_allowed_var y ?c),pos

let print_expr_of_ast ~new_syntax sigs tok algs = function
  | Primitives.Str_pexpr _ as x -> x
  | Primitives.Alg_pexpr x ->
    Primitives.Alg_pexpr (alg_expr_of_ast ~new_syntax sigs tok algs x)

let modif_expr_of_ast ~new_syntax sigs tok algs contact_map ?c modif acc =
  match modif with
  | Ast.INTRO (how,(who,pos)) ->
    Ast.INTRO
      (alg_expr_of_ast ~new_syntax sigs tok algs how ?c,
       (mixture_of_ast ~new_syntax sigs ~contact_map pos who ?c,pos)),
    acc
  | Ast.DELETE (how,(who,pos)) ->
    Ast.DELETE
      (alg_expr_of_ast ~new_syntax sigs tok algs how ?c,
       (mixture_of_ast ~new_syntax sigs pos who ?c,pos)),
    acc
  | Ast.UPDATE ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab algs with
      | Some i -> i
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 ("Variable " ^ (lab ^ " is not defined"),pos)) in
    Ast.UPDATE ((i,pos),alg_expr_of_ast ~new_syntax sigs tok algs how ?c),
    i::acc
  | Ast.UPDATE_TOK ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab tok with
      | Some x -> x
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 (lab ^" is not a declared token",pos)) in
    Ast.UPDATE_TOK ((i,pos), alg_expr_of_ast ~new_syntax sigs tok algs how ?c),
    acc
  | Ast.STOP p ->
    Ast.STOP (List.map (print_expr_of_ast ~new_syntax sigs tok algs) p),acc
  | Ast.SNAPSHOT p ->
    Ast.SNAPSHOT (List.map (print_expr_of_ast ~new_syntax sigs tok algs) p),acc
  | Ast.FLUX (rel,p) ->
    Ast.FLUX (rel,List.map (print_expr_of_ast ~new_syntax sigs tok algs) p),acc
  | Ast.FLUXOFF p ->
    Ast.FLUXOFF (List.map (print_expr_of_ast ~new_syntax sigs tok algs) p),acc
  | (Ast.PLOTENTRY | Ast.CFLOWLABEL (_,_ ) as x) -> x,acc
  | Ast.PRINT (p,p') ->
    Ast.PRINT
      (List.map (print_expr_of_ast ~new_syntax sigs tok algs) p,
       List.map (print_expr_of_ast ~new_syntax sigs tok algs) p'),acc
  | Ast.CFLOWMIX (b,(m,pos)) ->
    Ast.CFLOWMIX (b,(mixture_of_ast ~new_syntax sigs pos m,pos)),acc
  | Ast.SPECIES_OF (b,p,(m,pos)) ->
    Ast.SPECIES_OF
      (b,List.map (print_expr_of_ast ~new_syntax sigs tok algs) p,
       (mixture_of_ast ~new_syntax sigs pos m,pos)),acc

let perturbation_of_ast
    ~new_syntax sigs tok algs contact_map c ((pre,mods,post),pos) up_vars =
  let mods',up_vars' =
    List_util.fold_right_map
      (modif_expr_of_ast ~new_syntax sigs tok algs contact_map ~c) mods up_vars in
  ((bool_expr_of_ast ~new_syntax sigs tok algs pre,mods',
    match post with
    | None -> None
    | Some post -> Some (bool_expr_of_ast ~new_syntax sigs tok algs post)),pos),
  up_vars'

let agent_with_max_counter sigs c ((agent_name,_) as ag_ty) =
  let (incr_type,_,incr_b,_) = incr_agent sigs in
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Locality.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let c_na = c.Ast.count_nme in
  let c_id = Signature.num_of_site ~agent_name c_na sign in
  let (max_val,pos) = c.Ast.count_delta in
  let incrs = link_incr sigs 0 (max_val+1) (ag_id,c_id) false 1 pos (-1) in
  let p = Ast.LNK_VALUE (1,(incr_b,incr_type)),pos in
  let () = ports.(c_id) <- p,Maintained in
  let ra =
    { ra_type = ag_id;ra_ports = ports;ra_ints = internals;ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);} in
  ra::incrs

let counter_perturbation sigs c ag_ty =
  let filename =
    [Primitives.Str_pexpr ("counter_perturbation", snd c.Ast.count_nme) ] in
  let stop_message =
    "\nCounter "^(fst c.Ast.count_nme)^" of agent "^(fst ag_ty)^" reached maximum\n" in
  let stop_message' =
    [Primitives.Str_pexpr (stop_message, snd c.Ast.count_nme) ] in
  let mods = [Ast.PRINT ([],stop_message'); Ast.STOP filename] in
  let val_of_counter =
    Alg_expr.KAPPA_INSTANCE (agent_with_max_counter sigs c ag_ty) in
  let pre =
    Alg_expr.COMPARE_OP
      (Operator.EQUAL,(val_of_counter,snd c.Ast.count_nme),
       (Alg_expr.CONST (Nbr.I 1),snd c.Ast.count_nme)) in
  (pre,snd ag_ty),mods,None

let counters_perturbations sigs ast_sigs =
  List.fold_left
    (fun acc (ag_ty,sites,_)->
      List.fold_left
        (fun acc' site ->
          match site with
            Ast.Port _ -> acc'
          | Ast.Counter c ->
             ((counter_perturbation sigs c ag_ty),(snd ag_ty))::acc') acc sites)
    [] ast_sigs

let init_of_ast ~syntax_version sigs tok contact_map ?c = function
  | Ast.INIT_MIX who,pos ->
    Ast.INIT_MIX (mixture_of_ast ~syntax_version sigs ~contact_map pos who ?c),pos
  | Ast.INIT_TOK lab,pos ->
    match Mods.StringMap.find_option lab tok with
    | Some x -> Ast.INIT_TOK x,pos
    | None ->
      raise (ExceptionDefn.Malformed_Decl
               (lab ^" is not a declared token",pos))

let assemble_rule ~syntax_version ~r_editStyle
    sigs tk_nd algs r_mix r_created rm_tk add_tk rate un_rate =
  let tok = tk_nd.NamedDecls.finder in
  let tks =
    List.rev_map (fun (al,tk) ->
        (alg_expr_of_ast ~syntax_version sigs tok algs
           (Locality.dummy_annot (Alg_expr.UN_ALG_OP (Operator.UMINUS,al))),
         NamedDecls.elt_id ~kind:"token" tk_nd tk))
      rm_tk in
  let tks' =
    List_util.rev_map_append (fun (al,tk) ->
          (alg_expr_of_ast ~syntax_version sigs tok algs al,
           NamedDecls.elt_id ~kind:"token" tk_nd tk))
      add_tk tks in
  { r_mix; r_created; r_editStyle;
    r_delta_tokens = List.rev tks';
    r_rate = alg_expr_of_ast ~syntax_version sigs tok algs rate;
    r_un_rate =
      let r_dist d =
        alg_expr_of_ast ~syntax_version sigs tok algs ?max_allowed_var:None d in
      Option_util.map
        (fun (un_rate',dist) ->
           let un_rate'' =
             alg_expr_of_ast ~syntax_version sigs tok algs
               ?max_allowed_var:None un_rate' in
           match dist with
           | Some d -> (un_rate'', Some (r_dist d))
           | None -> (un_rate'', None))
        un_rate;
  }

let create_t ast_intf =
  NamedDecls.create (
    Tools.array_map_of_list
      (fun p ->
         (p.Ast.port_nme,
          (NamedDecls.create
             (Tools.array_map_of_list (fun x -> (x,())) p.Ast.port_int),
           List.fold_left (fun acc -> function
               | (Ast.LNK_FREE | Ast.ANY_FREE | Ast.LNK_ANY), _ -> acc
               | (Ast.LNK_SOME | Ast.LNK_VALUE _), pos ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Forbidden link status inside a definition of signature",
                           pos))
               | Ast.LNK_TYPE (a,b), _ -> (a,b) :: acc) [] p.Ast.port_lnk))
      ) (ast_intf))

let counters_to_ports counters =
  List.map
    (fun c ->
      {Ast.port_nme = c.Ast.count_nme; port_int = [];port_int_mod =None;
       port_lnk=[];port_lnk_mod = None}) counters

let create_sig_for_counters l with_counters =
  let l'=
    List.map
      (fun (name,intf,r) ->
        let ports,counters = separate_sites intf in
        let ports' = counters_to_ports counters in
        name,ports@ports',r)  l in
  if (with_counters) then
    let annot = Locality.dummy in
    let after =
      {Ast.port_nme=("a",annot);Ast.port_int=[];Ast.port_int_mod =None;
       Ast.port_lnk=[];Ast.port_lnk_mod=None} in
    let before =
      {Ast.port_nme=("b",Locality.dummy);Ast.port_int=[];Ast.port_int_mod =None;
       Ast.port_lnk=[];Ast.port_lnk_mod=None} in
    let counter_agent =
      (("__incr",Locality.dummy),[after;before],None) in
    counter_agent::l'
  else l'

let create_sig l with_counters =
  Tools.array_map_of_list
    (fun (name,intf,_) -> (name,create_t intf))
    (create_sig_for_counters l with_counters)

let compil_of_ast ~syntax_version overwrite c =
  let (c,with_counters) = Ast.compile_counters c in
  let c =
    if c.Ast.signatures = [] && c.Ast.tokens = []
    then
      if (with_counters) then
        raise (ExceptionDefn.Malformed_Decl
                 ("implicit signature is incompatible with counters",
                  Locality.dummy))
      else Ast.implicit_signature c
    else c in
  let sigs = Signature.create (create_sig c.Ast.signatures with_counters) in
  let contact_map =
    Array.init
      (Signature.size sigs)
      (fun i -> Array.init (Signature.arity sigs i)
          (fun s -> (Tools.recti
                       (fun a k -> k::a) []
                       (Signature.internal_states_number i s sigs),[]))) in
  let (name_pack,var_pack,cleaned_rules) =
    List.fold_right
      name_and_purify_rule c.Ast.rules ((0,Mods.StringSet.empty),[],[]) in
  let ((_,rule_names),extra_vars,cleaned_edit_rules) =
    List.fold_right
      name_and_purify_edit_rule c.Ast.edit_rules (name_pack,var_pack,[]) in
  let alg_vars_over =
    List_util.rev_map_append
      (fun (x,v) -> (Locality.dummy_annot x,
                     Alg_expr.const v)) overwrite
      (List.filter
         (fun ((x,_),_) ->
            List.for_all (fun (x',_) -> x <> x') overwrite)
         (c.Ast.variables@extra_vars)) in
  let algs =
    (NamedDecls.create
       ~forbidden:rule_names (Array.of_list alg_vars_over)).NamedDecls.finder in
  let tk_nd = NamedDecls.create
      (Tools.array_map_of_list (fun x -> (x,())) c.Ast.tokens) in
  let tok = tk_nd.NamedDecls.finder in
  let perts',updated_vars =
    List_util.fold_right_map
      (perturbation_of_ast ~syntax_version sigs tok algs contact_map with_counters)
      c.Ast.perturbations [] in
  let perts'' =
    if (with_counters) then (counters_perturbations sigs c.Ast.signatures)@perts'
    else perts' in
  let old_style_rules =
    List.map (fun (label,lhs,rhs,rm_tk,add_tk,rate,un_rate,r_pos) ->
        let mix,created =
          annotate_lhs_with_diff sigs c.Ast.signatures ~contact_map lhs rhs in
        label,
        (assemble_rule
           ~syntax_version ~r_editStyle:false
           sigs tk_nd algs mix created rm_tk add_tk rate un_rate,
         r_pos))
      cleaned_rules in
  let edit_rules =
    List.rev_map (fun (label,r) ->
        let mix,cmix = annotate_edit_mixture
            ~syntax_version:Ast.V4 ~is_rule:true sigs ~contact_map r.Ast.mix in
        (label,
         Locality.dummy_annot
           (assemble_rule
              ~syntax_version:Ast.V4 ~r_editStyle:true
              sigs tk_nd algs mix cmix [] r.Ast.delta_token
              r.Ast.act r.Ast.un_act)))
      cleaned_edit_rules in
  let rules = List.rev_append edit_rules old_style_rules in
  let rules = remove_counters sigs ~contact_map with_counters rules in
  sigs,contact_map,tk_nd,algs,updated_vars,
  {
    Ast.variables =
      List_util.mapi
        (fun i (lab,expr) ->
           (lab,alg_expr_of_ast
              ~syntax_version ~max_allowed_var:(pred i)
              sigs tok algs ~c:with_counters expr))
        alg_vars_over;
    Ast.rules ;
    Ast.edit_rules = [];
    Ast.observables =
      List.map (fun expr ->
          alg_expr_of_ast ~syntax_version sigs tok algs ~c:with_counters expr)
        c.Ast.observables;
    Ast.init =
      List.map (fun (lab,expr,ini) ->
          lab,alg_expr_of_ast ~syntax_version sigs tok algs ~c:with_counters expr,
          init_of_ast ~syntax_version sigs tok contact_map ~c:with_counters ini)
        c.Ast.init;
    Ast.perturbations = perts'';
    Ast.volumes = c.Ast.volumes;
    Ast.tokens = c.Ast.tokens;
    Ast.signatures = c.Ast.signatures;
    Ast.configurations = c.Ast.configurations;
  }

let init_of_ast ~syntax_version sigs contact_map ?c tok algs inits =
  List.map (fun (lab,expr,ini) ->
      lab,alg_expr_of_ast ~syntax_version sigs tok algs expr,
      init_of_ast ~syntax_version sigs tok contact_map ?c ini)
    inits
