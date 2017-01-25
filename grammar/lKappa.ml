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
  { ra_type: int;
    ra_erased: bool;
    ra_ports: ((int,int*int) Ast.link Locality.annot * switching) array;
    ra_ints: rule_internal array;
    ra_syntax: (((int,int*int) Ast.link Locality.annot * switching) array *
                rule_internal array) option;
  }

type rule_mixture = rule_agent list

type rule =
  { r_mix: rule_mixture;
    r_created: Raw_mixture.t;
    r_rm_tokens :
      ((rule_mixture,int) Alg_expr.e Locality.annot * int) list;
    r_add_tokens :
      ((rule_mixture,int) Alg_expr.e Locality.annot * int) list;
    r_rate : (rule_mixture,int) Alg_expr.e Locality.annot;
    r_un_rate : ((rule_mixture,int) Alg_expr.e Locality.annot
                 * (rule_mixture,int) Alg_expr.e Locality.annot option) option;
  }

let print_link_annot ~ltypes sigs f (s,a) =
  if ltypes then
    Format.fprintf f "(*%a.%a*)"
      (Signature.print_site sigs a) s
      (Signature.print_agent sigs) a

let print_rule_internal sigs ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j ->
    Format.fprintf f "~>>%a" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED -> Format.fprintf f "~--"
  | I_VAL_CHANGED (i,j) ->
    if i <> j then
      Format.fprintf
        f "~%a>>%a" (Signature.print_internal_state sigs ag_ty site) i
        (Signature.print_internal_state sigs ag_ty site) j
    else
      Format.fprintf f "~%a" (Signature.print_internal_state sigs ag_ty site) i
  | I_VAL_ERASED i ->
    Format.fprintf
      f "~%a--" (Signature.print_internal_state sigs ag_ty site) i

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

let print_switching f = function
  | Linked (i,_) -> Format.fprintf f ">>%i" i
  | Freed -> Format.fprintf f ">>%t" Pp.bottom
  | Maintained -> ()
  | Erased -> Format.fprintf f "--"

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

let print_rule_link sigs f ((e,_),s) =
  Format.fprintf
    f "%a%a"
    (Ast.print_link
       (Signature.print_site sigs)
       (Signature.print_agent sigs)
       (print_link_annot ~ltypes:true sigs))
    e
    print_switching s

let print_rule_intf sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
         | (Ast.LNK_ANY, _), Maintained ->  ints.(i) <> I_ANY
         | ((Ast.LNK_ANY, _), (Erased | Freed | Linked _) |
            ((Ast.LNK_SOME | Ast.FREE |
              Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true) then
        let () = Format.fprintf
            f "%t%a%a%a" (if empty then Pp.empty else Pp.comma)
            (Signature.print_site sigs ag_ty) i
            (print_rule_internal sigs ag_ty i)
            ints.(i) (print_rule_link sigs) ports.(i) in
        aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let print_rule_agent sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])" (Signature.print_agent sigs) ag.ra_type
    (print_rule_intf sigs ag.ra_type) (ag.ra_ports,ag.ra_ints)

let print_rule_mixture sigs f mix =
  Pp.list Pp.comma (print_rule_agent sigs) f mix

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
    (Signature.print_site sigs)
    (Signature.print_agent sigs)
    (print_link_annot ~ltypes sigs)
    f e

let print_link_rhs ~ltypes sigs f ((e,_),s) =
  match s with
  | Linked (i,_) ->
    Ast.print_link
      (Signature.print_site sigs) (Signature.print_agent sigs) (fun _ () -> ())
      f (Ast.LNK_VALUE (i,()))
  | Freed -> ()
  | Maintained ->
    Ast.print_link
      (Signature.print_site sigs) (Signature.print_agent sigs)
      (print_link_annot ~ltypes sigs)
      f e
  | Erased -> assert false

let print_intf_lhs ~ltypes sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
          | (((Ast.LNK_SOME | Ast.FREE |
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
          | (((Ast.LNK_SOME | Ast.FREE |
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
        (Raw_mixture.print ~compact:true ~sigs) created
    | h :: t ->
      if h.ra_erased then aux empty t
      else
        let () = Format.fprintf f "%t%a"
            (if empty then Pp.empty else Pp.comma)
            (print_agent_rhs ~ltypes sigs) h in
        aux false t in
  aux true mix

let print_rates sigs pr_tok pr_var f r =
  Format.fprintf
    f " @@ %a%t"
    (Alg_expr.print
       (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs) m)
       pr_tok pr_var) (fst r.r_rate)
    (fun f ->
       match r.r_un_rate with
       | None -> ()
       | Some ((ra,_),max_dist) ->
         Format.fprintf
           f "(%a%a)"
           (Alg_expr.print
              (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs) m)
              pr_tok pr_var) ra
           (Pp.option
              (fun f (md,_) ->
                Format.fprintf f ":%a"
                (Alg_expr.print
                   (fun f m -> Format.fprintf f "|%a|"
                                              (print_rule_mixture sigs) m)
                   pr_tok pr_var) md)) max_dist)

let print_rule ~ltypes ~rates sigs pr_tok pr_var f r =
  Format.fprintf
    f "@[<h>%a%t%a -> %a%t%a%t@]"
    (Pp.list Pp.comma (print_agent_lhs ~ltypes sigs)) r.r_mix
    (fun f -> match r.r_rm_tokens with [] -> ()
                                     | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (fun f ((nb,_),tk) ->
          Format.fprintf
            f "%a:%a"
            (Alg_expr.print
               (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs) m)
               pr_tok pr_var) nb
            pr_tok tk))
    r.r_rm_tokens
    (print_rhs ~ltypes sigs r.r_created) r.r_mix
    (fun f ->
       match r.r_add_tokens with [] -> ()
                               | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (fun f ((nb,_),tk) ->
          Format.fprintf
            f "%a:%a"
            (Alg_expr.print
               (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs) m)
               pr_tok pr_var) nb
            pr_tok tk))
    r.r_add_tokens
    (fun f -> if rates then print_rates sigs pr_tok pr_var f r)

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
          ra_syntax = Some (ports,ints)
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent",x))

let rule_mixture_to_json = JsonUtil.of_list rule_agent_to_json
let rule_mixture_of_json = JsonUtil.to_list rule_agent_of_json

let lalg_expr_to_json = Alg_expr.e_to_yojson rule_mixture_to_json JsonUtil.of_int
let lalg_expr_of_json =
  Alg_expr.e_of_yojson rule_mixture_of_json (JsonUtil.to_int ?error_msg:None)

let rule_to_json r =
  `Assoc
    [
      "mixture", rule_mixture_to_json r.r_mix;
      "created", Raw_mixture.to_json r.r_created;
      "rm_tokens",
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
           (Locality.annot_to_json lalg_expr_to_json)
           JsonUtil.of_int)
        r.r_rm_tokens;
      "add_tokens",
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
           (Locality.annot_to_json lalg_expr_to_json)
           JsonUtil.of_int)
        r.r_add_tokens;
      "rate", Locality.annot_to_json lalg_expr_to_json r.r_rate;
      "unary_rate",
      JsonUtil.of_option
        (JsonUtil.of_pair
           (Locality.annot_to_json lalg_expr_to_json)
           (JsonUtil.of_option (Locality.annot_to_json lalg_expr_to_json)))
        r.r_un_rate;
    ]
let rule_of_json = function
  | `Assoc l as x when List.length l < 7 ->
    begin
      try
        {
          r_mix = rule_mixture_of_json (List.assoc "mixture" l);
          r_created = Raw_mixture.of_json (List.assoc "created" l);
          r_rm_tokens =
            JsonUtil.to_list
              (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                 (Locality.annot_of_json lalg_expr_of_json)
                 (JsonUtil.to_int ?error_msg:None))
              (List.assoc "rm_tokens" l);
          r_add_tokens =
            JsonUtil.to_list
              (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                 (Locality.annot_of_json lalg_expr_of_json)
                 (JsonUtil.to_int ?error_msg:None))
              (List.assoc "rm_tokens" l);
          r_rate = Locality.annot_of_json lalg_expr_of_json (List.assoc "rate" l);
          r_un_rate =
            (try
               JsonUtil.to_option
                 (JsonUtil.to_pair
                    (Locality.annot_of_json lalg_expr_of_json)
                    (JsonUtil.to_option (Locality.annot_of_json
                                           lalg_expr_of_json)))
                 (List.assoc "unary_rate" l)
             with Not_found -> None);
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
    let si,sl = contact_map.(sty).(sp) in
    let di,dl = contact_map.(dty).(dp) in
    let () = contact_map.(sty).(sp) <-
        si,List_util.merge_uniq Mods.int_pair_compare sl [dty,dp] in
    contact_map.(dty).(dp) <-
      di,List_util.merge_uniq Mods.int_pair_compare dl [sty,sp]

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
  { ra_type = a.ra_type; ra_erased = a.ra_erased; ra_ports = p; ra_ints = i;
    ra_syntax =
      Tools.option_map (fun _ -> Array.copy p, Array.copy i) a.ra_syntax; }

let to_erased sigs x =
  List.map
    (fun r ->
       let ports = Array.map (fun (a,_) -> a,Erased) r.ra_ports in
       let ints =
         Array.mapi (fun j -> function
             | I_VAL_CHANGED (i,_) | I_VAL_ERASED i -> I_VAL_ERASED i
             | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED ->
               match Signature.default_internal_state r.ra_type j sigs with
               | Some _ -> I_ANY_ERASED
               | None -> I_ANY) r.ra_ints in
       { ra_type = r.ra_type; ra_erased = true; ra_ports = ports; ra_ints =ints;
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
              | ((Ast.LNK_ANY, _) | (Ast.FREE, _)),_ -> Raw_mixture.FREE
           )
           r.ra_ports in
       { Raw_mixture.a_type = r.ra_type;
         Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; })
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
                 (Locality.dummy_annot (Ast.LNK_VALUE (i,(-1,-1))), Maintained)
             | Raw_mixture.FREE ->
               (Locality.dummy_annot Ast.FREE, Maintained)
           )
           r.Raw_mixture.a_ports in
       { ra_type = r.Raw_mixture.a_type; ra_erased = false;
         ra_ports = ports; ra_ints = internals;
       ra_syntax = Some (Array.copy ports, Array.copy internals); })
    x

let annotate_dropped_agent sigs links_annot ((agent_name, _ as ag_ty),intf) =
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
         let p_na = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in

         let () = match p.Ast.port_int with
           | [] -> ()
           | [ va ] ->
             internals.(p_id) <-
               I_VAL_ERASED (Signature.num_of_internal_state p_id va sign)
           | _ :: (_, pos) :: _ -> several_internal_states pos in
         match p.Ast.port_lnk with
         | [Ast.LNK_ANY, pos] ->
           let () =
             ports.(p_id) <- ((Ast.LNK_ANY,pos), Erased) in (lannot,pset')
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
         | [Ast.FREE,_] | [] ->
           let () = ports.(p_id) <- Locality.dummy_annot Ast.FREE, Erased in
           (lannot,pset')
         | [Ast.LNK_VALUE (i,()), pos] ->
           let va,lannot' =
             build_link sigs pos i ag_id p_id Erased lannot in
           let () = ports.(p_id) <- va in (lannot',pset')
         | _::(_,pos)::_ ->
           raise (ExceptionDefn.Malformed_Decl
                    ("Several link state for a single site",pos)))
      (links_annot,Mods.IntSet.empty) intf in
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = true;
    ra_syntax = Some (Array.copy ports, Array.copy internals);},lannot

let annotate_created_agent
    sigs ?contact_map rannot ((agent_name, pos as ag_ty),intf) =
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
         let p_na = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in
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
         | [Ast.LNK_VALUE (i,()), pos] ->
           let () = ports.(p_id) <- Raw_mixture.VAL i in
           let _,rannot' =
             build_link sigs ?contact_map pos i ag_id p_id Freed rannot in
           pset',rannot'
         | [Ast.FREE, _] | [] -> pset',rannot
      ) (Mods.IntSet.empty,rannot) intf in
  rannot,
  ({ Raw_mixture.a_type = ag_id;
     Raw_mixture.a_ports = ports; Raw_mixture.a_ints = internals; },
   pos)

let annotate_agent_with_diff
    sigs ?contact_map (agent_name, _ as ag_ty) links_annot lp rp =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports = Array.make arity (Locality.dummy_annot Ast.LNK_ANY, Maintained) in
  let internals = Array.make arity I_ANY in
  let register_port_modif p_id lnk1 p' (lhs_links,rhs_links as links_annot) =
    match lnk1,p'.Ast.port_lnk with
    | [Ast.LNK_ANY,_], [Ast.LNK_ANY,_] -> links_annot
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
    | [Ast.LNK_ANY,pos], ([Ast.FREE,_] | []) ->
      let () = ports.(p_id) <- ((Ast.LNK_ANY,pos), Freed) in
      links_annot
    | [Ast.LNK_SOME,pos_lnk], ([Ast.FREE,_] | []) ->
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
    | [Ast.LNK_TYPE (dst_p,dst_ty),pos_lnk], ([Ast.FREE,_] | []) ->
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
    | ([Ast.FREE,_] | []), ([Ast.FREE,_] | []) ->
      let () = ports.(p_id) <- (Locality.dummy_annot Ast.FREE, Maintained) in
      links_annot
    | [Ast.LNK_VALUE (i,()),pos], ([Ast.FREE,_] | []) ->
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
    | ([Ast.FREE,_] | []), [Ast.LNK_VALUE (i,()),pos] ->
      let () =
        ports.(p_id) <- (Locality.dummy_annot Ast.FREE, Linked (i,pos)) in
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
  let find_in_rp (na,pos) rp =
    let (p',r) =
      List.partition (fun p -> String.compare (fst p.Ast.port_nme) na = 0) rp in
    match p' with
    | [p'] -> (p',r)
    | [] -> not_enough_specified agent_name (na,pos)
    | _ :: _ -> several_occurence_of_site agent_name (na,pos) in
  let rp_r,annot,_ =
    List.fold_left
      (fun (rp,annot,pset) p ->
         let p_na = p.Ast.port_nme in
         let p_id = Signature.num_of_site ~agent_name p_na sign in
         let pset' = Mods.IntSet.add p_id pset in
         let () = if pset == pset' then
             several_occurence_of_site agent_name p.Ast.port_nme in

         let p',rp' = find_in_rp p_na rp in
         let annot' = register_port_modif p_id p.Ast.port_lnk p' annot in
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
  { ra_type = ag_id; ra_ports = ports; ra_ints = internals; ra_erased = false;
    ra_syntax = Some (Array.copy ports, Array.copy internals);},annot'

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
            | ((Ast.LNK_VALUE _ | Ast.LNK_ANY |
                Ast.LNK_SOME | Ast.LNK_TYPE _ | Ast.FREE),_),_ -> ())
         ra.ra_ports) mix

(*
Is responsible for the check that:
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)

let annotate_lhs_with_diff sigs ?contact_map lhs rhs =
  let rec aux links_annot acc lhs rhs =
    match lhs,rhs with
    | ((lag_na,_ as ag_ty),lag_p)::lt, ((rag_na,_),rag_p)::rt
      when String.compare lag_na rag_na = 0 &&
           Ast.no_more_site_on_right true lag_p rag_p ->
      let ra,links_annot' =
        annotate_agent_with_diff
          sigs ?contact_map ag_ty links_annot lag_p rag_p in
      aux links_annot' (ra::acc) lt rt
    | erased, added ->
      let () =
        if added <> [] then
          List.iter (fun ((lag,pos),lag_p) ->
              if List.exists
                  (fun ((rag,_),rag_p) ->
                     String.compare lag rag = 0 &&
                     Ast.no_more_site_on_right false lag_p rag_p) added then
                ExceptionDefn.warning ~pos
                  (fun f ->
                     Format.fprintf
                       f "Rule induced deletion AND creation of the agent %s" lag))
            erased in
      let mix,(lhs_links_one,lhs_links_two) =
        List.fold_left
          (fun (acc,lannot) x ->
             let ra,lannot' =
               annotate_dropped_agent sigs lannot x in
             (ra::acc,lannot'))
          (acc,fst links_annot) erased in
      let () =
        match Mods.IntMap.root lhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
      let () = refer_links_annot lhs_links_two mix in
      let cmix,(rhs_links_one,_),_ =
        List.fold_left
          (fun (acc,rannot,id) x ->
             let rannot',x' =
               annotate_created_agent sigs ?contact_map rannot x in
             x'::acc,rannot',succ id)
          ([],snd links_annot,0) added in
      let () =
        match Mods.IntMap.root rhs_links_one with
        | None -> ()
        | Some (i,(_,_,_,pos)) -> link_only_one_occurence i pos in
      List.rev mix, List.rev cmix in
  aux
    ((Mods.IntMap.empty,Mods.IntMap.empty),(Mods.IntMap.empty,Mods.IntMap.empty))
    [] lhs rhs

let add_un_variable k_un acc rate_var =
  match k_un with
  | None -> (acc,None)
  | Some (k,dist) ->
    let acc_un,k' = if Alg_expr.has_mix (fst k) then
        ((Locality.dummy_annot rate_var,k)::acc,
         Locality.dummy_annot (Alg_expr.ALG_VAR rate_var))
      else (acc,k) in
    (acc_un,Some (k',dist))

let name_and_purify_rule (label_opt,(r,r_pos)) ((id,set),acc,rules) =
  let id',label = match label_opt with
    | None ->
       (succ id,set), Format.asprintf "r%i: %a" id Ast.print_ast_rule r
    | Some (lab,pos) ->
      let set' = Mods.StringSet.add lab set in
      if set == set' then
        raise
          (ExceptionDefn.Malformed_Decl
             ("A rule named '"^lab^"' already exists.",pos))
      else if r.Ast.bidirectional then
        let set'' =
          Mods.StringSet.add (Ast.flip_label lab) set' in
        if set' == set'' then
          raise
            (ExceptionDefn.Malformed_Decl
               ("A rule named '"^Ast.flip_label lab^"' already exists.",pos))
        else (id,set''),lab
      else (id,set'),lab in
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
       (Tools.option_map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        Locality.dummy_annot (Alg_expr.ALG_VAR rate_var),k_op_un,r_pos)::rules)
    | true, Some rate ->
      let rate_var_un = (Ast.flip_label label)^"_un_rate" in
      let acc_un, k_op_un = add_un_variable r.Ast.k_op_un acc'' rate_var_un in
      (acc_un,
       (Tools.option_map (fun (l,p) -> (Ast.flip_label l,p)) label_opt,
        r.Ast.rhs,r.Ast.lhs,r.Ast.add_token,r.Ast.rm_token,
        rate,k_op_un,r_pos)::rules)
    | false, None -> (acc'',rules)
    | (false, Some _ | true, None) ->
       raise
         (ExceptionDefn.Malformed_Decl
            ("Incompatible arrow and kinectic rate for inverse definition",
             r_pos)) in
  (id',acc''',
   (label_opt,r.Ast.lhs,r.Ast.rhs,r.Ast.rm_token,r.Ast.add_token,
    k_def,k_un,r_pos)
   ::rules')

let mixture_of_ast sigs ?contact_map pos mix =
  match annotate_lhs_with_diff sigs ?contact_map mix mix with
  | r, [] -> r
  | _, _ -> raise (ExceptionDefn.Internal_Error
                     ("A mixture cannot create agents",pos))

let rec alg_expr_of_ast sigs tok algs ?max_allowed_var (alg,pos) =
  ((match alg with
      | Alg_expr.KAPPA_INSTANCE ast ->
        Alg_expr.KAPPA_INSTANCE (mixture_of_ast sigs pos ast)
      | Alg_expr.ALG_VAR lab ->
        let i =
          match Mods.StringMap.find_option lab algs with
          | Some x -> x
          | None ->
            raise (ExceptionDefn.Malformed_Decl
                     (lab ^" is not a declared variable",pos)) in
        let () = match max_allowed_var with
          | Some j when j < i ->
            raise (ExceptionDefn.Malformed_Decl
                     ("Reference to not yet defined '"^lab ^"' is forbidden.",
                      pos))
          | None | Some _ -> ()
        in Alg_expr.ALG_VAR i
      | Alg_expr.TOKEN_ID tk_nme ->
        let i =
          match Mods.StringMap.find_option tk_nme tok with
          | Some x -> x
          | None ->
            raise (ExceptionDefn.Malformed_Decl
                     (tk_nme ^ " is not a declared token",pos))
        in Alg_expr.TOKEN_ID i
      | (Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _) as x -> x
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        Alg_expr.BIN_ALG_OP
          (op,
           alg_expr_of_ast sigs tok algs ?max_allowed_var a,
           alg_expr_of_ast sigs tok algs ?max_allowed_var b)
      | Alg_expr.UN_ALG_OP (op,a) ->
        Alg_expr.UN_ALG_OP
          (op,alg_expr_of_ast sigs tok algs ?max_allowed_var a)
      | Alg_expr.IF (cond,yes,no) ->
        Alg_expr.IF
          (bool_expr_of_ast sigs tok algs ?max_allowed_var cond,
           alg_expr_of_ast sigs tok algs ?max_allowed_var yes,
           alg_expr_of_ast sigs tok algs ?max_allowed_var no)
    ),
   pos)
and bool_expr_of_ast sigs tok algs ?max_allowed_var = function
  | (Alg_expr.TRUE | Alg_expr.FALSE),_ as x -> x
  | Alg_expr.BOOL_OP (op,x,y),pos ->
    Alg_expr.BOOL_OP
      (op, bool_expr_of_ast sigs tok algs ?max_allowed_var x,
       bool_expr_of_ast sigs tok algs ?max_allowed_var y),
    pos
  | Alg_expr.COMPARE_OP (op,x,y),pos ->
    Alg_expr.COMPARE_OP
      (op,alg_expr_of_ast sigs tok algs ?max_allowed_var  x,
       alg_expr_of_ast sigs tok algs ?max_allowed_var y),pos

let print_expr_of_ast sigs tok algs = function
  | Ast.Str_pexpr _ as x -> x
  | Ast.Alg_pexpr x ->
    Ast.Alg_pexpr (alg_expr_of_ast sigs tok algs x)

let modif_expr_of_ast sigs tok algs contact_map modif acc =
  match modif with
  | Ast.INTRO (how,(who,pos)) ->
    Ast.INTRO
      (alg_expr_of_ast sigs tok algs how,
       (mixture_of_ast sigs ~contact_map pos who,pos)),
    acc
  | Ast.DELETE (how,(who,pos)) ->
    Ast.DELETE
      (alg_expr_of_ast sigs tok algs how, (mixture_of_ast sigs pos who,pos)),
    acc
  | Ast.UPDATE ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab algs with
      | Some i -> i
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 ("Variable " ^ (lab ^ " is not defined"),pos)) in
    Ast.UPDATE ((i,pos),alg_expr_of_ast sigs tok algs how),
    i::acc
  | Ast.UPDATE_TOK ((lab,pos),how) ->
    let i =
      match Mods.StringMap.find_option lab tok with
      | Some x -> x
      | None ->
        raise (ExceptionDefn.Malformed_Decl
                 (lab ^" is not a declared token",pos)) in
    Ast.UPDATE_TOK ((i,pos), alg_expr_of_ast sigs tok algs how),
    acc
  | Ast.STOP p ->
    Ast.STOP (List.map (print_expr_of_ast sigs tok algs) p),acc
  | Ast.SNAPSHOT p ->
    Ast.SNAPSHOT (List.map (print_expr_of_ast sigs tok algs) p),acc
  | Ast.FLUX (rel,p) ->
    Ast.FLUX (rel,List.map (print_expr_of_ast sigs tok algs) p),acc
  | Ast.FLUXOFF p ->
    Ast.FLUXOFF (List.map (print_expr_of_ast sigs tok algs) p),acc
  | (Ast.PLOTENTRY | Ast.CFLOWLABEL (_,_ ) as x) -> x,acc
  | Ast.PRINT (p,p') ->
    Ast.PRINT
      (List.map (print_expr_of_ast sigs tok algs) p,
       List.map (print_expr_of_ast sigs tok algs) p'),acc
  | Ast.CFLOWMIX (b,(m,pos)) ->
    Ast.CFLOWMIX (b,(mixture_of_ast sigs pos m,pos)),acc

let perturbation_of_ast
    sigs tok algs contact_map ((pre,mods,post),pos) up_vars =
  let mods',up_vars' =
    List_util.fold_right_map
      (modif_expr_of_ast sigs tok algs contact_map) mods up_vars in
  ((bool_expr_of_ast sigs tok algs pre,mods',
    match post with
    | None -> None
    | Some post -> Some (bool_expr_of_ast sigs tok algs post)),pos),
  up_vars'

let init_of_ast sigs tok contact_map = function
  | Ast.INIT_MIX who,pos ->
    Ast.INIT_MIX (mixture_of_ast sigs ~contact_map pos who),pos
  | Ast.INIT_TOK lab,pos ->
    match Mods.StringMap.find_option lab tok with
    | Some x -> Ast.INIT_TOK x,pos
    | None ->
      raise (ExceptionDefn.Malformed_Decl
               (lab ^" is not a declared token",pos))

let create_t ast_intf =
  NamedDecls.create (
    Tools.array_map_of_list
      (fun p ->
         (p.Ast.port_nme,
          (NamedDecls.create
             (Tools.array_map_of_list (fun x -> (x,())) p.Ast.port_int),
           List.fold_left (fun acc -> function
               | (Ast.FREE | Ast.LNK_ANY), _ -> acc
               | (Ast.LNK_SOME | Ast.LNK_VALUE _), pos ->
                 raise (ExceptionDefn.Malformed_Decl
                          ("Forbidden link status inside a definition of signature", pos))
               | Ast.LNK_TYPE (a,b), _ -> (a,b) :: acc) [] p.Ast.port_lnk))
      ) ast_intf)

let create_sig l =
  Tools.array_map_of_list
    (fun (name,intf) -> (name,create_t intf)) l

let compil_of_ast overwrite c =
  let c =
    if c.Ast.signatures = [] && c.Ast.tokens = []
    then Ast.implicit_signature c
    else c in
  let sigs = Signature.create (create_sig c.Ast.signatures) in
  let contact_map =
    Array.init
      (Signature.size sigs)
      (fun i -> Array.init (Signature.arity sigs i)
          (fun s -> (Tools.recti
                       (fun a k -> k::a) []
                       (Signature.internal_states_number i s sigs),[]))) in
  let ((_,rule_names),extra_vars,cleaned_rules) =
    List.fold_right
      name_and_purify_rule c.Ast.rules ((0,Mods.StringSet.empty),[],[]) in
  let alg_vars_over =
    List_util.rev_map_append
      (fun (x,v) -> (Locality.dummy_annot x,
                     Locality.dummy_annot (Alg_expr.CONST v))) overwrite
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
      (perturbation_of_ast sigs tok algs contact_map) c.Ast.perturbations [] in
  sigs,contact_map,tk_nd,updated_vars,
  {
    Ast.variables =
      List_util.mapi
        (fun i (lab,expr) ->
           (lab,alg_expr_of_ast ~max_allowed_var:(pred i) sigs tok algs expr))
        alg_vars_over;
    Ast.rules =
      List.map (fun (label,lhs,rhs,rm_tk,add_tk,rate,un_rate,r_pos) ->
          let mix,created =
            annotate_lhs_with_diff sigs ~contact_map lhs rhs in
          label,
          ({ r_mix = mix; r_created = List.map fst created;
             r_rm_tokens =
               List.map (fun (al,tk) ->
                   (alg_expr_of_ast sigs tok algs al,
                    NamedDecls.elt_id ~kind:"token" tk_nd tk))
                 rm_tk;
             r_add_tokens =
               List.map (fun (al,tk) ->
                   (alg_expr_of_ast sigs tok algs al,
                    NamedDecls.elt_id ~kind:"token" tk_nd tk))
                 add_tk;
             r_rate = alg_expr_of_ast sigs tok algs rate;
             r_un_rate =
               let r_dist d =
                 alg_expr_of_ast sigs tok algs ?max_allowed_var:None d in
               Tools.option_map
                 (fun (un_rate',dist) ->
                   let un_rate'' =
                     alg_expr_of_ast sigs tok algs
                                     ?max_allowed_var:None un_rate' in
                   match dist with
                   | Some d -> (un_rate'', Some (r_dist d))
                   | None -> (un_rate'', None))
                 un_rate;
           },r_pos)) cleaned_rules;
    Ast.observables =
      List.map (fun expr -> alg_expr_of_ast sigs tok algs expr)
        c.Ast.observables;
    Ast.init =
      List.map (fun (lab,expr,ini) ->
          lab,alg_expr_of_ast sigs tok algs expr,
          init_of_ast sigs tok contact_map ini)
        c.Ast.init;
    Ast.perturbations = perts';
    Ast.volumes = c.Ast.volumes;
    Ast.tokens = c.Ast.tokens;
    Ast.signatures = c.Ast.signatures;
    Ast.configurations = c.Ast.configurations;
  }
