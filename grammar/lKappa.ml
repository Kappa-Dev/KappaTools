(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type switching = Linked of int | Freed | Maintained | Erased

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

type 'a rule_agent_counters =
  {
    ra : 'a;
    ra_counters : (Ast.counter * switching) option array;
  }

type rule =
  {
    r_mix: rule_mixture;
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
    Format.fprintf f "/*%a.%a*/"
      (Signature.print_site sigs a) s
      (Signature.print_agent sigs) a

let print_rule_internal sigs ~show_erased ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j ->
    Format.fprintf f "{#/%a}" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED -> if show_erased then Format.fprintf f "~--"
  | I_VAL_CHANGED (i,j) ->
    if i <> j then
      Format.fprintf
        f "{%a/%a}" (Signature.print_internal_state sigs ag_ty site) i
        (Signature.print_internal_state sigs ag_ty site) j
    else
      Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) i
  | I_VAL_ERASED i ->
    Format.fprintf
      f "{%a%t}" (Signature.print_internal_state sigs ag_ty site) i
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
  | Linked i -> Format.fprintf f "/%i" i
  | Freed -> Format.pp_print_string f "/."
  | Maintained -> ()
  | Erased -> if show_erased then Format.pp_print_string f "--"

let switching_to_json = function
  | Freed -> `String "Freed"
  | Maintained -> `String "Maintained"
  | Erased -> `String "Erased"
  | Linked i -> JsonUtil.of_int i

let switching_of_json = function
  | `String "Freed" -> Freed
  | `String "Maintained" -> Maintained
  | `String "Erased"-> Erased
  | x -> Linked (JsonUtil.to_int ~error_msg:"Invalid Switching" x)

let print_rule_link sigs ~show_erased ~ltypes f ((e,_),s) =
  Format.fprintf
    f "[%a%a]"
    (Ast.print_link
       (Signature.print_site sigs)
       (Signature.print_agent sigs) (print_link_annot ~ltypes sigs))
    e
    (print_switching ~show_erased) s

let print_counter_test f = function
  | (c,true) -> Format.fprintf f "=%i" c
  | (c,false) -> Format.fprintf f ">=%i" c

let print_counter_delta counters j f switch = match switch with
  | Linked i ->
     begin
       let root = Raw_mixture.find counters i in
       let (s,(_,is_counter)) =
         Mods.DynArray.get counters.Raw_mixture.rank root in
       let delta = if (is_counter) then s-1 else (j-i) in
       Format.fprintf f "/+=%d" delta
     end
  | Freed ->
     raise (ExceptionDefn.Internal_Error
              (Locality.dummy_annot("Cannot erase all increment agents")))
  | Maintained -> ()
  | Erased -> ()

let print_rule_intf
      sigs ~show_erased ~ltypes ag_ty f (ports,ints,counters,created_counters) =
  let rec aux empty i =
    if i < Array.length ports then
      if (match ports.(i) with
         | (Ast.LNK_ANY, _), Maintained ->  ints.(i) <> I_ANY
         | ((Ast.LNK_ANY, _), (Erased | Freed | Linked _) |
            ((Ast.LNK_SOME | Ast.ANY_FREE | Ast.LNK_FREE |
              Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true) then

        let ((e,_),switch) = ports.(i) in
        let is_counter = match e with
          | Ast.ANY_FREE | Ast.LNK_FREE | Ast.LNK_ANY
            | Ast.LNK_TYPE _ | Ast.LNK_SOME -> false
          | Ast.LNK_VALUE (j,_) ->
             try
               let root = Raw_mixture.find counters j in
               let (c,(eq,is_counter')) =
                 Mods.DynArray.get counters.Raw_mixture.rank root in
               if (is_counter')&&not(!Parameter.debugModeOn) then
                 let () = Format.fprintf f "%t%a{%a%a}"
                          (if empty then Pp.empty else Pp.space)
                          (Signature.print_site sigs ag_ty) i
                          print_counter_test (c-1,eq)
                          (print_counter_delta created_counters j) switch
                 in true else false
             with Invalid_argument _ -> false in
        let () = if not(is_counter) then
                   Format.fprintf
                     f "%t%a%a%a" (if empty then Pp.empty else Pp.space)
                     (Signature.print_site sigs ag_ty) i
                     (print_rule_internal sigs ~show_erased ag_ty i)
                     ints.(i)
                     (print_rule_link sigs ~show_erased ~ltypes)
                     ports.(i) else () in
        aux false (succ i)
      else aux empty (succ i) in
  aux true 0

let union_find_counters sigs mix =
  let t = Raw_mixture.create 1 in
  let () =
    match sigs with
    | None -> ()
    | Some sigs ->
      List.iter
        (fun ag ->
           match Signature.ports_if_counter_agent sigs ag.ra_type with
           | None -> ()
           | Some (before,after) ->
             let ((a,_),_) = ag.ra_ports.(after) in
             let ((b,_),_) = ag.ra_ports.(before) in
             match b with
             | Ast.ANY_FREE | Ast.LNK_FREE | Ast.LNK_ANY
             | Ast.LNK_TYPE _ | Ast.LNK_SOME -> ()
             | Ast.LNK_VALUE (lnk_b,_) ->
               match a with
               | Ast.LNK_VALUE (lnk_a,_) -> Raw_mixture.union t lnk_b lnk_a
               | Ast.ANY_FREE | Ast.LNK_FREE ->
                 let root = Raw_mixture.find t lnk_b in
                 let (s,_) = Mods.DynArray.get t.Raw_mixture.rank root in
                 Mods.DynArray.set t.Raw_mixture.rank root (s,(true,true))
               | Ast.LNK_ANY ->
                 let root = Raw_mixture.find t lnk_b in
                 let (s,_) = Mods.DynArray.get t.Raw_mixture.rank root in
                 Mods.DynArray.set t.Raw_mixture.rank root (s,(false,true))
               | Ast.LNK_TYPE _ | Ast.LNK_SOME ->
                 raise (ExceptionDefn.Internal_Error
                          (Locality.dummy_annot
                             ("Port a of __incr agent not well specified"))))
        mix in
  t

let print_rule_agent sigs ~ltypes counters created_counters f ag =
  Format.fprintf f "%a(@[<h>%a@])%t"
    (Signature.print_agent sigs) ag.ra_type
    (print_rule_intf sigs ~show_erased:false ~ltypes ag.ra_type)
    (ag.ra_ports,ag.ra_ints,counters,created_counters)
    (fun f -> if ag.ra_erased then Format.pp_print_string f "-")

let print_rule_mixture sigs ~ltypes created f mix =
  let incr_agents = union_find_counters (Some sigs) mix in
  let created_incr = Raw_mixture.union_find_counters (Some sigs) created in
  let rec aux_print some = function
    | [] -> ()
    | h::t ->
      if Signature.is_counter_agent (Some sigs) h.ra_type
      && not !Parameter.debugModeOn
      then aux_print some t
      else
        let () = if some then Pp.comma f in
        let () = print_rule_agent sigs ~ltypes incr_agents created_incr f h in
        aux_print true t in
  aux_print false mix

let print_internal_lhs sigs ag_ty site f = function
  | I_ANY -> ()
  | (I_ANY_CHANGED _ | I_ANY_ERASED) -> Format.pp_print_string f "{#}"
  | (I_VAL_CHANGED (i,_) | I_VAL_ERASED i) ->
    Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) i

let print_internal_rhs sigs ag_ty site f = function
  | I_ANY -> ()
  | (I_ANY_CHANGED j | I_VAL_CHANGED (_,j)) ->
    Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) j
  | (I_ANY_ERASED | I_VAL_ERASED _) -> assert false

let print_link_lhs ~ltypes sigs f ((e,_),_) =
  Ast.print_link
    (Signature.print_site sigs)
    (Signature.print_agent sigs) (print_link_annot ~ltypes sigs)
    f e

let print_link_rhs ~ltypes sigs f ((e,_),s) =
  match s with
  | Linked i ->
    Ast.print_link
      (Signature.print_site sigs)
      (Signature.print_agent sigs) (fun _ () -> ())
      f (Ast.LNK_VALUE (i,()))
  | Freed -> Format.pp_print_string f "."
  | Maintained ->
    Ast.print_link
      (Signature.print_site sigs)
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
            f "%t%a%a[%a]"
            (if empty then Pp.empty else Pp.space)
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
            f "%t%a%a[%a]"
            (if empty then Pp.empty else Pp.space)
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
        (Raw_mixture.print ~created:false ~sigs) created
    | h :: t ->
      if h.ra_erased then
        let () = Format.fprintf f "%t."
            (if empty then Pp.empty else Pp.comma) in
        aux false t
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
       (fun f m -> Format.fprintf f "|%a|" (print_rule_mixture sigs ~ltypes []) m)
       pr_tok pr_var) (fst r.r_rate)
    (fun f ->
       match r.r_un_rate with
       | None -> ()
       | Some ((ra,_),max_dist) ->
         Format.fprintf
           f " {%a%a}"
           (Alg_expr.print
              (fun f m -> Format.fprintf f "|%a|"
                  (print_rule_mixture sigs ~ltypes []) m)
              pr_tok pr_var) ra
           (Pp.option
              (fun f (md,_) ->
                Format.fprintf f ":%a"
                (Alg_expr.print
                   (fun f m -> Format.fprintf f "|%a|"
                       (print_rule_mixture sigs ~ltypes []) m)
                   pr_tok pr_var) md)) max_dist)

let print_rule ~full sigs pr_tok pr_var f r =
  Format.fprintf
    f "@[<h>%t%t%a%t@]"
    (fun f ->
       if full || r.r_editStyle then
         Format.fprintf f "%a%t%a"
           (print_rule_mixture sigs ~ltypes:false r.r_created) r.r_mix
           (fun f -> if r.r_mix <> [] && r.r_created <> [] then Pp.comma f)
           (Raw_mixture.print ~created:true ~sigs)
           r.r_created
       else Format.fprintf f "%a%t%a -> %a"
           (Pp.list Pp.comma (print_agent_lhs ~ltypes:false sigs)) r.r_mix
           (fun f -> if r.r_mix <> [] && r.r_created <> [] then Pp.comma f)
           (Pp.list Pp.comma (fun f _ -> Format.pp_print_string f "."))
           r.r_created
           (print_rhs ~ltypes:false sigs r.r_created) r.r_mix)
    (fun f ->
       match r.r_delta_tokens with [] -> ()
                                 | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       Pp.comma
       (fun f ((nb,_),tk) ->
          Format.fprintf
            f "%a %a"
            (Alg_expr.print
               (fun f m -> Format.fprintf
                   f "|%a|" (print_rule_mixture sigs ~ltypes:false []) m)
               pr_tok pr_var) nb
            pr_tok tk))
    r.r_delta_tokens
    (fun f -> if full then print_rates sigs pr_tok pr_var f r)

let rule_agent_to_json filenames a =
  `Assoc [
    "type", `Int a.ra_type;
    "bindings",
    `List (Array.fold_right
             (fun (e,s) c ->
                (`List [
                    Locality.annot_to_yojson
                      ~filenames
                      (Ast.link_to_json (fun _ i -> `Int i) (fun i -> `Int i)
                         (fun (s,a) -> [`Int s;`Int a])) e;
                    switching_to_json s])::c)
             a.ra_ports []);
    "states",
    `List (Array.fold_right
             (fun x c -> rule_internal_to_json x :: c) a.ra_ints []);
    "erased", `Bool a.ra_erased;
  ]
let rule_agent_of_json filenames = function
  | `Assoc l as x when List.length l = 4 ->
    begin
      try
        let ports =
          match List.assoc "bindings" l with
          | `List s ->
            Tools.array_map_of_list
              (function
                | `List [e;s] ->
                  (Locality.annot_of_yojson
                     ~filenames
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

let rule_mixture_to_json filenames =
  JsonUtil.of_list (rule_agent_to_json filenames)
let rule_mixture_of_json filenames =
  JsonUtil.to_list (rule_agent_of_json filenames)

let lalg_expr_to_json filenames =
  Alg_expr.e_to_yojson
    ~filenames (rule_mixture_to_json filenames) JsonUtil.of_int
let lalg_expr_of_json filenames =
  Alg_expr.e_of_yojson
    ~filenames (rule_mixture_of_json filenames)
    (JsonUtil.to_int ?error_msg:None)

let rule_to_json ~filenames r =
  `Assoc
    [
      "mixture", rule_mixture_to_json filenames r.r_mix;
      "created", Raw_mixture.to_json r.r_created;
      "delta_tokens",
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
           (Locality.annot_to_yojson ~filenames (lalg_expr_to_json filenames))
           JsonUtil.of_int)
        r.r_delta_tokens;
      "rate", Locality.annot_to_yojson
        ~filenames (lalg_expr_to_json filenames) r.r_rate;
      "unary_rate",
      JsonUtil.of_option
        (JsonUtil.of_pair
           (Locality.annot_to_yojson ~filenames (lalg_expr_to_json filenames))
           (JsonUtil.of_option
              (Locality.annot_to_yojson
                 ~filenames (lalg_expr_to_json filenames))))
        r.r_un_rate;
      "editStyle", `Bool r.r_editStyle;
    ]
let rule_of_json ~filenames = function
  | `Assoc l as x when List.length l < 7 ->
    begin
      try
        {
          r_mix = rule_mixture_of_json filenames (List.assoc "mixture" l);
          r_created = Raw_mixture.of_json (List.assoc "created" l);
          r_delta_tokens =
            JsonUtil.to_list
              (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                 (Locality.annot_of_yojson
                    ~filenames (lalg_expr_of_json filenames))
                 (JsonUtil.to_int ?error_msg:None))
              (List.assoc "delta_tokens" l);
          r_rate =
            Locality.annot_of_yojson ~filenames (lalg_expr_of_json filenames)
              (List.assoc "rate" l);
          r_un_rate =
            (try
               JsonUtil.to_option
                 (JsonUtil.to_pair
                    (Locality.annot_of_yojson
                       ~filenames (lalg_expr_of_json filenames))
                    (JsonUtil.to_option
                       (Locality.annot_of_yojson
                          (lalg_expr_of_json filenames))))
                 (List.assoc "unary_rate" l)
             with Not_found -> None);
           r_editStyle = Yojson.Basic.Util.to_bool (List.assoc "editStyle" l);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect rule",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect rule",x))

let forbid_modification pos = function
  | None -> ()
  | Some _ ->
    raise (ExceptionDefn.Malformed_Decl
             ("A modification is forbidden here.",pos))

let several_internal_states pos =
  raise (ExceptionDefn.Malformed_Decl
           ("In a pattern, a site cannot have several internal states.",pos))

let not_enough_specified ~status ~side agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
           ("The "^status^" state of agent '"^agent_name^"', site '"^na
            ^"' on the "^side^" hand side is underspecified",pos))

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
                not_enough_specified
                  ~status:"linking" ~side:"left" ag_na (p_na,pos)
              | (Ast.LNK_VALUE (i,_), _),_ -> Raw_mixture.VAL i
              | (((Ast.LNK_ANY | Ast.ANY_FREE | Ast.LNK_FREE), _)),_ ->
                Raw_mixture.FREE
           )
           r.ra_ports in
       { Raw_mixture.a_type = r.ra_type;
         Raw_mixture.a_ports = ports; Raw_mixture.a_ints =
                                        internals; })
    x
