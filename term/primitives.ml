(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Transformation = struct
  type 'a t =
    | Agent of 'a
    | Freed of 'a Instantiation.site
    | Linked of 'a Instantiation.site * 'a Instantiation.site
    | NegativeWhatEver of 'a Instantiation.site
    | PositiveInternalized of
        'a * Instantiation.site_name * Instantiation.internal_state
    | NegativeInternalized of 'a Instantiation.site

  let to_yojson = function
    | Agent a -> `Assoc ["Agent", Matching.Agent.to_yojson a]
    | Freed (a,s) ->
       `Assoc ["Freed", `List [Matching.Agent.to_yojson a; `Int s]]
    | Linked ((a,s),(b,t)) ->
       `Assoc ["Linked",
               `List [Matching.Agent.to_yojson a;`Int s;
                      Matching.Agent.to_yojson b;`Int t]]
    | NegativeWhatEver (a,s) ->
       `Assoc ["NegativeWhatEver", `List [Matching.Agent.to_yojson a; `Int s]]
    | PositiveInternalized (a,s,i) ->
       `Assoc ["PositiveInternalized",
               `List [Matching.Agent.to_yojson a;`Int s;`Int i]]
    | NegativeInternalized (a,s) ->
       `Assoc ["NegativeInternalized",`List [Matching.Agent.to_yojson a;`Int s]]

  let of_yojson = function
    | `Assoc ["Agent", a] -> Agent (Matching.Agent.of_yojson a)
    | `Assoc ["Freed", `List [a;`Int s]] ->
       Freed ((Matching.Agent.of_yojson a),s)
    | `Assoc ["Linked",`List [a;(`Int s); b;(`Int t)]] ->
       Linked ((Matching.Agent.of_yojson a,s),(Matching.Agent.of_yojson b,t))
    | `Assoc ["NegativeWhatEver",`List [a;`Int s]] ->
       NegativeWhatEver (Matching.Agent.of_yojson a,s)
    | `Assoc ["PositiveInternalized",`List [a;`Int s;`Int i]] ->
       PositiveInternalized (Matching.Agent.of_yojson a,s,i)
    | `Assoc ["NegativeInternalized",`List [a;`Int s]] ->
       NegativeInternalized (Matching.Agent.of_yojson a,s)
    | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x))

  let rename id inj = function
    | Freed (p,s) as x ->
      let p' = Matching.Agent.rename id inj p in
      if p == p' then x else Freed (p',s)
    | NegativeWhatEver (p,s) as x ->
      let p' = Matching.Agent.rename id inj p in
      if p == p' then x else NegativeWhatEver (p',s)
    | Linked ((p1,s1),(p2,s2)) as x ->
      let p1' = Matching.Agent.rename id inj p1 in
      let p2' = Matching.Agent.rename id inj p2 in
      if p1 == p1' && p2 == p2' then x else Linked ((p1',s1),(p2',s2))
    | PositiveInternalized (p,s,i) as x ->
      let p' = Matching.Agent.rename id inj p in
      if p == p' then x else PositiveInternalized (p',s,i)
    | NegativeInternalized (p,s) as x ->
      let p' = Matching.Agent.rename id inj p in
      if p == p' then x else NegativeInternalized (p',s)
    | Agent p as x ->
      let p' = Matching.Agent.rename id inj p in
      if p == p' then x else Agent p'

  let concretize inj2graph = function
    | Agent n -> Agent (Matching.Agent.concretize inj2graph n)
    | Freed (n,s) -> Freed (Matching.Agent.concretize inj2graph n,s)
    | Linked ((n,s),(n',s')) ->
      Linked ((Matching.Agent.concretize inj2graph n,s),
              (Matching.Agent.concretize inj2graph n',s'))
    | NegativeWhatEver (n,s) ->
      NegativeWhatEver (Matching.Agent.concretize inj2graph n,s)
    | PositiveInternalized (n,s,i) ->
      PositiveInternalized (Matching.Agent.concretize inj2graph n,s,i)
    | NegativeInternalized (n,s) ->
      NegativeInternalized (Matching.Agent.concretize inj2graph n,s)

  let raw_mixture_of_fresh sigs l =
    let (_,fresh,mixte,existings) =
      List.fold_left
        (fun (fid,fr,mi,ex) -> function
           | (NegativeWhatEver _ | NegativeInternalized _ |
              Agent (Matching.Agent.Existing _) |
              Linked ((Matching.Agent.Existing _,_),(Matching.Agent.Existing _,_)) |
              PositiveInternalized (Matching.Agent.Existing _,_,_) |
              Freed (Matching.Agent.Existing _,_)  as x) -> (fid,fr,mi,x::ex)
           | Agent (Matching.Agent.Fresh (a_type,id)) ->
             let si = Signature.arity sigs a_type in
             let n = {
               Raw_mixture.a_type;
               Raw_mixture.a_ports = Array.make si Raw_mixture.FREE;
               Raw_mixture.a_ints = Array.make si None;
             } in
             (fid,Mods.IntMap.add id n fr,mi,ex)
           | PositiveInternalized (Matching.Agent.Fresh (_,id),s,i) ->
             let () = match Mods.IntMap.find_option id fr with
               | Some a -> a.Raw_mixture.a_ints.(s) <- Some i
               | None -> () in
             (fid,fr,mi,ex)
           | Freed (Matching.Agent.Fresh _,_) -> (fid,fr,mi,ex)
           | Linked ((Matching.Agent.Fresh (_,id),s1),
                      (Matching.Agent.Existing _ as a,s2)) |
             Linked ((Matching.Agent.Existing _ as a,s2),
                     (Matching.Agent.Fresh (_,id),s1)) ->
             let () = match Mods.IntMap.find_option id fr with
               | Some a -> a.Raw_mixture.a_ports.(s1) <- Raw_mixture.VAL fid
               | None -> () in
             (succ fid,fr,(a,s2,fid)::mi,ex)
           | Linked ((Matching.Agent.Fresh (_,id1),s1),
                     (Matching.Agent.Fresh (_,id2),s2)) ->
             let () = match Mods.IntMap.find_option id1 fr with
               | Some a -> a.Raw_mixture.a_ports.(s1) <- Raw_mixture.VAL fid
               | None -> () in
             let () = match Mods.IntMap.find_option id2 fr with
               | Some a -> a.Raw_mixture.a_ports.(s2) <- Raw_mixture.VAL fid
               | None -> () in
             (succ fid,fr,mi,ex)
        ) (1,Mods.IntMap.empty,[],[]) l in
    (Mods.IntMap.bindings fresh,mixte,List.rev existings)

  let print ?sigs f = function
    | Agent p ->
      Format.fprintf f "@[%a@]" (Matching.Agent.print ?sigs) p
    | Freed (p,s) ->
      Format.fprintf
        f "@[%a.%a = %t@]" (Matching.Agent.print ?sigs) p
        (Matching.Agent.print_site ?sigs p) s Pp.bottom
    | NegativeWhatEver (p,s) ->
      Format.fprintf
        f "@[%a.%a = ???@]" (Matching.Agent.print ?sigs) p
        (Matching.Agent.print_site ?sigs p) s
    | Linked ((p1,s1),(p2,s2)) ->
      Format.fprintf
        f "@[%a.%a = %a.%a@]"
        (Matching.Agent.print ?sigs) p1 (Matching.Agent.print_site ?sigs p1) s1
        (Matching.Agent.print ?sigs) p2 (Matching.Agent.print_site ?sigs p2) s2
    | PositiveInternalized (p,s,i) ->
      Format.fprintf
        f "@[%a.%a =@]" (Matching.Agent.print ?sigs) p
        (Matching.Agent.print_internal ?sigs p s) i
    | NegativeInternalized (p,s) ->
      Format.fprintf
        f "@[%a.%a~ =@]" (Matching.Agent.print ?sigs) p
        (Matching.Agent.print_site ?sigs p) s

  let get_negative_part created_agents lnk_dst ((a,_),_ as p) (don,out) =
    if List.mem p don || List.mem a created_agents then don,out
    else
      match lnk_dst p with
      | None -> p::don, Freed p::out
      | Some p' -> p::p'::don, Linked (p,p')::out

  let agents_created_by_action = function
    | Instantiation.Create ((ag_id, _), _) -> Some ag_id
    | Instantiation.Bind _ | Instantiation.Free _ | Instantiation.Bind_to _
    | Instantiation.Remove _ | Instantiation.Mod_internal _ -> None

  let agents_created_by_actions actions =
    List_util.map_option agents_created_by_action actions

  let negative_transformations_of_actions sigs lnk_dst actions =
    let created_agents = agents_created_by_actions actions in
    snd
      (List.fold_right
         (fun x (don,out as acc) -> match x with
            | Instantiation.Create (_,_) -> acc
            | Instantiation.Mod_internal (((id, _), _ as p),_) ->
              if List.mem id created_agents then don, out else
              don, NegativeInternalized p::out
            | Instantiation.Bind (p1,p2)
            | Instantiation.Bind_to (p1,p2) ->
              get_negative_part created_agents lnk_dst p1
                (get_negative_part created_agents lnk_dst p2 acc)
            | Instantiation.Free p ->
              get_negative_part created_agents lnk_dst p acc
            | Instantiation.Remove (_,ty as a) ->
              Tools.recti
                (fun st s -> get_negative_part created_agents lnk_dst (a,s) st)
                (don,Agent a::out) (Signature.arity sigs ty)
         )
         actions ([],[]))

  let positive_transformations_of_actions sigs side_effect_dsts actions =
    let rem,rev =
      List.fold_left
        (fun (rem,out as acc) -> function
           | Instantiation.Create ((_,ty as a),_) ->
             Tools.recti (fun st s -> (a,s)::st)
               rem (Signature.arity sigs ty), Agent a::out
           | Instantiation.Mod_internal ((a,s),i) ->
             rem, PositiveInternalized (a,s,i)::out
           | Instantiation.Bind (p1,p2)
           | Instantiation.Bind_to (p1,p2) ->
             List.filter (fun p -> p <> p1 && p <> p2) rem,
             Linked (p1,p2)::out
           | Instantiation.Free p ->
             List.filter (fun p' -> p' <> p) rem, Freed p::out
           | Instantiation.Remove _ -> acc
        )
        ([],[]) actions in
    List.rev_append rev
      (List_util.rev_map_append (fun p -> Freed p) rem
         (List.map (fun p -> Freed p) side_effect_dsts))
end

type alg_expr = (Pattern.id array list, int) Alg_expr.e

type elementary_rule = {
  rate : alg_expr Locality.annot;
  unary_rate : (alg_expr Locality.annot * alg_expr option) option;
  connected_components : Pattern.id array; (*id -> cc*)
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  delta_tokens : (alg_expr Locality.annot * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
}

let extract_cc_ids r = r.connected_components

let extract_abstract_event r = r.instantiations

let alg_expr_to_yojson ~filenames =
  Alg_expr.e_to_yojson
    ~filenames
    (JsonUtil.of_list (JsonUtil.of_array Pattern.id_to_yojson))
    JsonUtil.of_int

let alg_expr_of_yojson ~filenames =
  Alg_expr.e_of_yojson
    ~filenames
    (JsonUtil.to_list (JsonUtil.to_array Pattern.id_of_yojson))
    (JsonUtil.to_int ?error_msg:None)

let rule_to_yojson ~filenames r =
  JsonUtil.smart_assoc [
    "rate", Locality.annot_to_yojson ~filenames
      (alg_expr_to_yojson ~filenames) r.rate;
     "unary_rate",
     JsonUtil.of_option
       (JsonUtil.of_pair
          (Locality.annot_to_yojson ~filenames (alg_expr_to_yojson ~filenames))
          (JsonUtil.of_option (alg_expr_to_yojson ~filenames)))
       r.unary_rate;
      "connected_components",
      JsonUtil.of_array Pattern.id_to_yojson r.connected_components;
      "removed", JsonUtil.of_list Transformation.to_yojson r.removed;
      "inserted", JsonUtil.of_list Transformation.to_yojson r.inserted;
      "delta_tokens",
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
           (Locality.annot_to_yojson ~filenames
              (alg_expr_to_yojson ~filenames))
           JsonUtil.of_int)
        r.delta_tokens;
      "syntactic_rule", `Int r.syntactic_rule;
      "instantiations",
      Instantiation.event_to_json Matching.Agent.to_yojson r.instantiations;
   ]

let rule_of_yojson ~filenames r =
  match r with
  | ((`Assoc l):Yojson.Basic.json) as x ->
     begin
       try {
         rate = Locality.annot_of_yojson ~filenames
             (alg_expr_of_yojson ~filenames) (List.assoc "rate" l);
         unary_rate =
           JsonUtil.to_option
             (JsonUtil.to_pair
                (Locality.annot_of_yojson ~filenames
                   (alg_expr_of_yojson ~filenames))
                (JsonUtil.to_option (alg_expr_of_yojson ~filenames)))
             (Yojson.Basic.Util.member "unary_rate" x);
         connected_components =
           JsonUtil.to_array Pattern.id_of_yojson
             (Yojson.Basic.Util.member "connected_components" x);
         removed =
           JsonUtil.to_list Transformation.of_yojson
             (Yojson.Basic.Util.member "removed" x);
         inserted =
           JsonUtil.to_list Transformation.of_yojson
             (Yojson.Basic.Util.member "inserted" x);
         delta_tokens =
           JsonUtil.to_list
             (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                (Locality.annot_of_yojson ~filenames
                   (alg_expr_of_yojson ~filenames))
                (JsonUtil.to_int ?error_msg:None))
             (Yojson.Basic.Util.member "delta_tokens" x);
           syntactic_rule = JsonUtil.to_int (List.assoc "syntactic_rule" l);
           instantiations =
             Instantiation.event_of_json Matching.Agent.of_yojson
               (List.assoc "instantiations" l);
       }
       with Not_found ->
         raise (Yojson.Basic.Util.Type_error ("Not a correct elementary rule",x))
     end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct elementary rule",x))

type 'alg_expr print_expr =
    Str_pexpr of string Locality.annot
  | Alg_pexpr of 'alg_expr Locality.annot

let print_expr_to_yojson ~filenames f_mix f_var = function
  | Str_pexpr s -> Locality.annot_to_yojson ~filenames JsonUtil.of_string s
  | Alg_pexpr a ->
    Locality.annot_to_yojson ~filenames
      (Alg_expr.e_to_yojson ~filenames f_mix f_var) a

let print_expr_of_yojson ~filenames f_mix f_var x =
  try Str_pexpr (Locality.annot_of_yojson
                   ~filenames (JsonUtil.to_string ?error_msg:None) x)
  with Yojson.Basic.Util.Type_error _ ->
  try Alg_pexpr (Locality.annot_of_yojson
                   ~filenames (Alg_expr.e_of_yojson ~filenames f_mix f_var) x)
  with Yojson.Basic.Util.Type_error _ ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect print expr",x))

let map_expr_print f x =
  List.map (function
      | Str_pexpr _ as x -> x
      | Alg_pexpr e -> Alg_pexpr (f e)) x

type flux_kind = ABSOLUTE | RELATIVE | PROBABILITY

let flux_kind_to_yojson = function
  | ABSOLUTE -> `String "ABSOLUTE"
  | RELATIVE -> `String "RELATIVE"
  | PROBABILITY -> `String "PROBABILITY"

let flux_kind_of_yojson = function
  | `String "ABSOLUTE" -> ABSOLUTE
  | `String "RELATIVE" -> RELATIVE
  | `String "PROBABILITY" -> PROBABILITY
  | x -> raise
           (Yojson.Basic.Util.Type_error ("Incorrect flux_kind",x))

let write_flux_kind ob f =
  Yojson.Basic.to_outbuf ob (flux_kind_to_yojson f)

let string_of_flux_kind ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_flux_kind ob x;
  Bi_outbuf.contents ob

let read_flux_kind p lb =
  flux_kind_of_yojson (Yojson.Basic.from_lexbuf ~stream:true p lb)

let flux_kind_of_string s =
  read_flux_kind (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

type modification =
    ITER_RULE of alg_expr Locality.annot * elementary_rule
  | UPDATE of int * alg_expr Locality.annot
  | SNAPSHOT of alg_expr print_expr list
  | STOP of alg_expr print_expr list
  | CFLOW of string option * Pattern.id array *
             Instantiation.abstract Instantiation.test list list
  | FLUX of flux_kind * alg_expr print_expr list
  | FLUXOFF of alg_expr print_expr list
  | CFLOWOFF of string option * Pattern.id array
  | PLOTENTRY
  | PRINT of alg_expr print_expr list * alg_expr print_expr list
  | SPECIES of alg_expr print_expr list * Pattern.id array *
             Instantiation.abstract Instantiation.test list list
  | SPECIES_OFF of alg_expr print_expr list

let print_t_expr_to_yojson ~filenames =
  print_expr_to_yojson
    ~filenames
    (JsonUtil.of_list (JsonUtil.of_array Pattern.id_to_yojson))
    JsonUtil.of_int

let print_t_expr_of_yojson ~filenames =
  print_expr_of_yojson
    ~filenames
    (JsonUtil.to_list (JsonUtil.to_array Pattern.id_of_yojson))
    (JsonUtil.to_int ?error_msg:None)

let modification_to_yojson ~filenames = function
  | ITER_RULE(n,r) ->
    `Assoc [ "action", `String "ITER";
             "repeats", Locality.annot_to_yojson ~filenames
               (alg_expr_to_yojson ~filenames) n;
             "rule", rule_to_yojson ~filenames r ]
  | UPDATE(v,e) ->
    `Assoc [ "action", `String "UPDATE";
             "var", `Int v;
             "value", Locality.annot_to_yojson ~filenames
               (alg_expr_to_yojson ~filenames) e ]
  | SNAPSHOT f ->
    JsonUtil.smart_assoc [
      "action", `String "SNAPSHOT";
      "file", JsonUtil.of_list (print_t_expr_to_yojson ~filenames) f ]
  | STOP f ->
    JsonUtil.smart_assoc [
      "action", `String "STOP";
      "file", JsonUtil.of_list (print_t_expr_to_yojson ~filenames) f ]
  | CFLOW (name,ids,tests) ->
    JsonUtil.smart_assoc [
      "action", `String "CFLOW";
      "name", JsonUtil.of_option JsonUtil.of_string name;
      "pattern", JsonUtil.of_array Pattern.id_to_yojson ids;
      "tests",
      JsonUtil.of_list
        (JsonUtil.of_list
           (Instantiation.test_to_json Matching.Agent.to_yojson))
        tests ]
  | CFLOWOFF (name,ids) ->
    `Assoc [ "action", `String "CFLOWOFF";
             "name", JsonUtil.of_option JsonUtil.of_string name;
             "pattern", JsonUtil.of_array Pattern.id_to_yojson ids ]
  | FLUX(kind,f) ->
    `Assoc [ "action", `String "FLUX";
             "kind", flux_kind_to_yojson kind;
             "file", `List (List.map (print_t_expr_to_yojson ~filenames) f) ]
  | FLUXOFF f ->
    JsonUtil.smart_assoc [
      "action", `String "FLUXOFF";
      "file", JsonUtil.of_list (print_t_expr_to_yojson ~filenames) f ]
  | PLOTENTRY -> `Assoc [ "action", `String "PLOTNOW" ]
  | PRINT(f,t) ->
    `Assoc [ "action", `String "PRINT";
             "text", `List (List.map (print_t_expr_to_yojson ~filenames) t);
             "file", `List (List.map (print_t_expr_to_yojson ~filenames) f) ]
  | SPECIES (f,ids,tests) ->
     JsonUtil.smart_assoc [
      "action", `String "SPECIES";
      "file", `List (List.map (print_t_expr_to_yojson ~filenames) f);
      "pattern", JsonUtil.of_array Pattern.id_to_yojson ids;
      "tests",
      JsonUtil.of_list
        (JsonUtil.of_list
           (Instantiation.test_to_json Matching.Agent.to_yojson))
        tests ]
  | SPECIES_OFF f ->
    `Assoc [ "action", `String "SPECIES_OFF";
             "file", `List (List.map (print_t_expr_to_yojson ~filenames) f)]


let modification_of_yojson ~filenames = function
  | `Assoc [ "action", `String "PRINT"; "file", `List f; "text", `List t ]
  | `Assoc [ "text", `List t; "file", `List f; "action", `String "PRINT" ]
  | `Assoc [ "action", `String "PRINT"; "text", `List t; "file", `List f ]
  | `Assoc [ "text", `List t; "action", `String "PRINT"; "file", `List f ]
  | `Assoc [ "file", `List f; "action", `String "PRINT"; "text", `List t ]
  | `Assoc [ "file", `List f; "text", `List t; "action", `String "PRINT" ] ->
    PRINT(List.map (print_t_expr_of_yojson ~filenames) f,
          List.map (print_t_expr_of_yojson ~filenames) t)
  | `Assoc [ "action", `String "PRINT"; "file", `Null; "text", `List t ]
  | `Assoc [ "text", `List t; "file", `Null; "action", `String "PRINT" ]
  | `Assoc [ "action", `String "PRINT"; "text", `List t; "file", `Null ]
  | `Assoc [ "text", `List t; "action", `String "PRINT"; "file", `Null ]
  | `Assoc [ "file", `Null; "action", `String "PRINT"; "text", `List t ]
  | `Assoc [ "file", `Null; "text", `List t; "action", `String "PRINT" ]
  | `Assoc [ "action", `String "PRINT"; "text", `List t ]
  | `Assoc [ "text", `List t; "action", `String "PRINT" ] ->
    PRINT([], List.map (print_t_expr_of_yojson ~filenames) t)
  | `Assoc [ "action", `String "FLUX"; "file", `List f; "kind", kind ]
  | `Assoc [ "kind", kind; "file", `List f; "action", `String "FLUX" ]
  | `Assoc [ "action", `String "FLUX"; "kind", kind; "file", `List f ]
  | `Assoc [ "kind", kind; "action", `String "FLUX"; "file", `List f ]
  | `Assoc [ "file", `List f; "action", `String "FLUX"; "kind", kind ]
  | `Assoc [ "file", `List f; "kind", kind; "action", `String "FLUX" ] ->
    FLUX(flux_kind_of_yojson kind,
         List.map (print_t_expr_of_yojson ~filenames) f)
  | `Assoc [ "action", `String "UPDATE"; "var", `Int v; "value", e ]
  | `Assoc [ "var", `Int v; "action", `String "UPDATE"; "value", e ]
  | `Assoc [ "action", `String "UPDATE"; "value", e; "var", `Int v ]
  | `Assoc [ "var", `Int v; "value", e; "action", `String "UPDATE" ]
  | `Assoc [ "value", e; "action", `String "UPDATE"; "var", `Int v ]
  | `Assoc [ "value", e; "var", `Int v; "action", `String "UPDATE" ] ->
    UPDATE(v,Locality.annot_of_yojson ~filenames
             (alg_expr_of_yojson ~filenames) e)
  | `Assoc [ "action", `String "ITER"; "repeats", n; "rule", r ]
  | `Assoc [ "action", `String "ITER"; "rule", r; "repeats", n ]
  | `Assoc [ "repeats", n; "action", `String "ITER"; "rule", r ]
  | `Assoc [ "rule", r; "action", `String "ITER"; "repeats", n ]
  | `Assoc [ "repeats", n; "rule", r; "action", `String "ITER" ]
  | `Assoc [ "rule", r; "repeats", n; "action", `String "ITER" ] ->
    ITER_RULE (Locality.annot_of_yojson ~filenames
                 (alg_expr_of_yojson ~filenames) n,
               rule_of_yojson ~filenames r)
  | `Assoc [ "action", `String "PLOTNOW" ] -> PLOTENTRY
  | `Assoc [ "action", `String "FLUXOFF"; "file", `List l ]
  | `Assoc [ "file", `List l; "action", `String "FLUXOFF" ] ->
    FLUXOFF (List.map (print_t_expr_of_yojson ~filenames) l)
  | `Assoc [ "action", `String "FLUXOFF"; "file", `Null ]
  | `Assoc [ "file", `Null; "action", `String "FLUXOFF" ]
  | `Assoc [ "action", `String "FLUXOFF" ] -> FLUXOFF []
  | `Assoc [ "action", `String "SNAPSHOT"; "file", `List l ]
  | `Assoc [ "file", `List l; "action", `String "SNAPSHOT" ] ->
    SNAPSHOT (List.map (print_t_expr_of_yojson ~filenames) l)
  | `Assoc [ "action", `String "SNAPSHOT"; "file", `Null ]
  | `Assoc [ "file", `Null; "action", `String "SNAPSHOT" ]
  | `Assoc [ "action", `String "SNAPSHOT" ] -> SNAPSHOT []
  | `Assoc [ "action", `String "STOP"; "file", `List l ]
  | `Assoc [ "file", `List l; "action", `String "STOP" ] ->
    STOP (List.map (print_t_expr_of_yojson ~filenames) l)
  | `Assoc [ "action", `String "STOP"; "file", `Null ]
  | `Assoc [ "file", `Null; "action", `String "STOP" ]
  | `Assoc [ "action", `String "STOP" ] -> STOP []
  | `Assoc _ as l when Yojson.Basic.Util.member "action" l = `String "CFLOW" ->
    CFLOW (
      JsonUtil.to_option (JsonUtil.to_string ?error_msg:None)
        (Yojson.Basic.Util.member "name" l),
      JsonUtil.to_array Pattern.id_of_yojson
        (Yojson.Basic.Util.member "pattern" l),
      JsonUtil.to_list (JsonUtil.to_list
                          (Instantiation.test_of_json Matching.Agent.of_yojson))
        (Yojson.Basic.Util.member "tests" l))
  | `Assoc _ as l when Yojson.Basic.Util.member "action" l = `String "CFLOWOFF" ->
    CFLOWOFF(
      JsonUtil.to_option (JsonUtil.to_string ?error_msg:None)
        (Yojson.Basic.Util.member "name" l),
      JsonUtil.to_array Pattern.id_of_yojson
        (Yojson.Basic.Util.member "pattern" l))
  | `Assoc [ "action", `String "SPECIES_OFF"; "file", p ]
  | `Assoc [ "file", p; "action", `String "SPECIES_OFF" ] ->
    SPECIES_OFF(JsonUtil.to_list (print_t_expr_of_yojson ~filenames) p)
  | `Assoc _ as l when Yojson.Basic.Util.member "action" l = `String "SPECIES" ->
    SPECIES (
       JsonUtil.to_list (print_t_expr_of_yojson ~filenames)
        (Yojson.Basic.Util.member "file" l),
      JsonUtil.to_array Pattern.id_of_yojson
        (Yojson.Basic.Util.member "pattern" l),
      JsonUtil.to_list (JsonUtil.to_list
                          (Instantiation.test_of_json Matching.Agent.of_yojson))
        (Yojson.Basic.Util.member "tests" l))
  | x -> raise
           (Yojson.Basic.Util.Type_error ("Invalid modification",x))

type perturbation =
  { alarm: Nbr.t option;
    precondition: (Pattern.id array list,int) Alg_expr.bool Locality.annot;
    effect : modification list;
    repeat : (Pattern.id array list,int) Alg_expr.bool Locality.annot;
    needs_backtrack : bool;
  }

let bool_expr_to_yojson ~filenames =
  Alg_expr.bool_to_yojson
    ~filenames
    (JsonUtil.of_list (JsonUtil.of_array Pattern.id_to_yojson))
    JsonUtil.of_int

let bool_expr_of_yojson ~filenames =
  Alg_expr.bool_of_yojson
    ~filenames
    (JsonUtil.to_list (JsonUtil.to_array Pattern.id_of_yojson))
    (JsonUtil.to_int ?error_msg:None)

let perturbation_to_yojson ~filenames p =
  `Assoc [
    "alarm", JsonUtil.of_option (fun n -> Nbr.to_yojson n) p.alarm;
    "condition", Locality.annot_to_yojson ~filenames
      (bool_expr_to_yojson ~filenames) p.precondition;
    "effect", JsonUtil.of_list (modification_to_yojson ~filenames) p.effect;
    "repeat", Locality.annot_to_yojson ~filenames
      (bool_expr_to_yojson ~filenames) p.repeat;
    "needs_backtrack", `Bool p.needs_backtrack ]

let perturbation_of_yojson ~filenames = function
  | `Assoc l as x when List.length l = 5 ->
     begin
       try {
           alarm = JsonUtil.to_option Nbr.of_yojson (List.assoc "alarm" l);
           precondition =
             Locality.annot_of_yojson ~filenames
               (bool_expr_of_yojson ~filenames) (List.assoc "condition" l);
           effect =
             JsonUtil.to_list
               (modification_of_yojson ~filenames) (List.assoc "effect" l);
           repeat =
             Locality.annot_of_yojson ~filenames
               (bool_expr_of_yojson ~filenames) (List.assoc "repeat" l);
           needs_backtrack =
             (function
              | `Bool b -> b
              | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect bool",x)))
               (List.assoc "needs_backtrack" l);
         }
       with Not_found ->
         raise (Yojson.Basic.Util.Type_error ("Invalid perturbation",x))
     end
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid perturbation",x))

let exists_modification check l =
  Array.fold_left (fun acc p -> acc || List.exists check p.effect) false l

let extract_connected_components_expr acc e =
  List.fold_left
    (List.fold_left (fun acc a -> List.rev_append (Array.to_list a) acc))
    acc (Alg_expr.extract_connected_components e)

let extract_connected_components_bool e =
  List.fold_left
    (List.fold_left (fun acc a -> List.rev_append (Array.to_list a) acc))
    [] (Alg_expr.extract_connected_components_bool e)

let extract_connected_components_rule acc r =
  let a =
    List.fold_left
      (fun acc (x,_) -> extract_connected_components_expr acc x)
      acc r.delta_tokens in
  let b = match r.unary_rate with
    | None -> a
    | Some (x,_) -> extract_connected_components_expr a x in
  let c = extract_connected_components_expr b r.rate in
  List.rev_append (Array.to_list r.connected_components) c

let extract_connected_components_print acc x =
  List.fold_left (fun acc -> function
      | Str_pexpr _ -> acc
      | Alg_pexpr e -> extract_connected_components_expr acc e)
    acc x

let extract_connected_components_modification acc = function
  | ITER_RULE (e,r) ->
    extract_connected_components_rule
      (extract_connected_components_expr acc e) r
  | UPDATE (_,e) -> extract_connected_components_expr acc e
  | SNAPSHOT p | STOP p | SPECIES_OFF p
  | FLUX (_,p) | FLUXOFF p -> extract_connected_components_print acc p
  | PRINT (fn,p) ->
    extract_connected_components_print
      (extract_connected_components_print acc p) fn
  | CFLOW (_,x,_) | CFLOWOFF (_,x) | SPECIES (_,x,_) ->
     List.rev_append (Array.to_list x) acc
  | PLOTENTRY -> acc

let extract_connected_components_modifications l =
  List.fold_left extract_connected_components_modification [] l

let map_expr_rule f x = {
  rate = f x.rate;
  unary_rate = Option_util.map (fun (x,d) -> (f x,d)) x.unary_rate;
  connected_components = x.connected_components;
  removed = x.removed;
  inserted = x.inserted;
  delta_tokens = List.map (fun (x,t) -> (f x,t)) x.delta_tokens;
  syntactic_rule = x.syntactic_rule;
  instantiations = x.instantiations;
}

let map_expr_modification f = function
  | ITER_RULE (e,r) -> ITER_RULE (f e, map_expr_rule f r)
  | UPDATE (i,e) -> UPDATE (i,f e)
  | SNAPSHOT p -> SNAPSHOT (map_expr_print f p)
  | STOP p -> STOP (map_expr_print f p)
  | PRINT (fn,p) -> PRINT (map_expr_print f fn, map_expr_print f p)
  | FLUX (b,p) -> FLUX (b,map_expr_print f p)
  | FLUXOFF p -> FLUXOFF (map_expr_print f p)
  | (CFLOW _ | CFLOWOFF _ | SPECIES_OFF _ | PLOTENTRY) as x -> x
  | SPECIES (p,x,t) -> SPECIES ((map_expr_print f p),x,t)

let map_expr_perturbation f_alg f_bool x =
  { alarm = x.alarm;
    precondition = f_bool x.precondition;
    effect = List.map (map_expr_modification f_alg) x.effect;
    repeat = f_bool x.repeat;
    needs_backtrack = x.needs_backtrack;
  }

let stops_of_perturbation algs_deps x =
  let stopping_time = match x.alarm with
    | Some n -> [(Some n,n)]
    | None ->
       let () =
         if (Alg_expr.is_equality_test_time algs_deps (fst x.precondition)) then
           raise
             (ExceptionDefn.Malformed_Decl
                ("Equality test on time requires an alarm",(snd x.precondition)))
       in
       let () =
         if (Alg_expr.is_equality_test_time algs_deps (fst x.repeat)) then
           raise
             (ExceptionDefn.Malformed_Decl
                ("Equality test on time requires an alarm",(snd x.repeat))) in
       [] in
  stopping_time
