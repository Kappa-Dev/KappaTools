(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type syntax_version = V3 | V4

let merge_version a b =
  match a,b with
  | V4, _ | _, V4 -> V4
  | V3, V3 -> V3

type ('a,'annot) link =
  | ANY_FREE
  | LNK_VALUE of int * 'annot
  | LNK_FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a (* port *)
    * 'a (*agent_type*)

type internal = string Locality.annot list

type port = {
  port_nme:string Locality.annot;
  port_int:internal;
  port_int_mod: string Locality.annot option;
  port_lnk:(string Locality.annot,unit) link Locality.annot list;
  port_lnk_mod: int Locality.annot option option;
}

type counter_test = CEQ of int | CGTE of int | CVAR of string

type counter = {
  count_nme: string Locality.annot;
  count_test: counter_test Locality.annot option;
  count_delta: int Locality.annot;
}

type site =
  | Port of port
  | Counter of counter

type agent_mod = Erase | Create

type agent = (string Locality.annot * site list * agent_mod option)

type mixture = agent list

type edit_rule = {
  mix: mixture;
  delta_token: ((mixture,string) Alg_expr.e Locality.annot
                * string Locality.annot) list;
  act: (mixture,string) Alg_expr.e Locality.annot;
  un_act:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
}

type rule = {
  lhs: mixture ;
  rm_token: ((mixture,string) Alg_expr.e Locality.annot
             * string Locality.annot) list ;
  bidirectional:bool ;
  rhs: mixture ;
  add_token: ((mixture,string) Alg_expr.e Locality.annot
              * string Locality.annot) list;
  k_def: (mixture,string) Alg_expr.e Locality.annot ;
  k_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*k_1:radius_opt*)
  k_op: (mixture,string) Alg_expr.e Locality.annot option ;
  k_op_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*rate for backward rule*)
}

let flip_label str = str^"_op"

type ('mixture,'id) modif_expr =
  | INTRO of
      (('mixture,'id) Alg_expr.e Locality.annot * 'mixture Locality.annot)
  | DELETE of
      (('mixture,'id) Alg_expr.e Locality.annot * 'mixture Locality.annot)
  | UPDATE of
      ('id Locality.annot *
       ('mixture,'id) Alg_expr.e Locality.annot)
  | UPDATE_TOK of
      ('id Locality.annot *
       ('mixture,'id) Alg_expr.e Locality.annot)
  | STOP of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | SNAPSHOT of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | PRINT of
      (('mixture,'id) Alg_expr.e Primitives.print_expr list) *
       (('mixture,'id)  Alg_expr.e Primitives.print_expr list)
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Locality.annot)
  | CFLOWMIX of (bool * 'mixture Locality.annot)
  | FLUX of
      Primitives.flux_kind * ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | FLUXOFF of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | SPECIES_OF of
      (bool * ('mixture,'id) Alg_expr.e Primitives.print_expr list
       * 'mixture Locality.annot)

type ('mixture,'id) perturbation =
  (Nbr.t option *
   ('mixture,'id) Alg_expr.bool Locality.annot option *
   (('mixture,'id) modif_expr list) *
   ('mixture,'id) Alg_expr.bool Locality.annot option) Locality.annot

type configuration = string Locality.annot * (string Locality.annot list)

type ('mixture,'id) variable_def =
  string Locality.annot * ('mixture,'id) Alg_expr.e Locality.annot

type ('mixture,'id) init_t =
  | INIT_MIX of 'mixture
  | INIT_TOK of 'id

type ('mixture,'id) init_statment =
  string Locality.annot option *
  ('mixture,'id) Alg_expr.e Locality.annot *
  ('mixture,'id) init_t Locality.annot

type ('mixture,'id) instruction =
  | SIG      of agent
  | TOKENSIG of string Locality.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of
      (string Locality.annot option *
      ('mixture,'id) Alg_expr.e Locality.annot *
      ('mixture,'id) init_t Locality.annot)
  (*volume, init, position *)
  | DECLARE  of ('mixture,'id) variable_def
  | OBS      of ('mixture,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('mixture,'id) Alg_expr.e Locality.annot
  | PERT     of ('mixture,'id) perturbation
  | CONFIG   of configuration

type ('mixture,'id) command =
  | RUN of ('mixture,'id) Alg_expr.bool Locality.annot
  | MODIFY of ('mixture,'id) modif_expr list
  | QUIT

type ('agent,'mixture,'id,'rule,'edit_rule) compil =
  {
    variables :
      ('mixture,'id) variable_def list;
    (*pattern declaration for reusing as variable in perturbations
      or kinetic rate*)
    signatures :
      'agent list; (*agent signature declaration*)
    rules :
      (string Locality.annot option * 'rule Locality.annot) list;
    (*rules (possibly named)*)
    edit_rules :
      (string Locality.annot option * 'edit_rule) list;
    observables :
      ('mixture,'id) Alg_expr.e Locality.annot list;
    (*list of patterns to plot*)
    init :
      (string Locality.annot option *
       ('mixture,'id) Alg_expr.e Locality.annot *
       ('mixture,'id) init_t Locality.annot) list;
    (*initial graph declaration*)
    perturbations :
      ('mixture,'id) perturbation list;
    configurations :
      configuration list;
    tokens :
      string Locality.annot list;
    volumes :
      (string * float * string) list
  }

type parsing_compil = (agent,mixture,string,rule,edit_rule) compil

let no_more_site_on_right error left right =
  List.for_all
    (function
      | Counter _ -> true
      | Port p ->
        List.exists (function
            | Counter _ -> false
            | Port p' -> fst p.port_nme = fst p'.port_nme) left
       || let () =
            if error then
              raise (ExceptionDefn.Malformed_Decl
                       ("Site '"^fst p.port_nme^
                        "' was not mentionned in the left-hand side.",
                        snd p.port_nme))
       in false)
    right

let empty_compil =
  {
    variables      = [];
    signatures     = [];
    rules          = [];
    edit_rules     = [];
    init           = [];
    observables    = [];
    perturbations  = [];
    configurations = [];
    tokens         = [];
    volumes        = []
  }

(*
  let reverse res =
  let l_pat = List.rev !res.patterns
  and l_sig = List.rev !res.signatures
  and l_rul = List.rev !res.rules
  and l_ini = List.rev !res.init
  and l_obs = List.rev !res.observables
  in
  res:={patterns=l_pat ; signatures=l_sig ;
        rules=l_rul ; init = l_ini ; observables = l_obs}
*)

let print_link ~syntax_version pr_port pr_type pr_annot f = function
  | ANY_FREE -> if syntax_version = V3 then Format.fprintf f "?"
  | LNK_TYPE (p, a) -> Format.fprintf f "!%a.%a" (pr_port a) p pr_type a
  | LNK_ANY -> Format.fprintf f "?"
  | LNK_FREE -> if syntax_version = V4 then Format.fprintf f "!."
  | LNK_SOME -> Format.fprintf f "!_"
  | LNK_VALUE (i,a) -> Format.fprintf f "!%i%a" i pr_annot a

let link_to_json port_to_json type_to_json annot_to_json = function
  | ANY_FREE -> `String "ANY_FREE"
  | LNK_FREE -> `String "FREE"
  | LNK_TYPE (p, a) -> `List [port_to_json a p; type_to_json a]
  | LNK_ANY -> `Null
  | LNK_SOME -> `String "SOME"
  | LNK_VALUE (i,a) -> `List (`Int i :: annot_to_json a)

let link_of_json port_of_json type_of_json annot_of_json = function
  | `String "ANY_FREE" -> ANY_FREE
  | `String "FREE" -> LNK_FREE
  | `List [p; a] -> let x = type_of_json a in LNK_TYPE (port_of_json x p, x)
  | `Null -> LNK_ANY
  | `String "SOME" -> LNK_SOME
  | `List (`Int i :: ( [] | _::_::_ as a)) -> LNK_VALUE (i,annot_of_json a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect link",x))

let print_ast_link =
  print_link
    (fun _ f (x,_) -> Format.pp_print_string f x)
    (fun f (x,_) -> Format.pp_print_string f x)
    (fun _ () -> ())
let print_ast_internal =
  Pp.list Pp.empty (fun f (x,_) -> Format.fprintf f "~%s" x)

let print_ast_port f p =
  let f_mod_i = Pp.option ~with_space:false
      (fun f (i,_) -> Format.fprintf f "/~%s" i) in
  let f_mod_l = Pp.option ~with_space:false
      (fun f x -> Format.fprintf f "/%a"
          (Pp.option ~with_space:false (fun f (l,_)-> Format.fprintf f "!%i" l))
          x) in
  Format.fprintf f "%s%a%a%a%a" (fst p.port_nme)
    print_ast_internal p.port_int f_mod_i p.port_int_mod
    (Pp.list Pp.empty (fun f (x,_) -> print_ast_link ~syntax_version:V4 f x))
    p.port_lnk
    f_mod_l p.port_lnk_mod

let print_counter_test f = function
  | CEQ x, _ -> Format.fprintf f "=%i" x
  | CGTE x, _ -> Format.fprintf f ">=%i" x
  | CVAR x, _ ->  Format.fprintf f ":%s" x

let print_ast_site f = function
  | Port p -> print_ast_port f p
  | Counter c ->
    Format.fprintf f "%s%a+=%i" (fst c.count_nme)
      (Pp.option print_counter_test) c.count_test (fst c.count_delta)

let string_annot_to_json = Locality.annot_to_json JsonUtil.of_string
let string_annot_of_json =
  Locality.annot_of_json (JsonUtil.to_string ?error_msg:None)

let counter_test_to_json = function
  | CEQ x -> `Assoc [ "test", `String "eq"; "val", `Int x ]
  | CGTE x -> `Assoc [ "test", `String "gte"; "val", `Int x ]
  | CVAR x -> `Assoc [ "test", `String "eq"; "val", `String x ]
let counter_test_of_json = function
  | `Assoc [ "test", `String "eq"; "val", `Int x ]
  | `Assoc [ "val", `Int x; "test", `String "eq" ] -> CEQ x
  | `Assoc [ "val", `Int x; "test", `String "gte" ]
  | `Assoc [ "test", `String "gte"; "val", `Int x ] -> CGTE x
  | `Assoc [ "test", `String "eq"; "val", `String x ]
  | `Assoc [ "val", `String x; "test", `String "eq" ] -> CVAR x
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect counter test",x))

let port_to_json p =
  let mod_l = JsonUtil.of_option
      (JsonUtil.of_option (Locality.annot_to_json JsonUtil.of_int)) in
  let mod_i =JsonUtil.of_option
      (Locality.annot_to_json JsonUtil.of_string) in
  `Assoc [
    "port_nme", string_annot_to_json p.port_nme;
    "port_int", `Assoc [
      "state", JsonUtil.of_list string_annot_to_json p.port_int;
      "mod", mod_i p.port_int_mod];
    "port_lnk", `Assoc [
      "state", JsonUtil.of_list
        (Locality.annot_to_json
           (link_to_json
              (fun _ -> string_annot_to_json)
              string_annot_to_json (fun ()->[])))
        p.port_lnk;
      "mod", mod_l p.port_lnk_mod]
  ]
let site_of_json = function
  | `Assoc [ "count_nme", n; "count_test", t; "count_delta", d ] |
    `Assoc [ "count_nme", n; "count_delta", d; "count_test", t ] |
    `Assoc [ "count_test", t; "count_nme", n; "count_delta", d ] |
    `Assoc [ "count_test", t; "count_delta", d; "count_nme", n ] |
    `Assoc [ "count_delta", d; "count_nme", n; "count_test", t ] |
    `Assoc [ "count_delta", d; "count_test", t; "count_nme", n ] ->
    Counter {
      count_nme = Locality.annot_of_json Yojson.Basic.Util.to_string n;
      count_test = JsonUtil.to_option
          (Locality.annot_of_json counter_test_of_json) t;
      count_delta = Locality.annot_of_json Yojson.Basic.Util.to_int d;
    }
  | `Assoc [ "port_nme", n; "port_int", i; "port_lnk", l ] |
    `Assoc [ "port_nme", n; "port_lnk", l; "port_int", i ] |
    `Assoc [ "port_int", i; "port_nme", n; "port_lnk", l ] |
    `Assoc [ "port_lnk", l; "port_nme", n; "port_int", i ] |
    `Assoc [ "port_int", i; "port_lnk", l; "port_nme", n ] |
    `Assoc [ "port_lnk", l; "port_int", i; "port_nme", n ] ->
    let mod_l = JsonUtil.to_option
        (JsonUtil.to_option
           (Locality.annot_of_json (JsonUtil.to_int ?error_msg:None))) in
    let mod_i = JsonUtil.to_option
        (Locality.annot_of_json (JsonUtil.to_string ?error_msg:None)) in
    let port_int,port_int_mod =
      match i with
      | `Assoc [ "state", i; "mod", m ]
      | `Assoc [ "mod", m; "state", i ] ->
        (JsonUtil.to_list string_annot_of_json i, mod_i m)
      | _-> raise (Yojson.Basic.Util.Type_error ("Not internal states",i)) in
    let port_lnk,port_lnk_mod =
      match l with
      | `Assoc [ "state", l; "mod", m ]
      | `Assoc [ "mod", m; "state", l ] ->
        (JsonUtil.to_list
           (Locality.annot_of_json
              (link_of_json
                 (fun _ -> string_annot_of_json) string_annot_of_json
                 (fun _ -> ()))) l,mod_l m)
      | _ -> raise (Yojson.Basic.Util.Type_error ("Not link states",i)) in
    Port
      { port_nme = string_annot_of_json n;
        port_int; port_int_mod;
        port_lnk; port_lnk_mod;
      }
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent",x))

let site_to_json = function
  | Port p -> port_to_json p
  | Counter c ->
    `Assoc [
      "count_nme", Locality.annot_to_json JsonUtil.of_string c.count_nme;
      "count_test", JsonUtil.of_option
        (Locality.annot_to_json counter_test_to_json) c.count_test;
      "count_delta", Locality.annot_to_json JsonUtil.of_int c.count_delta
    ]

let print_agent_mod f = function
  | Create -> Format.pp_print_string f "+"
  | Erase -> Format.pp_print_string f "-"

let print_ast_agent f ((ag_na,_),l,m) =
  Format.fprintf f "%a%s(%a)"
    (Pp.option ~with_space:false print_agent_mod) m ag_na
    (Pp.list (fun f -> Format.fprintf f ",")
       print_ast_site) l

let agent_mod_to_yojson = function
  | Create -> `String "created"
  | Erase -> `String "erase"

let agent_mod_of_yojson = function
  | `String "created" -> Create
  | `String "erase" -> Erase
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect agent modification",x))

let agent_to_json (na,l,m) =
  `Assoc [ "name", Locality.annot_to_json JsonUtil.of_string na;
           "sig", JsonUtil.of_list site_to_json l;
           "mod", (JsonUtil.of_option agent_mod_to_yojson) m]

let agent_of_json = function
  | `Assoc [ "name", n; "sig", s; "mod", m ]
  | `Assoc [ "sig", s; "name", n; "mod", m ]
  | `Assoc [ "name", n; "mod", m; "sig", s ]
  | `Assoc [ "sig", s; "mod", m; "name", n ]
  | `Assoc [ "mod", m; "name", n; "sig", s ]
  | `Assoc [ "mod", m; "sig", s; "name", n ] ->
    (Locality.annot_of_json (JsonUtil.to_string ?error_msg:None) n,
    JsonUtil.to_list site_of_json s, (JsonUtil.to_option agent_mod_of_yojson) m)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent",x))

let print_ast_mix f m = Pp.list Pp.comma print_ast_agent f m

let init_to_json f_mix f_var = function
  | INIT_MIX m -> `List [`String "mixture"; f_mix m ]
  | INIT_TOK t -> `List [`String "token"; f_var t ]

let init_of_json f_mix f_var = function
  | `List [`String "mixture"; m ] -> INIT_MIX (f_mix m)
  | `List [`String "token"; t ] -> INIT_TOK (f_var t)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid Ast init statement",x))

let print_tok pr_mix pr_tok pr_var f ((nb,_),(n,_)) =
  Format.fprintf f "%a %a" (Alg_expr.print pr_mix pr_tok pr_var) nb pr_tok n
let print_one_size tk f mix =
  Format.fprintf
    f "%a%t%a" print_ast_mix mix
    (fun f -> match tk with [] -> () | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (print_tok
          (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
          Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x)))
    tk
let print_arrow f bidir =
  Format.pp_print_string f (if bidir then "<->" else "->")

let print_raw_rate pr_mix pr_tok pr_var op f (def,_) =
  Format.fprintf
    f "%a%t" (Alg_expr.print pr_mix pr_tok pr_var) def
    (fun f ->
       match op with
         None -> ()
       | Some (d,_) ->
         Format.fprintf f ", %a" (Alg_expr.print pr_mix pr_tok pr_var) d)
let print_rates un op f def =
  Format.fprintf
    f "%a%t"
    (print_raw_rate
       (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
       Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x) op)
    def
    (fun f ->
       match un with
         None -> ()
       | Some ((d,_),max_dist) ->
         Format.fprintf
           f " {%a:%a}"
           (Alg_expr.print
              (fun f m ->
                 Format.fprintf f "|%a|" print_ast_mix m)
              Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x)) d
           (Pp.option (fun f (md,_) ->
                        Format.fprintf f ":%a"
                        (Alg_expr.print
                           (fun f m ->
                              Format.fprintf f "|%a|" print_ast_mix m)
                           Format.pp_print_string
                           (fun f x -> Format.fprintf f "'%s'" x)) md))
           max_dist)

let print_ast_edit_rule f r =
  Format.fprintf f "@[<h>%a @@@ %a@]"
    print_ast_mix r.mix (print_rates r.un_act None) r.act

let print_ast_rule f r =
  Format.fprintf
    f "@[<h>%a %a@ %a @@ %a@]"
    (print_one_size r.rm_token) r.lhs
    print_arrow r.bidirectional
    (print_one_size r.add_token) r.rhs
    (print_rates r.k_un r.k_op) r.k_def
let print_ast_rule_no_rate ~reverse f r =
  Format.fprintf
    f "@[<h>%a -> %a@]"
    (print_one_size r.rm_token) (if reverse then r.rhs else r.lhs)
    (print_one_size r.add_token) (if reverse then r.lhs else r.rhs)

let rule_to_json f_mix f_var r =
  `Assoc [
    "lhs", f_mix r.lhs;
    "rm_token",
    JsonUtil.of_list
      (JsonUtil.of_pair
         (Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         string_annot_to_json)
      r.rm_token;
    "bidirectional", `Bool r.bidirectional;
    "rhs", f_mix r.rhs;
    "add_token",
    JsonUtil.of_list
      (JsonUtil.of_pair
         (Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         string_annot_to_json)
      r.add_token;
    "k_def", Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) r.k_def;
    "k_un",
    JsonUtil.of_option
      (JsonUtil.of_pair
         (Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         (JsonUtil.of_option (Locality.annot_to_json
                                (Alg_expr.e_to_yojson f_mix f_var))))
      r.k_un;
    "k_op",
    JsonUtil.of_option
      (Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var)) r.k_op;
    "k_op_un",
    JsonUtil.of_option
      (JsonUtil.of_pair
         (Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         (JsonUtil.of_option (Locality.annot_to_json
                                (Alg_expr.e_to_yojson f_mix f_var))))
      r.k_op_un;
  ]

let rule_of_json f_mix f_var = function
  | `Assoc l as x when List.length l <= 9 ->
    begin
      try
        {
          lhs = f_mix (List.assoc "lhs" l);
          rm_token =
            JsonUtil.to_list
              (JsonUtil.to_pair
                 (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 string_annot_of_json)
              (List.assoc "rm_token" l);
          bidirectional =
            Yojson.Basic.Util.to_bool (List.assoc "bidirectional" l);
          rhs = f_mix (List.assoc "rhs" l);
          add_token =
            JsonUtil.to_list
              (JsonUtil.to_pair
                 (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 string_annot_of_json)
              (List.assoc "add_token" l);
          k_def = Locality.annot_of_json
              (Alg_expr.e_of_yojson f_mix f_var) (List.assoc "k_def" l);
          k_un =
            JsonUtil.to_option
              (JsonUtil.to_pair
                 (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 (JsonUtil.to_option
                    (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))))
              (List.assoc "k_un" l);
          k_op = JsonUtil.to_option
              (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
              (List.assoc "k_op" l);
          k_op_un =
            JsonUtil.to_option
              (JsonUtil.to_pair
                 (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 (JsonUtil.to_option
                    (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))))
              (List.assoc "k_op_un" l);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule",x))

let edit_rule_to_yojson r =
  let mix_to_json = JsonUtil.of_list agent_to_json in
  JsonUtil.smart_assoc [
    "mix", JsonUtil.of_list agent_to_json r.mix;
    "delta_token",
    JsonUtil.of_list
      (JsonUtil.of_pair
         (Locality.annot_to_json
            (Alg_expr.e_to_yojson mix_to_json JsonUtil.of_string))
         string_annot_to_json) r.delta_token;
    "k_def", Locality.annot_to_json
      (Alg_expr.e_to_yojson mix_to_json JsonUtil.of_string) r.act;
    "k_un",
    JsonUtil.of_option
      (JsonUtil.of_pair
         (Locality.annot_to_json
            (Alg_expr.e_to_yojson mix_to_json JsonUtil.of_string))
         (JsonUtil.of_option (Locality.annot_to_json
                                (Alg_expr.e_to_yojson
                                   mix_to_json JsonUtil.of_string))))
      r.un_act;
  ]

let edit_rule_of_yojson r =
  let mix_of_json =
    JsonUtil.to_list agent_of_json in
  match r with
  | `Assoc l as x when List.length l < 5 ->
    begin try {
      mix = JsonUtil.to_list agent_of_json (List.assoc "mix" l);
      delta_token =JsonUtil.to_list
          (JsonUtil.to_pair
             (Locality.annot_of_json
                (Alg_expr.e_of_yojson
                   mix_of_json (JsonUtil.to_string ?error_msg:None)))
             string_annot_of_json)
          (Yojson.Basic.Util.member "delta_token" x);
      act = Locality.annot_of_json
          (Alg_expr.e_of_yojson
             mix_of_json (JsonUtil.to_string ?error_msg:None))
          (List.assoc "k_def" l);
      un_act =
        JsonUtil.to_option
          (JsonUtil.to_pair
             (Locality.annot_of_json
                (Alg_expr.e_of_yojson
                   mix_of_json (JsonUtil.to_string ?error_msg:None)))
             (JsonUtil.to_option (Locality.annot_of_json
                                    (Alg_expr.e_of_yojson
                                       mix_of_json
                                       (JsonUtil.to_string ?error_msg:None)))))
           (Yojson.Basic.Util.member "k_un" x);
    }
    with Not_found ->
      raise (Yojson.Basic.Util.Type_error ("Incorrect AST edit rule",x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect AST edit rule",x))

let modif_to_json f_mix f_var = function
  | INTRO (alg,mix) ->
    `List [ `String "INTRO";
            Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg;
            Locality.annot_to_json f_mix mix ]
  | DELETE (alg,mix) ->
    `List [ `String "DELETE";
            Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg;
            Locality.annot_to_json f_mix mix ]
  | UPDATE (id,alg) ->
    `List [ `String "UPDATE";
            Locality.annot_to_json f_var id;
            Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg ]
  | UPDATE_TOK (id,alg) ->
    `List [ `String "UPDATE_TOK";
            Locality.annot_to_json f_var id;
            Locality.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg ]
  | STOP l ->
    `List (`String "STOP" ::
           List.map (Primitives.print_expr_to_yojson f_mix f_var) l)
  | SNAPSHOT l ->
    `List (`String "SNAPSHOT" ::
           List.map (Primitives.print_expr_to_yojson f_mix f_var) l)
  | PRINT (file,expr) ->
    `List [ `String "PRINT";
            JsonUtil.of_list (Primitives.print_expr_to_yojson f_mix f_var) file;
            JsonUtil.of_list (Primitives.print_expr_to_yojson f_mix f_var) expr ]
  | PLOTENTRY -> `String "PLOTENTRY"
  | CFLOWLABEL (b,id) ->
    `List [ `String "CFLOWLABEL"; `Bool b; string_annot_to_json id ]
  | CFLOWMIX (b,m) ->
    `List [ `String "CFLOW"; `Bool b; Locality.annot_to_json f_mix m ]
  | FLUX (b,file) ->
    `List [ `String "FLUX"; Primitives.flux_kind_to_yojson b;
            JsonUtil.of_list (Primitives.print_expr_to_yojson f_mix f_var) file]
  | FLUXOFF file ->
    `List (`String "FLUXOFF" ::
           List.map (Primitives.print_expr_to_yojson f_mix f_var) file)
  | SPECIES_OF (b,l,m) ->
     `List [ `String "SPECIES_OF";
             `Bool b;
             JsonUtil.of_list (Primitives.print_expr_to_yojson f_mix f_var) l;
             Locality.annot_to_json f_mix m ]

let modif_of_json f_mix f_var = function
  | `List [ `String "INTRO"; alg; mix ] ->
     INTRO
       (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg,
        Locality.annot_of_json f_mix mix)
  | `List [ `String "DELETE"; alg; mix ] ->
    DELETE
      (Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg,
       Locality.annot_of_json f_mix mix)
  | `List [ `String "UPDATE"; id; alg ] ->
    UPDATE
      (Locality.annot_of_json f_var id,
       Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg)
  | `List [ `String "UPDATE_TOK"; id; alg ] ->
     UPDATE_TOK
       (Locality.annot_of_json f_var id,
        Locality.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg)
  | `List (`String "STOP" :: l) ->
    STOP (List.map (Primitives.print_expr_of_yojson f_mix f_var) l)
  | `List (`String "SNAPSHOT" :: l) ->
    SNAPSHOT (List.map (Primitives.print_expr_of_yojson f_mix f_var) l)
  | `List [ `String "PRINT"; file; expr ] ->
     PRINT
       (JsonUtil.to_list (Primitives.print_expr_of_yojson f_mix f_var) file,
        JsonUtil.to_list (Primitives.print_expr_of_yojson f_mix f_var) expr)
  | `String "PLOTENTRY" -> PLOTENTRY
  | `List [ `String "CFLOWLABEL"; `Bool b; id ] ->
     CFLOWLABEL (b, string_annot_of_json id)
  | `List [ `String "CFLOW"; `Bool b; m ] ->
     CFLOWMIX (b, Locality.annot_of_json f_mix m)
  | `List [ `String "FLUX"; b; file ] ->
    FLUX (Primitives.flux_kind_of_yojson b,
          JsonUtil.to_list (Primitives.print_expr_of_yojson f_mix f_var) file)
  | `List (`String "FLUXOFF" :: file) ->
     FLUXOFF (List.map (Primitives.print_expr_of_yojson f_mix f_var) file)
  | `List [ `String "SPECIES_OF"; `Bool b; file; m ] ->
     SPECIES_OF
       (b,
        JsonUtil.to_list (Primitives.print_expr_of_yojson f_mix f_var) file,
        Locality.annot_of_json f_mix m)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid modification",x))

let merge_internals =
  List.fold_left
    (fun acc (x,_ as y) ->
       if List.exists (fun (x',_) -> String.compare x x' = 0) acc
       then acc else y::acc)

let rec merge_sites_counter c = function
  | [] -> [Counter c]
  | Counter c' :: _ as l when fst c.count_nme = fst c'.count_nme -> l
  | (Port _ | Counter _ as h) :: t -> h :: merge_sites_counter c t
let rec merge_sites_port p = function
  | [] -> [Port {p with port_lnk = []}]
  | Port h :: t when fst p.port_nme = fst h.port_nme ->
    Port {h with port_int =
                   merge_internals h.port_int p.port_int}::t
  | (Port _ | Counter _ as h) :: t -> h :: merge_sites_port p t
let merge_sites =
  List.fold_left
    (fun acc -> function
       | Port p -> merge_sites_port p acc
       | Counter c -> merge_sites_counter c acc)

let merge_agents =
  List.fold_left
    (fun acc ((na,_ as x),s,_) ->
       let rec aux = function
         | [] -> [x,List.map
                    (function
                      | Port p -> Port {p with port_lnk = []}
                      | Counter _ as x -> x) s,None]
         | ((na',_),s',_) :: t when String.compare na na' = 0 ->
           (x,merge_sites s' s,None)::t
         | h :: t -> h :: aux t in
       aux acc)

let merge_tokens =
  List.fold_left
    (fun acc (_,(na,_ as tok)) ->
       let rec aux = function
         | [] -> [ tok ]
         | (na',_) :: _ as l when String.compare na na' = 0 -> l
         | h :: t as l ->
           let o = aux t in
           if t == o then l else h::o in
       aux acc)

let sig_from_inits =
  List.fold_left
    (fun (ags,toks) -> function
       | _,_,(INIT_MIX m,_) -> (merge_agents ags m,toks)
       | _,na,(INIT_TOK t,pos) -> (ags,merge_tokens toks [na,(t,pos)]))

let sig_from_edit_rules =
  List.fold_left
    (fun (ags,toks) (_,r) ->
       (merge_agents ags r.mix, merge_tokens toks r.delta_token))

let sig_from_rules =
  List.fold_left
    (fun (ags,toks) (_,(r,_)) ->
       let (ags',toks') =
         if r.bidirectional then
           (merge_agents ags r.rhs, merge_tokens toks r.add_token)
       else (ags,toks) in
       (merge_agents ags' r.lhs, merge_tokens toks' r.rm_token))

let sig_from_perts =
  List.fold_left
    (fun acc ((_,_,p,_),_) ->
       List.fold_left
         (fun (ags,toks) -> function
            | INTRO (_,(m,_)) ->
              (merge_agents ags m,toks)
            | UPDATE_TOK (t,na) ->
              (ags,merge_tokens toks [na,t])
            | (DELETE _ | UPDATE _ | STOP _ | SNAPSHOT _ | PRINT _ | PLOTENTRY |
               CFLOWLABEL _ | CFLOWMIX _ | FLUX _ | FLUXOFF _ | SPECIES_OF _) ->
               (ags,toks))
         acc p)

let implicit_signature r =
  let acc = sig_from_inits (r.signatures,r.tokens) r.init in
  let acc' = sig_from_rules acc r.rules in
  let acc'' = sig_from_edit_rules acc' r.edit_rules in
  let ags,toks = sig_from_perts acc'' r.perturbations in
  { r with signatures = ags; tokens = toks }

let split_mixture m =
    List.fold_right
      (fun  (na,intf,modif) (lhs,rhs,add,del) ->
         match modif with
         | Some Erase -> (lhs,rhs,add,(na,intf,None)::del)
         | Some Create -> (lhs,rhs,(na,intf,None)::add,del)
         | None ->
           let (intfl,intfr) =
             List.fold_left
               (fun (l,r) -> function
                  | Port p ->
                    (Port {port_nme = p.port_nme;
                           port_int = p.port_int;
                           port_int_mod = None;
                           port_lnk = p.port_lnk;
                           port_lnk_mod=None}::l,
                     Port {port_nme = p.port_nme;
                           port_int =
                             (match p.port_int_mod with
                              | None -> p.port_int
                              | Some x -> [x]);
                           port_int_mod = None;
                           port_lnk =
                             (match p.port_lnk_mod with
                              | None -> p.port_lnk
                              | Some None -> [Locality.dummy_annot LNK_FREE]
                              | Some (Some (i,pos)) -> [LNK_VALUE (i,()),pos]);
                           port_lnk_mod=None}::r)
                  | Counter _ -> (l,r)
               ) ([],[]) intf in
           ((na,intfl,None)::lhs,(na,intfr,None)::rhs,add,del)
      ) m ([],[],[],[])

let compil_to_json c =
  let mix_to_json = JsonUtil.of_list agent_to_json in
  let var_to_json = JsonUtil.of_string in
  `Assoc
    [
      "signatures",
      JsonUtil.of_list agent_to_json c.signatures;
      "tokens", JsonUtil.of_list string_annot_to_json c.tokens;
      "variables", JsonUtil.of_list
        (JsonUtil.of_pair
           string_annot_to_json
           (Locality.annot_to_json
              (Alg_expr.e_to_yojson mix_to_json var_to_json)))
        c.variables;
      "rules", JsonUtil.of_list
        (JsonUtil.of_pair
           (JsonUtil.of_option string_annot_to_json)
           (Locality.annot_to_json
              (rule_to_json mix_to_json var_to_json)))
        c.rules;
      "edit_rules", JsonUtil.of_list
        (JsonUtil.of_pair
           (JsonUtil.of_option string_annot_to_json)
           edit_rule_to_yojson)
        c.edit_rules;
      "observables",
      JsonUtil.of_list
        (Locality.annot_to_json (Alg_expr.e_to_yojson mix_to_json var_to_json))
        c.observables;
      "init",
      JsonUtil.of_list
        (JsonUtil.of_pair
           (Locality.annot_to_json
              (Alg_expr.e_to_yojson mix_to_json var_to_json))
           (Locality.annot_to_json (init_to_json mix_to_json var_to_json)))
        (List.map (fun (_,a,i) -> (a,i)) c.init);
      "perturbations", JsonUtil.of_list
        (Locality.annot_to_json
           (fun (alarm,pre,modif,post) ->
              `List [
                 JsonUtil.of_option Nbr.to_yojson alarm;
                 JsonUtil.of_option
                   (Locality.annot_to_json
                      (Alg_expr.bool_to_yojson mix_to_json var_to_json)) pre;
                JsonUtil.of_list (modif_to_json mix_to_json var_to_json) modif;
                JsonUtil.of_option
                  (Locality.annot_to_json
                     (Alg_expr.bool_to_yojson mix_to_json var_to_json)) post;
              ])) c.perturbations;
      "configurations",
      JsonUtil.of_list
        (JsonUtil.of_pair
           string_annot_to_json (JsonUtil.of_list string_annot_to_json))
        c.configurations;
    ]

let compil_of_json = function
  | `Assoc l as x when List.length l = 9 ->
    let mix_of_json =
      JsonUtil.to_list agent_of_json in
    let var_of_json = JsonUtil.to_string ?error_msg:None in
    begin
      try
        {
          signatures =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST signature")
              agent_of_json
              (List.assoc "signatures" l);
          tokens =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST token sig")
              string_annot_of_json (List.assoc "tokens" l);
          variables =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST variables")
              (JsonUtil.to_pair
                 string_annot_of_json
                 (Locality.annot_of_json
                    (Alg_expr.e_of_yojson mix_of_json var_of_json)))
              (List.assoc "variables" l);
          rules =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST rules")
              (JsonUtil.to_pair
                 (JsonUtil.to_option string_annot_of_json)
                 (Locality.annot_of_json
                    (rule_of_json mix_of_json var_of_json)))
              (List.assoc "rules" l);
          edit_rules =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST rules")
              (JsonUtil.to_pair
                 (JsonUtil.to_option string_annot_of_json) edit_rule_of_yojson)
              (List.assoc "edit_rules" l);
          observables =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST observables")
              (Locality.annot_of_json
                 (Alg_expr.e_of_yojson mix_of_json var_of_json))
              (List.assoc "observables" l);
          init =
            List.map
              (fun (a,i) -> (None,a,i))
              (JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST init")
                 (JsonUtil.to_pair
                    (Locality.annot_of_json
                       (Alg_expr.e_of_yojson mix_of_json var_of_json))
                    (Locality.annot_of_json
                       (init_of_json mix_of_json var_of_json)))
                 (List.assoc "init" l));
          perturbations =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST perturbations")
              (Locality.annot_of_json
                 (function
                   | `List [alarm; pre; modif; post] ->
                      (JsonUtil.to_option Nbr.of_yojson alarm,
                       JsonUtil.to_option
                       (Locality.annot_of_json
                        (Alg_expr.bool_of_yojson mix_of_json var_of_json)) pre,
                      JsonUtil.to_list
                        (modif_of_json mix_of_json var_of_json) modif,
                      JsonUtil.to_option
                        (Locality.annot_of_json
                           (Alg_expr.bool_of_yojson mix_of_json var_of_json))
                        post)
                   | x ->
                     raise
                       (Yojson.Basic.Util.Type_error ("Not a perturbation",x))
                 ))
              (List.assoc "perturbations" l);
          configurations =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST configuration")
              (JsonUtil.to_pair
                 string_annot_of_json (JsonUtil.to_list string_annot_of_json))
              (List.assoc "configurations" l);
          volumes = [];
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect AST",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST",x))

let combinations ls1 ls2 =
  if (ls1 = []) then List.fold_left (fun acc (b,ds) -> ([b],ds)::acc) [] ls2
  else
    List.fold_left
      (fun acc (a,cs) ->
        List.fold_left (fun acc' (b,ds) -> ((b::a),ds@cs)::acc') acc ls2)
      [] ls1

let name_match (s,_) (s',_) = (String.compare s s') = 0

let update_rate counters (k,a) =
  let update_id s k =
    let (a,_) = List.partition (fun (s',_) -> (String.compare s s') = 0) counters in
    match a with
    | [(_,x)] -> Alg_expr.CONST (Nbr.I x)
    | [] -> k
    | _::_ -> raise (ExceptionDefn.Malformed_Decl
                       ("Counter variable "^s^" appears twice in rule",
                        Locality.dummy)) in
  let rec update_bool k = match k with
    | Alg_expr.TRUE | Alg_expr.FALSE -> k
    | Alg_expr.BIN_BOOL_OP (op,(k1,a1),(k2,a2)) ->
       Alg_expr.BIN_BOOL_OP (op,((update_bool k1),a1),((update_bool k2),a2))
    | Alg_expr.UN_BOOL_OP (op,(k,a)) ->
      Alg_expr.UN_BOOL_OP (op,(update_bool k,a))
    | Alg_expr.COMPARE_OP (op,(k1,a1),(k2,a2)) ->
      Alg_expr.COMPARE_OP (op,((update_expr k1),a1),((update_expr k2),a2))
  and update_expr k = match k with
    | Alg_expr.BIN_ALG_OP (op,(k1,a1),(k2,a2)) ->
       Alg_expr.BIN_ALG_OP (op,((update_expr k1),a1),((update_expr k2),a2))
    | Alg_expr.UN_ALG_OP (op,(k1,a1)) ->
       Alg_expr.UN_ALG_OP (op,((update_expr k1),a1))
    | Alg_expr.IF ((k1,a1),(k2,a2),(k3,a3)) ->
       Alg_expr.IF
         (((update_bool k1),a1),((update_expr k2),a2),((update_expr k3),a3))
    | Alg_expr.DIFF_TOKEN ((k1,a1),k2) ->
       Alg_expr.DIFF_TOKEN (((update_expr k1),a1),k2)
    | Alg_expr.DIFF_KAPPA_INSTANCE ((k,a),m) ->
       Alg_expr.DIFF_KAPPA_INSTANCE (((update_expr k),a),m)
    | Alg_expr.ALG_VAR id| Alg_expr.TOKEN_ID id -> update_id id k
    | Alg_expr.STATE_ALG_OP _| Alg_expr.CONST _| Alg_expr.KAPPA_INSTANCE _ -> k
  in
  ((update_expr k),a)

let prepare_agent rsites lsites =
  let rec prepare_site c = function
    | [] -> [Counter c]
    | hd::tl ->
       match hd with
         Counter c' when (name_match c.count_nme c'.count_nme) ->
         (Counter {c' with count_delta = c.count_delta})::tl
       | Counter _ | Port _ -> hd::(prepare_site c tl) in
  let counters =
    List.fold_left
      (fun acc' rsite ->
        match rsite with Port _ -> acc' | Counter c -> c::acc')
      [] rsites in
  List.fold_left (fun acc' c -> prepare_site c acc') lsites counters

(* - add in the lhs : (i) counters only mentioned in the rhs and (ii) the deltas
   - syntactic checks of no test in rhs; no modif in lhs *)
let prepare_counters rules =
  let syntax sites f error =
    List.iter
      (function Port _ -> ()
              | Counter c ->
                 if (f c) then
                   raise (ExceptionDefn.Malformed_Decl
                            ("Counter "^(fst c.count_nme)^error,
                             (snd c.count_nme)))) sites in

  let rec fold rhs lhs = match (rhs,lhs) with
    | ((rna,rsites,_)::r, ((lna,lsites,b) as lagent)::l) ->
       let () = syntax lsites (fun c -> not((fst c.count_delta)=0))
                       " has a modif in the lhs";
                syntax rsites (fun c -> not(c.count_test=None))
                       " has a test in the rhs" in
       if ((String.compare (fst rna) (fst lna)) = 0) then
         let lsites' = prepare_agent rsites lsites in
         (lna,lsites',b)::(fold r l)
       else lagent::(fold r l)
    | [], _ | _, [] -> [] in

  let aux r = {r with lhs = (fold r.rhs r.lhs)} in
  List.map (fun (s,(r,a)) -> (s,(aux r,a))) rules

let counters_signature s (agents:agent list) =
  let (_,sites',_) = List.find (fun (s',_,_) -> name_match s s') agents in
  List.fold_left
    (fun acc s -> match s with
                    Counter c -> c::acc
                  | Port _ -> acc) [] sites'

(* c': counter declaration, returns counter in rule*)
let enumerate_counter_tests x a ((delta,_) as count_delta) c' =
  let (max,_) = c'.count_delta in
  let min =
    match c'.count_test with
      None| Some (CGTE _,_)| Some (CVAR _,_) ->
             raise
               (ExceptionDefn.Malformed_Decl
                  ("Invalid counter signature - have to specify min bound",
                   (snd c'.count_nme)))
      | Some (CEQ min,_) -> min in

  let rec enum v =
    if (v=max) then []
    else
      if (v+delta < max)&&(v+delta >= 0)
      then (Counter
              {count_nme=c'.count_nme;count_test = Some(CEQ v,a);count_delta},
            [x,v])::(enum (v+1))
      else enum (v+1) in
  enum min

let enumerate rules f =
  List.rev
    (List.fold_left
       (fun acc (s,r) ->
         let enumerate_r =
           if ((fst r).lhs = []) then [(None,r)] else
             List.map
               (fun (s',r') ->
                 match s,s' with
                   None, _ -> None,r'
                 | Some _, None -> s,r'
                 | Some (s1,a1), Some s2 -> Some (s1^"__"^s2,a1),r') (f r) in
         enumerate_r@acc) [] rules)

let enumerate_edit rules f =
  List.rev
    (List.fold_left
       (fun acc (s,r) ->
         let enumerate_r =
           List.map
             (fun (s',r') ->
               match s,s' with
                 None, _ -> None,r'
               | Some _, None -> s,r'
               | Some (s1,a1), Some s2 -> Some (s1^"__"^s2,a1),r') (f r) in
         enumerate_r@acc) [] rules)


let remove_variable_in_counters rules edit_rules signatures =
  let counter_gte_delta c delta =
    let count_delta = {c with count_test=Some (CGTE (abs(delta)),Locality.dummy)} in
    [(Counter count_delta,[])] in
  let counter_gte_zero c =
    [(Counter {c with count_test=Some (CGTE 0,Locality.dummy)}), []] in

  let remove_var_site counters =
    function
      Port p -> [(Port p,[])]
    | Counter c ->
       let (delta,_) = c.count_delta in
       match c.count_test with
         None ->
         if (delta <0) then counter_gte_delta c delta else counter_gte_zero c
       | Some (CEQ v,_) ->
          if (delta >0 || abs(delta) <= v) then [(Counter c,[])]
          else
            raise
              (ExceptionDefn.Malformed_Decl
                 ("Counter "^(fst c.count_nme)^" becomes negative",
                  (snd c.count_nme)))
       | Some (CGTE v,pos) ->
          let () = if (v+delta <0) then
                     raise
                       (ExceptionDefn.Malformed_Decl
                          ("Counter "^(fst c.count_nme)^" becomes negative",
                           (snd c.count_nme))) in
          let () = if (v=0) then
                     let error = "Counter "^(fst c.count_nme)^":>0 always holds" in
                     ExceptionDefn.warning
                       ~pos (fun f -> Format.pp_print_string f error) in
          [(Counter c,[])]
       | Some (CVAR x,a) ->
          enumerate_counter_tests x a c.count_delta
               (List.find
                  (fun c' -> name_match c.count_nme c'.count_nme) counters) in
  let rec remove_var_sites counters = function
    | [] -> []
    | s::t ->
       combinations
         (remove_var_sites counters t) (remove_var_site counters s) in
  let remove_var_agent (s,sites,m) =
    let counters = counters_signature s signatures in
    let enumerate_sites = remove_var_sites counters sites in
    List.map (fun (sites',c) -> ((s,sites',m),c)) enumerate_sites in
  let rec remove_var_mixture = function
    | [] -> []
    | ag::t -> combinations (remove_var_mixture t) (remove_var_agent ag) in

  let update_opt_rate counters = function
    | None -> None
    | Some r -> Some (update_rate counters r) in
  let update_pair_rate counters = function
    | None -> None
    | Some (r1,r2) ->
       Some ((update_rate counters r1),(update_opt_rate counters r2)) in

  let merge get_mix f r =
    List.map
      (fun (mix,counters) -> f mix counters r)
      (remove_var_mixture (get_mix r)) in
  let remove_var_rule r =
    merge (fun (r,_) -> r.lhs)
          (fun lhs counters (r,a) ->
            let k_def = update_rate counters r.k_def in
            let k_un = update_pair_rate counters r.k_un in
            let k_op = update_opt_rate counters r.k_op in
            let k_op_un = update_pair_rate counters r.k_op_un in
            let append =
              if (counters = []) then None
              else
                Some (List.fold_left
                        (fun acc (_,i) -> (string_of_int i)^acc) "" counters) in
            (append,({r with lhs; k_def; k_un; k_op; k_op_un},a))) r in
  let remove_var_edit_rule r =
    merge (fun r -> r.mix)
          (fun mix counters r ->
            let act = update_rate counters r.act in
            let un_act = update_pair_rate counters r.un_act in
            let append = None in
            (append,{r with mix; act; un_act})) r in
  let rules = prepare_counters rules in

  ((enumerate_edit edit_rules remove_var_edit_rule),
   (enumerate rules remove_var_rule))

let with_counters c =
  let with_counters_mix mix =
    List.exists
      (fun (_,ls,_) ->
        List.exists (function Counter _ -> true | Port _ -> false) ls)
      mix in
  with_counters_mix c.signatures

let compile_counters c =
  if (with_counters c) then
    let (edit_rules,rules) =
      remove_variable_in_counters c.rules c.edit_rules c.signatures in
    let () =
      if (!Parameter.debugModeOn) then
      (Format.printf "@.ast rules@.";
      List.iter (fun (s,(r,_)) ->
                  let label = match s with None -> "" | Some (l,_) -> l in
                  Format.printf "@.%s = %a" label print_ast_rule r)
                rules;
      Format.printf "@.ast edit_rules@.";
      List.iter (fun (s,r) ->
                  let label = match s with None -> "" | Some (l,_) -> l in
                  Format.printf "@.%s = %a" label print_ast_edit_rule r)
                edit_rules) in
    ({c with rules;edit_rules},true)
  else (c,false)

let empty_counter =
  {count_nme = ("",Locality.dummy);
   count_test = None; count_delta =(0,Locality.dummy)}
