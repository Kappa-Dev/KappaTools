(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type syntax_version = V3 | V4

let merge_version a b =
  match a, b with
  | V4, _ | _, V4 -> V4
  | V3, V3 -> V3

type internal = string option Locality.annot list

type port = {
  port_nme: string Locality.annot;
  port_int: internal;
  port_int_mod: string Locality.annot option;
  port_lnk: (string Locality.annot, unit) LKappa.link Locality.annot list;
  port_lnk_mod: int Locality.annot option option;
}

type counter_test = CEQ of int | CGTE of int | CVAR of string

type counter = {
  count_nme: string Locality.annot;
  count_test: counter_test Locality.annot option;
  count_delta: int Locality.annot;
}

type site = Port of port | Counter of counter
type agent_mod = Erase | Create

type agent =
  | Present of string Locality.annot * site list * agent_mod option
  | Absent of Locality.t

type mixture = agent list list

type edit_notation = {
  mix: mixture;
  delta_token:
    ((mixture, string) Alg_expr.e Locality.annot * string Locality.annot) list;
}

type arrow_notation = {
  lhs: mixture;
  rm_token:
    ((mixture, string) Alg_expr.e Locality.annot * string Locality.annot) list;
  rhs: mixture;
  add_token:
    ((mixture, string) Alg_expr.e Locality.annot * string Locality.annot) list;
}

type rule_content = Edit of edit_notation | Arrow of arrow_notation

type rule = {
  rewrite: rule_content;
  bidirectional: bool;
  k_def: (mixture, string) Alg_expr.e Locality.annot;
  k_un:
    ((mixture, string) Alg_expr.e Locality.annot
    * (mixture, string) Alg_expr.e Locality.annot option)
    option;
  (*k_1:radius_opt*)
  k_op: (mixture, string) Alg_expr.e Locality.annot option;
  k_op_un:
    ((mixture, string) Alg_expr.e Locality.annot
    * (mixture, string) Alg_expr.e Locality.annot option)
    option;
      (*rate for backward rule*)
}

let flip_label str = str ^ "_op"

type ('pattern, 'mixture, 'id, 'rule) modif_expr =
  | APPLY of (('pattern, 'id) Alg_expr.e Locality.annot * 'rule Locality.annot)
  | UPDATE of ('id Locality.annot * ('pattern, 'id) Alg_expr.e Locality.annot)
  | STOP of ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | SNAPSHOT of bool * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | PRINT of
      ('pattern, 'id) Alg_expr.e Primitives.print_expr list
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Locality.annot)
  | CFLOWMIX of (bool * 'pattern Locality.annot)
  | DIN of
      Primitives.din_kind
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | DINOFF of ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | SPECIES_OF of
      bool
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
      * 'pattern Locality.annot

type ('pattern, 'mixture, 'id, 'rule) perturbation =
  (Nbr.t option
  * ('pattern, 'id) Alg_expr.bool Locality.annot option
  * ('pattern, 'mixture, 'id, 'rule) modif_expr list
  * ('pattern, 'id) Alg_expr.bool Locality.annot option)
  Locality.annot

type configuration = string Locality.annot * string Locality.annot list

type ('pattern, 'id) variable_def =
  string Locality.annot * ('pattern, 'id) Alg_expr.e Locality.annot

type ('mixture, 'id) init_t =
  | INIT_MIX of 'mixture Locality.annot
  | INIT_TOK of 'id Locality.annot list

type ('pattern, 'mixture, 'id) init_statment =
  (*  string Locality.annot option * (*volume*)*)
  ('pattern, 'id) Alg_expr.e Locality.annot * ('mixture, 'id) init_t

type ('agent, 'pattern, 'mixture, 'id, 'rule) instruction =
  | SIG of 'agent
  | TOKENSIG of string Locality.annot
  | VOLSIG of string * float * string (* type, volume, parameter*)
  | INIT of ('pattern, 'mixture, 'id) init_statment
  (*volume, init, position *)
  | DECLARE of ('pattern, 'id) variable_def
  | OBS of ('pattern, 'id) variable_def (*for backward compatibility*)
  | PLOT of ('pattern, 'id) Alg_expr.e Locality.annot
  | PERT of ('pattern, 'mixture, 'id, 'rule) perturbation
  | CONFIG of configuration
  | RULE of (string Locality.annot option * 'rule Locality.annot)

type ('pattern, 'mixture, 'id, 'rule) command =
  | RUN of ('pattern, 'id) Alg_expr.bool Locality.annot
  | MODIFY of ('pattern, 'mixture, 'id, 'rule) modif_expr list
  | QUIT

type ('agent, 'pattern, 'mixture, 'id, 'rule) compil = {
  filenames: string list;
  variables: ('pattern, 'id) variable_def list;
  (*pattern declaration for reusing as variable in perturbations
    or kinetic rate*)
  signatures: 'agent list; (*agent signature declaration*)
  rules: (string Locality.annot option * 'rule Locality.annot) list;
  (*rules (possibly named)*)
  observables: ('pattern, 'id) Alg_expr.e Locality.annot list;
  (*list of patterns to plot*)
  init: ('pattern, 'mixture, 'id) init_statment list;
  (*initial graph declaration*)
  perturbations: ('pattern, 'mixture, 'id, 'rule) perturbation list;
  configurations: configuration list;
  tokens: string Locality.annot list;
  volumes: (string * float * string) list;
}

type parsing_compil = (agent, mixture, mixture, string, rule) compil
type parsing_instruction = (agent, mixture, mixture, string, rule) instruction

let no_more_site_on_right error left right =
  List.for_all
    (function
      | Counter _ -> true
      | Port p ->
        List.exists
          (function
            | Counter _ -> false
            | Port p' -> fst p.port_nme = fst p'.port_nme)
          left
        ||
        let () =
          if error then
            raise
              (ExceptionDefn.Malformed_Decl
                 ( "Site '" ^ fst p.port_nme
                   ^ "' was not mentionned in the left-hand side.",
                   snd p.port_nme ))
        in
        false)
    right

let empty_compil =
  {
    filenames = [];
    variables = [];
    signatures = [];
    rules = [];
    init = [];
    observables = [];
    perturbations = [];
    configurations = [];
    tokens = [];
    volumes = [];
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

let print_ast_link mod_l f l =
  if l <> [] || mod_l <> None then
    Format.fprintf f "[%a%a]"
      (Pp.list Pp.space (fun f (x, _) ->
           LKappa.print_link
             (fun _ f (x, _) -> Format.pp_print_string f x)
             (fun f (x, _) -> Format.pp_print_string f x)
             (fun _ () -> ())
             f x))
      l
      (Pp.option ~with_space:false (fun f x ->
           Format.fprintf f "/%a"
             (fun f -> function
               | Some (l, _) -> Format.pp_print_int f l
               | None -> Format.pp_print_string f ".")
             x))
      mod_l

let print_ast_internal mod_i f l =
  if l <> [] || mod_i <> None then
    Format.fprintf f "{%a%a}"
      (Pp.list Pp.space (fun f -> function
         | Some x, _ -> Format.pp_print_string f x
         | None, _ -> Format.pp_print_string f "#"))
      l
      (Pp.option ~with_space:false (fun f (i, _) -> Format.fprintf f "/%s" i))
      mod_i

let print_ast_port f p =
  Format.fprintf f "%s%a%a" (fst p.port_nme)
    (print_ast_internal p.port_int_mod)
    p.port_int
    (print_ast_link p.port_lnk_mod)
    p.port_lnk

let print_counter_test f = function
  | CEQ x, _ -> Format.fprintf f "=%i" x
  | CGTE x, _ -> Format.fprintf f ">=%i" x
  | CVAR x, _ -> Format.fprintf f "=%s" x

let print_counter_delta test f (delta, _) =
  if delta <> 0 then
    Format.fprintf f "%a+=%d"
      (Pp.option ~with_space:false (fun f _ -> Format.pp_print_string f "/"))
      test delta

let print_counter f c =
  Format.fprintf f "%s{%a%a}" (fst c.count_nme)
    (Pp.option ~with_space:false print_counter_test)
    c.count_test
    (print_counter_delta c.count_test)
    c.count_delta

let print_ast_site f = function
  | Port p -> print_ast_port f p
  | Counter c -> print_counter f c

let string_annot_to_json filenames =
  Locality.annot_to_yojson ~filenames JsonUtil.of_string

let string_annot_of_json filenames =
  Locality.annot_of_yojson ~filenames (JsonUtil.to_string ?error_msg:None)

let string_option_annot_to_json filenames =
  Locality.annot_to_yojson ~filenames (JsonUtil.of_option JsonUtil.of_string)

let string_option_annot_of_json filenames =
  Locality.annot_of_yojson ~filenames
    (JsonUtil.to_option (JsonUtil.to_string ?error_msg:None))

let counter_test_to_json = function
  | CEQ x -> `Assoc [ "test", `String "eq"; "val", `Int x ]
  | CGTE x -> `Assoc [ "test", `String "gte"; "val", `Int x ]
  | CVAR x -> `Assoc [ "test", `String "eq"; "val", `String x ]

let counter_test_of_json = function
  | `Assoc [ ("test", `String "eq"); ("val", `Int x) ]
  | `Assoc [ ("val", `Int x); ("test", `String "eq") ] ->
    CEQ x
  | `Assoc [ ("val", `Int x); ("test", `String "gte") ]
  | `Assoc [ ("test", `String "gte"); ("val", `Int x) ] ->
    CGTE x
  | `Assoc [ ("test", `String "eq"); ("val", `String x) ]
  | `Assoc [ ("val", `String x); ("test", `String "eq") ] ->
    CVAR x
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect counter test", x))

let port_to_json filenames p =
  let mod_l =
    JsonUtil.of_option (function
      | None -> `String "FREE"
      | Some x -> Locality.annot_to_yojson ~filenames JsonUtil.of_int x)
  in
  let mod_i =
    JsonUtil.of_option (Locality.annot_to_yojson ~filenames JsonUtil.of_string)
  in
  JsonUtil.smart_assoc
    [
      "port_nme", string_annot_to_json filenames p.port_nme;
      ( "port_int",
        JsonUtil.smart_assoc
          [
            ( "state",
              JsonUtil.of_list
                (string_option_annot_to_json filenames)
                p.port_int );
            "mod", mod_i p.port_int_mod;
          ] );
      ( "port_lnk",
        JsonUtil.smart_assoc
          [
            ( "state",
              JsonUtil.of_list
                (Locality.annot_to_yojson ~filenames
                   (LKappa.link_to_json
                      (fun _ -> string_annot_to_json filenames)
                      (string_annot_to_json filenames)
                      (fun () -> [])))
                p.port_lnk );
            "mod", mod_l p.port_lnk_mod;
          ] );
    ]

let build_port_of_json filenames n i l =
  let mod_l =
    JsonUtil.to_option (function
      | `String "FREE" -> None
      | x ->
        Some
          (Locality.annot_of_yojson ~filenames
             (JsonUtil.to_int ?error_msg:None)
             x))
  in
  let mod_i =
    JsonUtil.to_option
      (Locality.annot_of_yojson ~filenames (JsonUtil.to_string ?error_msg:None))
  in
  let port_int, port_int_mod =
    match i with
    | `Assoc [] | `Null -> [], None
    | `Assoc [ ("state", i) ] ->
      JsonUtil.to_list (string_option_annot_of_json filenames) i, None
    | `Assoc [ ("mod", m) ] -> [], mod_i m
    | `Assoc [ ("state", i); ("mod", m) ] | `Assoc [ ("mod", m); ("state", i) ]
      ->
      JsonUtil.to_list (string_option_annot_of_json filenames) i, mod_i m
    | _ -> raise (Yojson.Basic.Util.Type_error ("Not internal states", i))
  in
  let port_lnk, port_lnk_mod =
    match l with
    | `Assoc [] | `Null -> [], None
    | `Assoc [ ("state", l) ] ->
      ( JsonUtil.to_list
          (Locality.annot_of_yojson ~filenames
             (LKappa.link_of_json
                (fun _ -> string_annot_of_json filenames)
                (string_annot_of_json filenames)
                (fun _ -> ())))
          l,
        None )
    | `Assoc [ ("mod", m) ] -> [], mod_l m
    | `Assoc [ ("state", l); ("mod", m) ] | `Assoc [ ("mod", m); ("state", l) ]
      ->
      ( JsonUtil.to_list
          (Locality.annot_of_yojson ~filenames
             (LKappa.link_of_json
                (fun _ -> string_annot_of_json filenames)
                (string_annot_of_json filenames)
                (fun _ -> ())))
          l,
        mod_l m )
    | _ -> raise (Yojson.Basic.Util.Type_error ("Not link states", i))
  in
  Port
    {
      port_nme = string_annot_of_json filenames n;
      port_int;
      port_int_mod;
      port_lnk;
      port_lnk_mod;
    }

let site_of_json filenames = function
  | `Assoc [ ("count_nme", n); ("count_test", t); ("count_delta", d) ]
  | `Assoc [ ("count_nme", n); ("count_delta", d); ("count_test", t) ]
  | `Assoc [ ("count_test", t); ("count_nme", n); ("count_delta", d) ]
  | `Assoc [ ("count_test", t); ("count_delta", d); ("count_nme", n) ]
  | `Assoc [ ("count_delta", d); ("count_nme", n); ("count_test", t) ]
  | `Assoc [ ("count_delta", d); ("count_test", t); ("count_nme", n) ] ->
    Counter
      {
        count_nme =
          Locality.annot_of_yojson ~filenames Yojson.Basic.Util.to_string n;
        count_test =
          JsonUtil.to_option
            (Locality.annot_of_yojson ~filenames counter_test_of_json)
            t;
        count_delta =
          Locality.annot_of_yojson ~filenames Yojson.Basic.Util.to_int d;
      }
  | `Assoc [ ("port_nme", n); ("port_int", i); ("port_lnk", l) ]
  | `Assoc [ ("port_nme", n); ("port_lnk", l); ("port_int", i) ]
  | `Assoc [ ("port_int", i); ("port_nme", n); ("port_lnk", l) ]
  | `Assoc [ ("port_lnk", l); ("port_nme", n); ("port_int", i) ]
  | `Assoc [ ("port_int", i); ("port_lnk", l); ("port_nme", n) ]
  | `Assoc [ ("port_lnk", l); ("port_int", i); ("port_nme", n) ] ->
    build_port_of_json filenames n i l
  | `Assoc [ ("port_nme", n); ("port_int", i) ]
  | `Assoc [ ("port_int", i); ("port_nme", n) ] ->
    build_port_of_json filenames n i `Null
  | `Assoc [ ("port_nme", n); ("port_lnk", l) ]
  | `Assoc [ ("port_lnk", l); ("port_nme", n) ] ->
    build_port_of_json filenames n `Null l
  | `Assoc [ ("port_nme", n) ] -> build_port_of_json filenames n `Null `Null
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent", x))

let site_to_json filenames = function
  | Port p -> port_to_json filenames p
  | Counter c ->
    `Assoc
      [
        ( "count_nme",
          Locality.annot_to_yojson ~filenames JsonUtil.of_string c.count_nme );
        ( "count_test",
          JsonUtil.of_option
            (Locality.annot_to_yojson ~filenames counter_test_to_json)
            c.count_test );
        ( "count_delta",
          Locality.annot_to_yojson ~filenames JsonUtil.of_int c.count_delta );
      ]

let print_agent_mod f = function
  | Create -> Format.pp_print_string f "+"
  | Erase -> Format.pp_print_string f "-"

let print_ast_agent f = function
  | Absent _ -> Format.pp_print_string f "."
  | Present ((ag_na, _), l, m) ->
    Format.fprintf f "%s(%a)%a" ag_na
      (Pp.list (fun f -> Format.fprintf f " ") print_ast_site)
      l
      (Pp.option ~with_space:false print_agent_mod)
      m

let agent_mod_to_yojson = function
  | Create -> `String "created"
  | Erase -> `String "erase"

let agent_mod_of_yojson = function
  | `String "created" -> Create
  | `String "erase" -> Erase
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect agent modification", x))

let agent_to_json filenames = function
  | Absent _ -> `Null
  | Present (na, l, m) ->
    JsonUtil.smart_assoc
      [
        "name", Locality.annot_to_yojson ~filenames JsonUtil.of_string na;
        "sig", JsonUtil.of_list (site_to_json filenames) l;
        "mod", (JsonUtil.of_option agent_mod_to_yojson) m;
      ]

let agent_of_json filenames = function
  | `Null -> Absent Locality.dummy
  | `Assoc [ ("name", n); ("sig", s); ("mod", m) ]
  | `Assoc [ ("sig", s); ("name", n); ("mod", m) ]
  | `Assoc [ ("name", n); ("mod", m); ("sig", s) ]
  | `Assoc [ ("sig", s); ("mod", m); ("name", n) ]
  | `Assoc [ ("mod", m); ("name", n); ("sig", s) ]
  | `Assoc [ ("mod", m); ("sig", s); ("name", n) ] ->
    Present
      ( Locality.annot_of_yojson ~filenames
          (JsonUtil.to_string ?error_msg:None)
          n,
        JsonUtil.to_list (site_of_json filenames) s,
        (JsonUtil.to_option agent_mod_of_yojson) m )
  | `Assoc [ ("name", n); ("mod", m) ] | `Assoc [ ("mod", m); ("name", n) ] ->
    Present
      ( Locality.annot_of_yojson ~filenames
          (JsonUtil.to_string ?error_msg:None)
          n,
        [],
        (JsonUtil.to_option agent_mod_of_yojson) m )
  | `Assoc [ ("name", n); ("sig", s) ] | `Assoc [ ("sig", s); ("name", n) ] ->
    Present
      ( Locality.annot_of_yojson ~filenames
          (JsonUtil.to_string ?error_msg:None)
          n,
        JsonUtil.to_list (site_of_json filenames) s,
        None )
  | `Assoc [ ("name", n) ] ->
    Present
      ( Locality.annot_of_yojson ~filenames
          (JsonUtil.to_string ?error_msg:None)
          n,
        [],
        None )
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent", x))

let print_ast_mix =
  Pp.list (fun f -> Format.fprintf f "\\@ ") (Pp.list Pp.comma print_ast_agent)

let to_erased_mixture =
  List.map
    (List.map (function
      | Absent pos -> Absent pos
      | Present (n, s, _) -> Present (n, s, Some Erase)))

let to_created_mixture =
  List.map
    (List.map (function
      | Absent pos -> Absent pos
      | Present (n, s, _) -> Present (n, s, Some Create)))

let to_dummy_user_link = function
  | [] | [ (LKappa.LNK_ANY, _) ] -> User_graph.WHATEVER
  | [ (LKappa.ANY_FREE, _) ] -> User_graph.LINKS []
  | [ (LKappa.LNK_VALUE (x, _), _) ] -> User_graph.LINKS [ (-1, -1), x ]
  | [ (LKappa.LNK_FREE, _) ] -> User_graph.LINKS []
  | [ (LKappa.LNK_SOME, _) ] -> User_graph.SOME
  | [ (LKappa.LNK_TYPE ((ty, _), (si, _)), _) ] -> User_graph.TYPE (ty, si)
  | _ :: _ :: _ -> assert false (* TODO *)

let to_dummy_user_internal = function
  | [] -> Some []
  | [ (None, _) ] -> None
  | [ (Some st, _) ] -> Some [ st ]
  | _ :: _ :: _ as l -> Some (List_util.map_option fst l)

let to_dummy_user_site = function
  | Port { port_nme; port_int; port_int_mod = _; port_lnk; port_lnk_mod = _ } ->
    {
      User_graph.site_name = fst port_nme;
      User_graph.site_type =
        User_graph.Port
          {
            User_graph.port_links = to_dummy_user_link port_lnk;
            User_graph.port_states = to_dummy_user_internal port_int;
          };
    }
  | Counter { count_nme; count_test = _; count_delta = _ } ->
    {
      User_graph.site_name = fst count_nme;
      User_graph.site_type = User_graph.Counter (-1);
      (* TODO *)
    }

let to_dummy_user_agent = function
  | Absent _ -> None
  | Present ((na, _), s, _mods) ->
    Some
      {
        User_graph.node_type = na;
        User_graph.node_id = None;
        User_graph.node_sites = Tools.array_map_of_list to_dummy_user_site s;
      }

let setup_link m ((line, row), site) va =
  match m.(line).(row) with
  | None -> ()
  | Some { User_graph.node_sites; _ } ->
    let s = node_sites.(site) in
    (match s.User_graph.site_type with
    | User_graph.Counter _ -> ()
    | User_graph.Port p ->
      node_sites.(site) <-
        {
          User_graph.site_name = s.User_graph.site_name;
          User_graph.site_type =
            User_graph.Port
              {
                User_graph.port_links = User_graph.LINKS [ va ];
                User_graph.port_states = p.User_graph.port_states;
              };
        })

let mixture_to_user_graph m =
  let out =
    Tools.array_map_of_list (Tools.array_map_of_list to_dummy_user_agent) m
  in
  let acc =
    Tools.array_fold_lefti
      (fun line ->
        Tools.array_fold_lefti (fun row acc -> function
          | None -> acc
          | Some { User_graph.node_sites; _ } ->
            Tools.array_fold_lefti
              (fun site acc -> function
                | {
                    User_graph.site_type =
                      User_graph.Port
                        { User_graph.port_links = User_graph.LINKS []; _ };
                    _;
                  } ->
                  acc
                | {
                    User_graph.site_type =
                      User_graph.Port
                        {
                          User_graph.port_links = User_graph.LINKS (_ :: _ :: _);
                          _;
                        };
                    _;
                  } ->
                  assert false
                | {
                    User_graph.site_type =
                      User_graph.Port
                        {
                          User_graph.port_links =
                            ( User_graph.WHATEVER | User_graph.SOME
                            | User_graph.TYPE (_, _) );
                          _;
                        };
                    _;
                  } ->
                  acc
                | { User_graph.site_type = User_graph.Counter _; _ } -> acc
                | {
                    User_graph.site_type =
                      User_graph.Port
                        {
                          User_graph.port_links = User_graph.LINKS [ (_, id) ];
                          _;
                        };
                    _;
                  } ->
                  (match Mods.IntMap.pop id acc with
                  | None, acc' -> Mods.IntMap.add id ((line, row), site) acc'
                  | Some va, acc' ->
                    let va' = (line, row), site in
                    let () = setup_link out va va' in
                    let () = setup_link out va' va in
                    acc'))
              acc node_sites))
      Mods.IntMap.empty out
  in
  let () = assert (Mods.IntMap.is_empty acc) in
  out

let init_to_json ~filenames f_mix f_var = function
  | INIT_MIX m ->
    `List [ `String "mixture"; Locality.annot_to_yojson ~filenames f_mix m ]
  | INIT_TOK t ->
    `List
      [
        `String "token";
        JsonUtil.of_list (Locality.annot_to_yojson ~filenames f_var) t;
      ]

let init_of_json ~filenames f_mix f_var = function
  | `List [ `String "mixture"; m ] ->
    INIT_MIX (Locality.annot_of_yojson ~filenames f_mix m)
  | `List [ `String "token"; t ] ->
    INIT_TOK
      (JsonUtil.to_list
         ~error_msg:(JsonUtil.build_msg "INIT_TOK")
         (Locality.annot_of_yojson ~filenames f_var)
         t)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid Ast init statement", x))

let print_tok pr_mix pr_tok pr_var f ((nb, _), (n, _)) =
  Format.fprintf f "%a %a" (Alg_expr.print pr_mix pr_tok pr_var) nb pr_tok n

let print_one_size tk f mix =
  Format.fprintf f "%a%t%a" print_ast_mix mix
    (fun f ->
      match tk with
      | [] -> ()
      | _ :: _ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (print_tok
          (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
          Format.pp_print_string
          (fun f x -> Format.fprintf f "'%s'" x)))
    tk

let print_arrow f bidir =
  Format.pp_print_string f
    (if bidir then
       "<->"
     else
       "->")

let print_raw_rate pr_mix pr_tok pr_var op f (def, _) =
  Format.fprintf f "%a%t" (Alg_expr.print pr_mix pr_tok pr_var) def (fun f ->
      match op with
      | None -> ()
      | Some (d, _) ->
        Format.fprintf f ", %a" (Alg_expr.print pr_mix pr_tok pr_var) d)

let print_ast_alg_expr =
  Alg_expr.print
    (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
    Format.pp_print_string
    (fun f x -> Format.fprintf f "'%s'" x)

let print_rates_one_dir un f def =
  Format.fprintf f "%a%t"
    (print_raw_rate
       (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
       Format.pp_print_string
       (fun f x -> Format.fprintf f "'%s'" x)
       None)
    def
    (fun f ->
      match un with
      | None -> ()
      | Some ((d, _), max_dist) ->
        Format.fprintf f " {%a%t}" print_ast_alg_expr d (fun f ->
            Pp.option
              (fun f (md, _) -> Format.fprintf f ":%a" print_ast_alg_expr md)
              f max_dist))

let print_rule_content ~bidirectional f = function
  | Edit r -> Format.fprintf f "@[<h>%a @]" (print_one_size r.delta_token) r.mix
  | Arrow r ->
    Format.fprintf f "@[<h>%a %a@ %a@]"
      (print_one_size r.rm_token)
      r.lhs print_arrow bidirectional
      (print_one_size r.add_token)
      r.rhs

let print_ast_rule f r =
  Format.fprintf f "@[<h>%a @@ %a%t@]"
    (print_rule_content ~bidirectional:r.bidirectional) r.rewrite
    (print_rates_one_dir r.k_un) r.k_def (fun f ->
      match r.k_op, r.k_op_un with
      | None, None -> ()
      | None, _ ->
        Format.fprintf f " , %a"
          (print_rates_one_dir r.k_op_un)
          (Alg_expr.const Nbr.zero)
      | Some a, _ -> Format.fprintf f " , %a" (print_rates_one_dir r.k_op_un) a)

let print_configuration f ((n, _), l) =
  Format.fprintf f "@[%%def: \"%s\" @[%a@]@]" n
    (Pp.list Pp.space (fun f (x, _) -> Format.fprintf f "\"%s\"" x))
    l

let print_init f = function
  | (n, _), INIT_MIX (m, _) ->
    Format.fprintf f "@[%%init: @[%a@]@ @[%a@]@]" print_ast_alg_expr n
      print_ast_mix m
  | (n, _), INIT_TOK t ->
    Format.fprintf f "@[%%init: %a %a@]" print_ast_alg_expr n
      (Pp.list Pp.space (fun f (x, _) -> Format.pp_print_string f x))
      t

let print_ast_bool_expr =
  Alg_expr.print_bool
    (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
    Format.pp_print_string
    (fun f x -> Format.fprintf f "'%s'" x)

let print_print_expr f =
  let aux f = function
    | Primitives.Str_pexpr (str, _) -> Format.fprintf f "\"%s\"" str
    | Primitives.Alg_pexpr (alg, _) -> print_ast_alg_expr f alg
  in
  function
  | [] -> ()
  | [ Primitives.Str_pexpr (str, _) ] -> Format.fprintf f " \"%s\"" str
  | ([ Primitives.Alg_pexpr _ ] | _ :: _ :: _) as e ->
    Format.fprintf f "@ (@[%a@])"
      (Pp.list (fun f -> Format.fprintf f ".") aux)
      e

let print_modif f = function
  | APPLY ((n, _), (r, _)) ->
    Format.fprintf f "$APPLY @[%a@] @[%a@];" print_ast_alg_expr n
      (print_rule_content ~bidirectional:false)
      r.rewrite
  | UPDATE ((s, _), (n, _)) ->
    Format.fprintf f "$UPDATE '%s@' @[%a@];" s print_ast_alg_expr n
  | STOP p -> Format.fprintf f "$STOP%a;" print_print_expr p
  | SNAPSHOT (raw, p) ->
    Format.fprintf f "$SNAPSHOT%a%t;" print_print_expr p (fun f ->
        if raw then Format.pp_print_string f " [true]")
  | PRINT ([], x) -> Format.fprintf f "$PRINTF%a" print_print_expr x
  | PRINT (file, x) ->
    Format.fprintf f "$PRINTF%a >%a" print_print_expr x print_print_expr file
  | PLOTENTRY -> Format.pp_print_string f "$PLOTNOW;"
  | CFLOWLABEL (on, (s, _)) ->
    Format.fprintf f "$TRACK '%s' %s;" s
      (if on then
         "[true]"
       else
         "[false]")
  | CFLOWMIX (on, (p, _)) ->
    Format.fprintf f "$TRACK @[%a@] %s;" print_ast_mix p
      (if on then
         "[true]"
       else
         "[false]")
  | DIN (k, p) ->
    Format.fprintf f "$DIN%a %t[true]" print_print_expr p (fun f ->
        match k with
        | Primitives.ABSOLUTE -> Format.fprintf f "\"absolute\" "
        | Primitives.RELATIVE -> ()
        | Primitives.PROBABILITY -> Format.fprintf f "\"probability\" ")
  | DINOFF p -> Format.fprintf f "$DIN%a [false]" print_print_expr p
  | SPECIES_OF (on, p, (m, _)) ->
    Format.fprintf f "$SPECIES_OF @[%a@] %s >%a;" print_ast_mix m
      (if on then
         "[true]"
       else
         "[false]")
      print_print_expr p

let print_perturbation f ((alarm, cond, modif, rep), _) =
  Format.fprintf f "@[%%mod:%a%a do@ @[%a@]%a@]"
    (Pp.option (fun f i -> Format.fprintf f "alarm %a" Nbr.print i))
    alarm
    (Pp.option (fun f (r, _) -> Format.fprintf f "@[%a@]" print_ast_bool_expr r))
    cond
    (Pp.list Pp.space print_modif)
    modif
    (Pp.option (fun f (r, _) ->
         Format.fprintf f "repeat @[%a@]" print_ast_bool_expr r))
    rep

let print_parsing_compil_kappa f c =
  Format.fprintf f "@[<v>%a@,@,%a@,%a@,@,%a@,@,%a@,%a@,@,%a@,@,%a@]@."
    (Pp.list Pp.space print_configuration)
    c.configurations
    (Pp.list Pp.space (fun f a ->
         Format.fprintf f "@[%%agent:@ @[%a@]@]" print_ast_agent a))
    c.signatures
    (Pp.list Pp.space (fun f (s, _) -> Format.fprintf f "%%token: %s" s))
    c.tokens
    (Pp.list Pp.space (fun f ((s, _), (a, _)) ->
         Format.fprintf f "@[%%var: '%s'@ @[%a@]@]" s print_ast_alg_expr a))
    c.variables
    (Pp.list Pp.space (fun f (a, _) ->
         Format.fprintf f "@[%%plot:@ @[%a@]@]" print_ast_alg_expr a))
    c.observables
    (Pp.list Pp.space (fun f (s, (r, _)) ->
         Format.fprintf f "@[@[%a%a@]@]"
           (Pp.option ~with_space:false (fun f (s, _) ->
                Format.fprintf f "'%s'@ " s))
           s print_ast_rule r))
    c.rules
    (Pp.list Pp.space print_perturbation)
    c.perturbations
    (Pp.list Pp.space print_init)
    c.init

let arrow_notation_to_yojson filenames f_mix f_var r =
  JsonUtil.smart_assoc
    [
      "lhs", f_mix r.lhs;
      ( "rm_token",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames f_mix f_var))
             (string_annot_to_json filenames))
          r.rm_token );
      "rhs", f_mix r.rhs;
      ( "add_token",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames f_mix f_var))
             (string_annot_to_json filenames))
          r.add_token );
    ]

let arrow_notation_of_yojson filenames f_mix f_var = function
  | `Assoc l as x when List.length l <= 4 ->
    {
      lhs = f_mix (Yojson.Basic.Util.member "lhs" x);
      rm_token =
        JsonUtil.to_list
          (JsonUtil.to_pair
             (Locality.annot_of_yojson ~filenames
                (Alg_expr.e_of_yojson ~filenames f_mix f_var))
             (string_annot_of_json filenames))
          (Yojson.Basic.Util.member "rm_token" x);
      rhs = f_mix (Yojson.Basic.Util.member "rhs" x);
      add_token =
        JsonUtil.to_list
          (JsonUtil.to_pair
             (Locality.annot_of_yojson ~filenames
                (Alg_expr.e_of_yojson ~filenames f_mix f_var))
             (string_annot_of_json filenames))
          (Yojson.Basic.Util.member "add_token" x);
    }
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect AST arrow_notation", x))

let edit_notation_to_yojson filenames r =
  let mix_to_json =
    JsonUtil.of_list (JsonUtil.of_list (agent_to_json filenames))
  in
  JsonUtil.smart_assoc
    [
      "mix", mix_to_json r.mix;
      ( "delta_token",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames mix_to_json JsonUtil.of_string))
             (string_annot_to_json filenames))
          r.delta_token );
    ]

let edit_notation_of_yojson filenames r =
  let mix_of_json =
    JsonUtil.to_list (JsonUtil.to_list (agent_of_json filenames))
  in
  match r with
  | `Assoc l as x when List.length l < 3 ->
    {
      mix = mix_of_json (Yojson.Basic.Util.member "mix" x);
      delta_token =
        JsonUtil.to_list
          (JsonUtil.to_pair
             (Locality.annot_of_yojson ~filenames
                (Alg_expr.e_of_yojson ~filenames mix_of_json
                   (JsonUtil.to_string ?error_msg:None)))
             (string_annot_of_json filenames))
          (Yojson.Basic.Util.member "delta_token" x);
    }
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST edit_notation", x))

let rule_content_to_yojson filenames f_mix f_var = function
  | Edit r -> `List [ `String "edit"; edit_notation_to_yojson filenames r ]
  | Arrow r ->
    `List [ `String "arrow"; arrow_notation_to_yojson filenames f_mix f_var r ]

let rule_content_of_yojson filenames f_mix f_var = function
  | `List [ `String "edit"; r ] -> Edit (edit_notation_of_yojson filenames r)
  | `List [ `String "arrow"; r ] ->
    Arrow (arrow_notation_of_yojson filenames f_mix f_var r)
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule content", x))

let rule_to_json filenames f_mix f_var r =
  JsonUtil.smart_assoc
    [
      "rewrite", rule_content_to_yojson filenames f_mix f_var r.rewrite;
      "bidirectional", `Bool r.bidirectional;
      ( "k_def",
        Locality.annot_to_yojson ~filenames
          (Alg_expr.e_to_yojson ~filenames f_mix f_var)
          r.k_def );
      ( "k_un",
        JsonUtil.of_option
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames f_mix f_var))
             (JsonUtil.of_option
                (Locality.annot_to_yojson ~filenames
                   (Alg_expr.e_to_yojson ~filenames f_mix f_var))))
          r.k_un );
      ( "k_op",
        JsonUtil.of_option
          (Locality.annot_to_yojson ~filenames
             (Alg_expr.e_to_yojson ~filenames f_mix f_var))
          r.k_op );
      ( "k_op_un",
        JsonUtil.of_option
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames f_mix f_var))
             (JsonUtil.of_option
                (Locality.annot_to_yojson ~filenames
                   (Alg_expr.e_to_yojson ~filenames f_mix f_var))))
          r.k_op_un );
    ]

let rule_of_json filenames f_mix f_var = function
  | `Assoc l as x when List.length l <= 6 ->
    (try
       {
         rewrite =
           rule_content_of_yojson filenames f_mix f_var
             (Yojson.Basic.Util.member "rewrite" x);
         bidirectional =
           Yojson.Basic.Util.to_bool
             (Yojson.Basic.Util.member "bidirectional" x);
         k_def =
           Locality.annot_of_yojson ~filenames
             (Alg_expr.e_of_yojson ~filenames f_mix f_var)
             (Yojson.Basic.Util.member "k_def" x);
         k_un =
           JsonUtil.to_option
             (JsonUtil.to_pair
                (Locality.annot_of_yojson ~filenames
                   (Alg_expr.e_of_yojson ~filenames f_mix f_var))
                (JsonUtil.to_option
                   (Locality.annot_of_yojson ~filenames
                      (Alg_expr.e_of_yojson ~filenames f_mix f_var))))
             (Yojson.Basic.Util.member "k_un" x);
         k_op =
           JsonUtil.to_option
             (Locality.annot_of_yojson ~filenames
                (Alg_expr.e_of_yojson ~filenames f_mix f_var))
             (Yojson.Basic.Util.member "k_op" x);
         k_op_un =
           JsonUtil.to_option
             (JsonUtil.to_pair
                (Locality.annot_of_yojson ~filenames
                   (Alg_expr.e_of_yojson ~filenames f_mix f_var))
                (JsonUtil.to_option
                   (Locality.annot_of_yojson ~filenames
                      (Alg_expr.e_of_yojson ~filenames f_mix f_var))))
             (Yojson.Basic.Util.member "k_op_un" x);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule", x))

let modif_to_json filenames f_mix f_var = function
  | APPLY (alg, r) ->
    `List
      [
        `String "APPLY";
        Locality.annot_to_yojson ~filenames
          (Alg_expr.e_to_yojson ~filenames f_mix f_var)
          alg;
        Locality.annot_to_yojson ~filenames
          (rule_to_json filenames f_mix f_var)
          r;
      ]
  | UPDATE (id, alg) ->
    `List
      [
        `String "UPDATE";
        Locality.annot_to_yojson ~filenames f_var id;
        Locality.annot_to_yojson ~filenames
          (Alg_expr.e_to_yojson ~filenames f_mix f_var)
          alg;
      ]
  | STOP l ->
    `List
      (`String "STOP"
      :: List.map (Primitives.print_expr_to_yojson ~filenames f_mix f_var) l)
  | SNAPSHOT (raw, l) ->
    `List
      (`String
         (if raw then
            "RAW_SNAPSHOT"
          else
            "SNAPSHOT")
      :: List.map (Primitives.print_expr_to_yojson ~filenames f_mix f_var) l)
  | PRINT (file, expr) ->
    `List
      [
        `String "PRINT";
        JsonUtil.of_list
          (Primitives.print_expr_to_yojson ~filenames f_mix f_var)
          file;
        JsonUtil.of_list
          (Primitives.print_expr_to_yojson ~filenames f_mix f_var)
          expr;
      ]
  | PLOTENTRY -> `String "PLOTENTRY"
  | CFLOWLABEL (b, id) ->
    `List [ `String "CFLOWLABEL"; `Bool b; string_annot_to_json filenames id ]
  | CFLOWMIX (b, m) ->
    `List
      [ `String "CFLOW"; `Bool b; Locality.annot_to_yojson ~filenames f_mix m ]
  | DIN (b, file) ->
    `List
      [
        `String "DIN";
        Primitives.din_kind_to_yojson b;
        JsonUtil.of_list
          (Primitives.print_expr_to_yojson ~filenames f_mix f_var)
          file;
      ]
  | DINOFF file ->
    `List
      (`String "DINOFF"
      :: List.map (Primitives.print_expr_to_yojson ~filenames f_mix f_var) file
      )
  | SPECIES_OF (b, l, m) ->
    `List
      [
        `String "SPECIES_OF";
        `Bool b;
        JsonUtil.of_list
          (Primitives.print_expr_to_yojson ~filenames f_mix f_var)
          l;
        Locality.annot_to_yojson ~filenames f_mix m;
      ]

let modif_of_json filenames f_mix f_var = function
  | `List [ `String "APPLY"; alg; mix ] ->
    APPLY
      ( Locality.annot_of_yojson ~filenames
          (Alg_expr.e_of_yojson ~filenames f_mix f_var)
          alg,
        Locality.annot_of_yojson ~filenames
          (rule_of_json filenames f_mix f_var)
          mix )
  | `List [ `String "UPDATE"; id; alg ] ->
    UPDATE
      ( Locality.annot_of_yojson ~filenames f_var id,
        Locality.annot_of_yojson ~filenames
          (Alg_expr.e_of_yojson ~filenames f_mix f_var)
          alg )
  | `List (`String "STOP" :: l) ->
    STOP (List.map (Primitives.print_expr_of_yojson ~filenames f_mix f_var) l)
  | `List (`String "SNAPSHOT" :: l) ->
    SNAPSHOT
      ( false,
        List.map (Primitives.print_expr_of_yojson ~filenames f_mix f_var) l )
  | `List (`String "RAW_SNAPSHOT" :: l) ->
    SNAPSHOT
      (true, List.map (Primitives.print_expr_of_yojson ~filenames f_mix f_var) l)
  | `List [ `String "PRINT"; file; expr ] ->
    PRINT
      ( JsonUtil.to_list
          (Primitives.print_expr_of_yojson ~filenames f_mix f_var)
          file,
        JsonUtil.to_list
          (Primitives.print_expr_of_yojson ~filenames f_mix f_var)
          expr )
  | `String "PLOTENTRY" -> PLOTENTRY
  | `List [ `String "CFLOWLABEL"; `Bool b; id ] ->
    CFLOWLABEL (b, string_annot_of_json filenames id)
  | `List [ `String "CFLOW"; `Bool b; m ] ->
    CFLOWMIX (b, Locality.annot_of_yojson ~filenames f_mix m)
  | `List [ `String "DIN"; b; file ] ->
    DIN
      ( Primitives.din_kind_of_yojson b,
        JsonUtil.to_list
          (Primitives.print_expr_of_yojson ~filenames f_mix f_var)
          file )
  | `List (`String "DINOFF" :: file) ->
    DINOFF
      (List.map (Primitives.print_expr_of_yojson ~filenames f_mix f_var) file)
  | `List [ `String "SPECIES_OF"; `Bool b; file; m ] ->
    SPECIES_OF
      ( b,
        JsonUtil.to_list
          (Primitives.print_expr_of_yojson ~filenames f_mix f_var)
          file,
        Locality.annot_of_yojson ~filenames f_mix m )
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid modification", x))

let merge_internal_mod acc = function
  | None -> acc
  | Some (x, pos) ->
    let x_op = Some x in
    if List.exists (fun (x', _) -> x_op = x') acc then
      acc
    else
      (x_op, pos) :: acc

let merge_internals =
  List.fold_left (fun acc ((x, _) as y) ->
      if
        x = None
        || List.exists
             (fun (x', _) ->
               Option_util.equal (fun x x' -> String.compare x x' = 0) x x')
             acc
      then
        acc
      else
        y :: acc)

let rec merge_sites_counter c = function
  | [] -> [ Counter c ]
  | Counter c' :: _ as l when fst c.count_nme = fst c'.count_nme -> l
  | ((Port _ | Counter _) as h) :: t -> h :: merge_sites_counter c t

let rec merge_sites_port p = function
  | [] -> [ Port { p with port_lnk = [] } ]
  | Port h :: t when fst p.port_nme = fst h.port_nme ->
    Port
      {
        h with
        port_int =
          merge_internal_mod
            (merge_internals h.port_int p.port_int)
            p.port_int_mod;
      }
    :: t
  | ((Port _ | Counter _) as h) :: t -> h :: merge_sites_port p t

let merge_sites =
  List.fold_left (fun acc -> function
    | Port p -> merge_sites_port p acc
    | Counter c -> merge_sites_counter c acc)

let merge_agents =
  List.fold_left
    (List.fold_left (fun acc -> function
       | Absent _ -> acc
       | Present (((na, _) as x), s, _) ->
         let rec aux = function
           | [] ->
             [
               Present
                 ( x,
                   List.map
                     (function
                       | Port p -> Port { p with port_lnk = [] }
                       | Counter _ as x -> x)
                     s,
                   None );
             ]
           | Present ((na', _), s', _) :: t when String.compare na na' = 0 ->
             Present (x, merge_sites s' s, None) :: t
           | ((Present _ | Absent _) as h) :: t -> h :: aux t
         in
         aux acc))

let merge_tokens =
  List.fold_left (fun acc (_, ((na, _) as tok)) ->
      let rec aux = function
        | [] -> [ tok ]
        | (na', _) :: _ as l when String.compare na na' = 0 -> l
        | h :: t as l ->
          let o = aux t in
          if t == o then
            l
          else
            h :: o
      in
      aux acc)

let sig_from_inits =
  List.fold_left (fun (ags, toks) -> function
    | _, INIT_MIX (m, _) -> merge_agents ags m, toks
    | na, INIT_TOK l -> ags, merge_tokens toks (List.map (fun x -> na, x) l))

let sig_from_rule (ags, toks) r =
  match r.rewrite with
  | Edit e -> merge_agents ags e.mix, merge_tokens toks e.delta_token
  | Arrow a ->
    let ags', toks' =
      if r.bidirectional then
        merge_agents ags a.rhs, merge_tokens toks a.add_token
      else
        ags, toks
    in
    merge_agents ags' a.lhs, merge_tokens toks' a.rm_token

let sig_from_rules = List.fold_left (fun p (_, (r, _)) -> sig_from_rule p r)

let sig_from_perts =
  List.fold_left (fun acc ((_, _, p, _), _) ->
      List.fold_left
        (fun p -> function
          | APPLY (_, (r, _)) -> sig_from_rule p r
          | UPDATE _ | STOP _ | SNAPSHOT _ | PRINT _ | PLOTENTRY | CFLOWLABEL _
          | CFLOWMIX _ | DIN _ | DINOFF _ | SPECIES_OF _ ->
            p)
        acc p)

let implicit_signature r =
  let acc = sig_from_inits (r.signatures, r.tokens) r.init in
  let acc' = sig_from_rules acc r.rules in
  let ags, toks = sig_from_perts acc' r.perturbations in
  { r with signatures = ags; tokens = toks }

let split_mixture m =
  List.fold_right
    (fun l (lhs, rhs) ->
      let ll, rr =
        List.fold_right
          (fun ag ((lhs, rhs) as pack) ->
            match ag with
            | Absent _ -> pack
            | Present (((_, pos) as na), intf, modif) ->
              (match modif with
              | Some Create ->
                Absent pos :: lhs, Present (na, intf, None) :: rhs
              | Some Erase -> Present (na, intf, None) :: lhs, Absent pos :: rhs
              | None ->
                let intfl, intfr =
                  List.fold_left
                    (fun (l, r) -> function
                      | Port p ->
                        ( Port
                            {
                              port_nme = p.port_nme;
                              port_int = p.port_int;
                              port_int_mod = None;
                              port_lnk = p.port_lnk;
                              port_lnk_mod = None;
                            }
                          :: l,
                          Port
                            {
                              port_nme = p.port_nme;
                              port_int =
                                (match p.port_int_mod with
                                | None -> p.port_int
                                | Some (x, pos) -> [ Some x, pos ]);
                              port_int_mod = None;
                              port_lnk =
                                (match p.port_lnk_mod with
                                | None -> p.port_lnk
                                | Some None ->
                                  [ Locality.dummy_annot LKappa.LNK_FREE ]
                                | Some (Some (i, pos)) ->
                                  [ LKappa.LNK_VALUE (i, ()), pos ]);
                              port_lnk_mod = None;
                            }
                          :: r )
                      | Counter c ->
                        ( Counter { c with count_delta = Locality.dummy_annot 0 }
                          :: l,
                          Counter { c with count_test = None } :: r ))
                    ([], []) intf
                in
                ( Present (na, intfl, None) :: lhs,
                  Present (na, intfr, None) :: rhs )))
          l ([], [])
      in
      ll :: lhs, rr :: rhs)
    m ([], [])

let compil_to_json c =
  let files =
    Array.of_list (Lexing.dummy_pos.Lexing.pos_fname :: c.filenames)
  in
  let filenames =
    Tools.array_fold_lefti
      (fun i map x -> Mods.StringMap.add x i map)
      Mods.StringMap.empty files
  in
  let mix_to_json =
    JsonUtil.of_list (JsonUtil.of_list (agent_to_json filenames))
  in
  let var_to_json = JsonUtil.of_string in
  `Assoc
    [
      "filenames", JsonUtil.of_array JsonUtil.of_string files;
      "signatures", JsonUtil.of_list (agent_to_json filenames) c.signatures;
      "tokens", JsonUtil.of_list (string_annot_to_json filenames) c.tokens;
      ( "variables",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (string_annot_to_json filenames)
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames mix_to_json var_to_json)))
          c.variables );
      ( "rules",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (JsonUtil.of_option (string_annot_to_json filenames))
             (Locality.annot_to_yojson ~filenames
                (rule_to_json filenames mix_to_json var_to_json)))
          c.rules );
      ( "observables",
        JsonUtil.of_list
          (Locality.annot_to_yojson ~filenames
             (Alg_expr.e_to_yojson ~filenames mix_to_json var_to_json))
          c.observables );
      ( "init",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (Locality.annot_to_yojson ~filenames
                (Alg_expr.e_to_yojson ~filenames mix_to_json var_to_json))
             (init_to_json ~filenames mix_to_json var_to_json))
          c.init );
      ( "perturbations",
        JsonUtil.of_list
          (Locality.annot_to_yojson ~filenames (fun (alarm, pre, modif, post) ->
               `List
                 [
                   JsonUtil.of_option Nbr.to_yojson alarm;
                   JsonUtil.of_option
                     (Locality.annot_to_yojson ~filenames
                        (Alg_expr.bool_to_yojson ~filenames mix_to_json
                           var_to_json))
                     pre;
                   JsonUtil.of_list
                     (modif_to_json filenames mix_to_json var_to_json)
                     modif;
                   JsonUtil.of_option
                     (Locality.annot_to_yojson ~filenames
                        (Alg_expr.bool_to_yojson ~filenames mix_to_json
                           var_to_json))
                     post;
                 ]))
          c.perturbations );
      ( "configurations",
        JsonUtil.of_list
          (JsonUtil.of_pair
             (string_annot_to_json filenames)
             (JsonUtil.of_list (string_annot_to_json filenames)))
          c.configurations );
    ]

let compil_of_json = function
  | `Assoc l as x when List.length l = 9 ->
    let var_of_json = JsonUtil.to_string ?error_msg:None in
    (try
       let filenames =
         JsonUtil.to_array
           (JsonUtil.to_string ?error_msg:None)
           (List.assoc "filenames" l)
       in
       let mix_of_json =
         JsonUtil.to_list (JsonUtil.to_list (agent_of_json filenames))
       in
       {
         filenames = List.tl (Array.to_list filenames);
         signatures =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST signature")
             (agent_of_json filenames)
             (List.assoc "signatures" l);
         tokens =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST token sig")
             (string_annot_of_json filenames)
             (List.assoc "tokens" l);
         variables =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST variables")
             (JsonUtil.to_pair
                (string_annot_of_json filenames)
                (Locality.annot_of_yojson ~filenames
                   (Alg_expr.e_of_yojson ~filenames mix_of_json var_of_json)))
             (List.assoc "variables" l);
         rules =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST rules")
             (JsonUtil.to_pair
                (JsonUtil.to_option (string_annot_of_json filenames))
                (Locality.annot_of_yojson ~filenames
                   (rule_of_json filenames mix_of_json var_of_json)))
             (List.assoc "rules" l);
         observables =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST observables")
             (Locality.annot_of_yojson ~filenames
                (Alg_expr.e_of_yojson ~filenames mix_of_json var_of_json))
             (List.assoc "observables" l);
         init =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST init")
             (JsonUtil.to_pair
                (Locality.annot_of_yojson ~filenames
                   (Alg_expr.e_of_yojson ~filenames mix_of_json var_of_json))
                (init_of_json ~filenames mix_of_json var_of_json))
             (List.assoc "init" l);
         perturbations =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST perturbations")
             (Locality.annot_of_yojson ~filenames (function
               | `List [ alarm; pre; modif; post ] ->
                 ( JsonUtil.to_option Nbr.of_yojson alarm,
                   JsonUtil.to_option
                     (Locality.annot_of_yojson ~filenames
                        (Alg_expr.bool_of_yojson ~filenames mix_of_json
                           var_of_json))
                     pre,
                   JsonUtil.to_list
                     (modif_of_json filenames mix_of_json var_of_json)
                     modif,
                   JsonUtil.to_option
                     (Locality.annot_of_yojson ~filenames
                        (Alg_expr.bool_of_yojson ~filenames mix_of_json
                           var_of_json))
                     post )
               | x ->
                 raise (Yojson.Basic.Util.Type_error ("Not a perturbation", x))))
             (List.assoc "perturbations" l);
         configurations =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "AST configuration")
             (JsonUtil.to_pair
                (string_annot_of_json filenames)
                (JsonUtil.to_list (string_annot_of_json filenames)))
             (List.assoc "configurations" l);
         volumes = [];
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Incorrect AST", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST", x))

let write_parsing_compil b ast = Yojson.Basic.write_json b (compil_to_json ast)
let read_parsing_compil p lb = compil_of_json (Yojson.Basic.read_json p lb)
