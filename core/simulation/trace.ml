(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Simulation_info = struct
  type 'a t = {
    story_id: int;
    story_time: float;
    story_event: int;
    profiling_info: 'a;
  }
  (* type of data to be given with observables for story compression
     (such as date when the obs is triggered*)

  let update_profiling_info a info =
    {
      story_id = info.story_id;
      story_time = info.story_time;
      story_event = info.story_event;
      profiling_info = a;
    }

  let event a = a.story_event
  let story_id a = a.story_id
  let compare_by_story_id x y = Mods.int_compare x.story_id y.story_id

  let dummy a =
    { story_id = 0; story_time = 0.; story_event = 0; profiling_info = a }

  let json_dictionnary =
    "\"simulation_info\":{\"id\":0,\"time\":1,\"event\":2,\"profiling\":3}"

  let to_json f x =
    `List
      [
        `Int x.story_id;
        `Float x.story_time;
        `Int x.story_event;
        f x.profiling_info;
      ]

  let of_json f = function
    | `List [ `Int story_id; `Float story_time; `Int story_event; info ] ->
      { story_id; story_time; story_event; profiling_info = f info }
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a simulation_info", x))

  let write_json f ob x =
    JsonUtil.write_sequence ob
      [
        (fun o -> Yojson.Basic.write_int o x.story_id);
        (fun o -> Yojson.Basic.write_float o x.story_time);
        (fun o -> Yojson.Basic.write_int o x.story_event);
        (fun o -> f o x.profiling_info);
      ]

  let read_json f st b =
    JsonUtil.read_variant Yojson.Basic.read_int
      (fun st b story_id ->
        let story_time =
          JsonUtil.read_next_item Yojson.Basic.read_number st b
        in
        let story_event = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        let profiling_info = JsonUtil.read_next_item f st b in
        { story_id; story_time; story_event; profiling_info })
      st b
end

type event_kind =
  | RULE of int
  | INIT of int list (* the agents *)
  | PERT of string (* the rule *)

let print_event_kind ?env f x =
  match env with
  | None ->
    (match x with
    | RULE i -> Format.fprintf f "RULE(%i)" i
    | INIT l ->
      Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
    | PERT s -> Format.fprintf f "PERT(%s)" s)
  | Some env ->
    (match x with
    | PERT s -> Format.pp_print_string f s
    | RULE r_id -> Model.print_rule ~noCounters:false ~env f r_id
    | INIT s ->
      Format.fprintf f "Intro @[<h>%a@]"
        (Pp.list Pp.comma (Model.print_agent ~env))
        s)

let print_event_kind_dot_annot env f = function
  | RULE r_id ->
    Format.fprintf f "[label=\"%a\", shape=%s, style=%s, fillcolor = %s]"
      (Model.print_rule ~noCounters:false ~env)
      r_id "invhouse" "filled" "lightblue"
  | INIT s ->
    Format.fprintf f
      "[label=\"Intro @[<h>%a@]\", shape=%s, style=%s, fillcolor=green]"
      (Pp.list Pp.comma (Model.print_agent ~env))
      s "house" "filled"
  | PERT s ->
    Format.fprintf f "[label=\"%s\", shape=%s, style=%s, fillcolor = %s]" s
      "invhouse" "filled" "green"

type step =
  | Subs of int * int
  | Rule of
      int * Instantiation.concrete Instantiation.event * unit Simulation_info.t
  | Pert of
      string
      * Instantiation.concrete Instantiation.event
      * unit Simulation_info.t
  | Init of Instantiation.concrete Instantiation.action list
  | Obs of
      string
      * Instantiation.concrete Instantiation.test list list
      * unit Simulation_info.t
  | Dummy of string

type t = step list

let subs_step a b = Subs (a, b)
let dummy_step x = Dummy x
let print_subs _f (_a, _b) = ()

let print_site ?env f ((ag_id, ag), s) =
  Format.fprintf f "%a_%i.%a" (Model.print_agent ?env) ag ag_id
    (match env with
    | Some env -> Signature.print_site (Model.signatures env) ag
    | None -> Format.pp_print_int)
    s

let print_init ~compact ?env log actions =
  let sigs =
    match env with
    | None -> None
    | Some env -> Some (Model.signatures env)
  in
  if compact then
    Format.fprintf log "INIT"
  else
    Format.fprintf log "***@[<1>INIT:%a@]***"
      (Pp.list Pp.space (Instantiation.print_concrete_action ?sigs))
      actions

let print_side_effects ?env =
  Pp.list
    (fun f -> Format.pp_print_string f " ")
    (fun f (site, state) ->
      Format.fprintf f "Side_effects(%a,%a)" (print_site ?env) site
        (Instantiation.print_concrete_binding_state
           ?sigs:(Option_util.map Model.signatures env))
        state)

let print_event ~compact ?env log (ev_kind, e) =
  let sigs =
    match env with
    | None -> None
    | Some env -> Some (Model.signatures env)
  in
  if compact then
    print_event_kind ?env log ev_kind
  else
    Format.fprintf log
      "@[***Refined event:***@,* Kappa_rule %a Story encoding:%a%a%a@]"
      (print_event_kind ?env) ev_kind
      (Pp.list Pp.empty
         (Pp.list Pp.empty (Instantiation.print_concrete_test ?sigs)))
      (e.Instantiation.tests @ [ e.Instantiation.connectivity_tests ])
      (Pp.list Pp.empty (Instantiation.print_concrete_action ?sigs))
      e.Instantiation.actions (print_side_effects ?env)
      e.Instantiation.side_effects_src

let print_obs ~compact ?env f (ev_kind, tests, _) =
  let sigs =
    match env with
    | None -> None
    | Some env -> Some (Model.signatures env)
  in
  if compact then
    Format.fprintf f "OBS %s" ev_kind
  else
    Format.fprintf f "***@[<1>OBS %s:%a@]***" ev_kind
      (Pp.list Pp.space
         (Pp.list Pp.space (Instantiation.print_concrete_test ?sigs)))
      tests

let print_step ?(compact = false) ?env f = function
  | Subs (a, b) -> print_subs f (a, b)
  | Rule (x, y, _z) -> print_event ~compact ?env f (RULE x, y)
  | Pert (x, y, _z) -> print_event ~compact ?env f (PERT x, y)
  | Init a -> print_init ~compact ?env f a
  | Obs (a, b, c) -> print_obs ~compact ?env f (a, b, c)
  | Dummy _ -> ()

let get_types_from_init a =
  List.fold_left
    (fun acc action ->
      match action with
      | Instantiation.Create ((_, atype), _) -> atype :: acc
      | Instantiation.Mod_internal _ | Instantiation.Bind _
      | Instantiation.Bind_to _ | Instantiation.Free _ | Instantiation.Remove _
        ->
        acc)
    [] a

let print_label_of_step ?env f x =
  match env with
  | None ->
    (match x with
    | Subs _ -> ()
    | Rule (x, _, _) -> Format.fprintf f "%i" x
    | Pert (x, _, _) -> Format.fprintf f "%s" x
    | Init a ->
      let l = get_types_from_init a in
      Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
    | Obs (x, _, _) -> Format.fprintf f "%s" x
    | Dummy _ -> ())
  | Some env ->
    (match x with
    | Subs _ -> ()
    | Rule (x, _, _) -> Model.print_rule ~noCounters:false ~env f x
    | Pert (x, _, _) -> Format.pp_print_string f x
    | Init a ->
      let l = get_types_from_init a in
      Format.fprintf f "Intro @[<h>%a@]"
        (Pp.list Pp.comma (Model.print_agent ~env))
        l
    | Obs (x, _, _) -> Format.pp_print_string f x
    | Dummy _ -> ())

let json_dictionnary =
  "\"step\":[\"Subs\",\"Rule\",\"Pert\",\"Init\",\"Obs\",\"Dummy\"]"

let write_step ob s =
  JsonUtil.write_sequence ob
    (match s with
    | Subs (a, b) ->
      [
        (fun o -> Yojson.Basic.write_int o 0);
        (fun o -> Yojson.Basic.write_int o a);
        (fun o -> Yojson.Basic.write_int o b);
      ]
    | Rule (x, y, z) ->
      [
        (fun o -> Yojson.Basic.write_int o 1);
        (fun o -> Yojson.Basic.write_int o x);
        (fun o -> Instantiation.write_event Agent.write_json o y);
        (fun o -> Simulation_info.write_json Yojson.Basic.write_null o z);
      ]
    | Pert (x, y, z) ->
      [
        (fun o -> Yojson.Basic.write_int o 2);
        (fun o -> Yojson.Basic.write_string o x);
        (fun o -> Instantiation.write_event Agent.write_json o y);
        (fun o -> Simulation_info.write_json Yojson.Basic.write_null o z);
      ]
    | Init a ->
      [
        (fun o -> Yojson.Basic.write_int o 3);
        (fun o ->
          JsonUtil.write_list (Instantiation.write_action Agent.write_json) o a);
      ]
    | Obs (x, y, z) ->
      [
        (fun o -> Yojson.Basic.write_int o 4);
        (fun o -> Yojson.Basic.write_string o x);
        (fun o ->
          JsonUtil.write_list
            (JsonUtil.write_list (Instantiation.write_test Agent.write_json))
            o y);
        (fun o -> Simulation_info.write_json Yojson.Basic.write_null o z);
      ]
    | Dummy _ -> [ (fun o -> Yojson.Basic.write_int o 5) ])

let read_step st b =
  JsonUtil.read_variant Yojson.Basic.read_int
    (fun st b -> function
      | 0 ->
        let a = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        let b = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        Subs (a, b)
      | 1 ->
        let x = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        let y =
          JsonUtil.read_next_item
            (Instantiation.read_event Agent.read_json)
            st b
        in
        let z =
          JsonUtil.read_next_item
            (Simulation_info.read_json Yojson.Basic.read_null)
            st b
        in
        Rule (x, y, z)
      | 2 ->
        let x = JsonUtil.read_next_item Yojson.Basic.read_string st b in
        let y =
          JsonUtil.read_next_item
            (Instantiation.read_event Agent.read_json)
            st b
        in
        let z =
          JsonUtil.read_next_item
            (Simulation_info.read_json Yojson.Basic.read_null)
            st b
        in
        Pert (x, y, z)
      | 3 ->
        let l =
          JsonUtil.read_next_item
            (Yojson.Basic.read_list_rev
               (Instantiation.read_action Agent.read_json))
            st b
        in
        Init (List.rev l)
      | 4 ->
        let x = JsonUtil.read_next_item Yojson.Basic.read_string st b in
        let y =
          JsonUtil.read_next_item
            (Yojson.Basic.read_list
               (Yojson.Basic.read_list
                  (Instantiation.read_test Agent.read_json)))
            st b
        in
        let z =
          JsonUtil.read_next_item
            (Simulation_info.read_json Yojson.Basic.read_null)
            st b
        in
        Obs (x, y, z)
      | 5 -> Dummy ""
      | _ -> raise (Yojson.json_error "Invalid step")
      (*st b*))
    st b

let step_to_yojson = function
  | Subs (a, b) -> `List [ `Int 0; `Int a; `Int b ]
  | Rule (x, y, z) ->
    `List
      [
        `Int 1;
        `Int x;
        Instantiation.event_to_json Agent.to_json y;
        Simulation_info.to_json (fun () -> `Null) z;
      ]
  | Pert (x, y, z) ->
    `List
      [
        `Int 2;
        `String x;
        Instantiation.event_to_json Agent.to_json y;
        Simulation_info.to_json (fun () -> `Null) z;
      ]
  | Init a ->
    let rev_actions =
      List.rev_map (Instantiation.action_to_json Agent.to_json) a
    in
    `List [ `Int 3; `List (List.rev rev_actions) ]
  | Obs (x, y, z) ->
    `List
      [
        `Int 4;
        `String x;
        `List
          (List.map
             (fun z ->
               `List (List.map (Instantiation.test_to_json Agent.to_json) z))
             y);
        Simulation_info.to_json (fun () -> `Null) z;
      ]
  | Dummy _ -> `List [ `Int 5 ]

let write_json = JsonUtil.write_list write_step
let read_json st b = List.rev (Yojson.Basic.read_list_rev read_step st b)

let string_of_step ?(len = 1024) x =
  let ob = Buffer.create len in
  write_step ob x;
  Buffer.contents ob

let step_of_string s =
  read_step (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let step_is_obs = function
  | Obs _ -> true
  | Rule _ | Pert _ | Subs _ | Dummy _ | Init _ -> false

let step_is_init = function
  | Init _ -> true
  | Rule _ | Pert _ | Subs _ | Dummy _ | Obs _ -> false

let step_is_subs = function
  | Subs _ -> true
  | Rule _ | Pert _ | Init _ | Dummy _ | Obs _ -> false

let step_is_rule = function
  | Rule _ -> true
  | Pert _ | Init _ | Subs _ | Dummy _ | Obs _ -> false

let step_is_pert = function
  | Pert _ -> true
  | Rule _ | Init _ | Subs _ | Dummy _ | Obs _ -> false

let simulation_info_of_step = function
  | Obs (_, _, info) | Rule (_, _, info) | Pert (_, _, info) -> Some info
  | Init _ -> Some (Simulation_info.dummy ())
  | Subs _ | Dummy _ -> None

let creation_of_actions op actions =
  List.fold_left
    (fun l -> function
      | Instantiation.Create (x, _) -> op x :: l
      | Instantiation.Mod_internal _ | Instantiation.Bind _
      | Instantiation.Bind_to _ | Instantiation.Free _ | Instantiation.Remove _
        ->
        l)
    [] actions

let creation_of_step = function
  | Rule (_, { Instantiation.actions = ac; _ }, _)
  | Pert (_, { Instantiation.actions = ac; _ }, _)
  | Init ac ->
    creation_of_actions fst ac
  | Obs _ | Dummy _ | Subs _ -> []

let has_creation_of_step x = creation_of_step x <> []

let tests_of_step = function
  | Subs _ -> []
  | Rule (_, e, _) | Pert (_, e, _) ->
    List.fold_right List.append e.Instantiation.tests
      e.Instantiation.connectivity_tests
  | Init _ -> []
  | Obs (_, x, _) -> List.concat x
  | Dummy _ -> []

let actions_of_step = function
  | Subs _ -> [], []
  | Rule (_, e, _) | Pert (_, e, _) ->
    e.Instantiation.actions, e.Instantiation.side_effects_src
  | Init y -> y, []
  | Obs (_, _, _) -> [], []
  | Dummy _ -> [], []

let side_effects_of_step = function
  | Rule (_, e, _) | Pert (_, e, _) -> e.Instantiation.side_effects_dst
  | Subs _ | Obs _ | Dummy _ | Init _ -> []

let init_trace_file ~uuid env desc =
  let () = output_string desc "{\n\"uuid\" : \"" in
  let () = output_string desc (string_of_int uuid) in
  let () = output_string desc "\",\n\"dict\":{" in
  let () = output_string desc Agent.json_dictionnary in
  let () = output_string desc "," in
  let () = output_string desc Instantiation.json_dictionnary in
  let () = output_string desc "," in
  let () = output_string desc Simulation_info.json_dictionnary in
  let () = output_string desc "," in
  let () = output_string desc json_dictionnary in
  let () = output_string desc "},\n\"model\":" in
  let () = Yojson.Basic.to_channel desc (Model.to_yojson env) in
  output_string desc ",\n\"trace\":["

let assert_field ident x =
  if ident <> x then
    Yojson.json_error
      ("trace lacks the field \"" ^ x ^ "\" (at the right place)")

let read_trace_headers lex_st lex_buf =
  let ident =
    JsonUtil.read_between_spaces Yojson.Basic.read_ident lex_st lex_buf
  in
  let ident', uuid =
    if ident = "uuid" then (
      let () = Yojson.Basic.read_colon lex_st lex_buf in
      let uuid =
        JsonUtil.read_between_spaces Yojson.Basic.read_string lex_st lex_buf
      in
      let uuid = try Some (int_of_string uuid) with _ -> None in
      JsonUtil.read_next_item Yojson.Basic.read_ident lex_st lex_buf, uuid
    ) else
      ident, None
  in
  let () = assert_field ident' "dict" in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = JsonUtil.read_between_spaces Yojson.Basic.skip_json lex_st lex_buf in
  uuid

let fold_trace f init lex_st lex_buf =
  let () = Yojson.Basic.read_lcurl lex_st lex_buf in
  let _ = read_trace_headers lex_st lex_buf in
  let ident = JsonUtil.read_next_item Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert_field ident "model" in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let env =
    Model.of_yojson
      (JsonUtil.read_between_spaces Yojson.Basic.read_json lex_st lex_buf)
  in
  let ident = JsonUtil.read_next_item Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert_field ident "trace" in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let out =
    JsonUtil.read_between_spaces
      (Yojson.Basic.read_sequence
         (fun acc x y -> f env acc (read_step x y))
         (init env))
      lex_st lex_buf
  in
  let () =
    try Yojson.Basic.read_object_end lex_buf with Yojson.End_of_object -> ()
  in
  env, out

let fold_trace_file f init fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let out = JsonUtil.read_between_spaces (fold_trace f init) lex_st lex_buf in
  let () = close_in desc in
  out

let get_headers_from_file fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let () = Yojson.Basic.read_lcurl lex_st lex_buf in
  let uuid = read_trace_headers lex_st lex_buf in
  let ident = JsonUtil.read_next_item Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert_field ident "model" in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let env =
    Model.of_yojson
      (JsonUtil.read_between_spaces Yojson.Basic.read_json lex_st lex_buf)
  in
  let () = close_in desc in
  uuid, env
