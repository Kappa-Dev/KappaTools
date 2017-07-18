(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Simulation_info = struct
  type 'a t =
    {
      story_id: int ;
      story_time: float ;
      story_event: int ;
      profiling_info: 'a;
    }
  (* type of data to be given with observables for story compression
     (such as date when the obs is triggered*)

  let update_profiling_info a info =
    {
      story_id = info.story_id ;
      story_time = info.story_time ;
      story_event = info.story_event ;
      profiling_info = a
    }

  let event a = a.story_event
  let story_id a = a.story_id

  let compare_by_story_id x y = Mods.int_compare x.story_id y.story_id

  let dummy a = {
    story_id = 0 ; story_time = 0. ;
    story_event = 0 ; profiling_info = a;
  }

  let to_json f x =
    JsonUtil.smart_assoc [
      ("id",`Int x.story_id);
      ("time",`Float x.story_time);
      ("event",`Int x.story_event);
      ("profiling",f x.profiling_info)]

  let of_json f = function
    | `Assoc l as x when List.length l <= 4 ->
      begin
        try
          { story_id =
              (match List.assoc "id" l with `Int i -> i | _ -> raise Not_found);
            story_time = (match List.assoc "time" l
                          with `Float i -> i | _ -> raise Not_found);
            story_event = (match List.assoc "event" l
                           with `Int i -> i | _ -> raise Not_found);
            profiling_info = f (Yojson.Basic.Util.member "profiling" x)}
        with Not_found ->
          raise (Yojson.Basic.Util.Type_error ("Not a simulation_info",x))
      end
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a simulation_info",x))
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
    match x with
    | PERT s -> Format.pp_print_string f s
    | RULE r_id ->
      Model.print_rule ~env f r_id
    | INIT s ->
      Format.fprintf
        f "Intro @[<h>%a@]"
        (Pp.list Pp.comma (Model.print_agent ~env)) s

let print_event_kind_dot_annot env f = function
  | RULE r_id  ->
    Format.fprintf
      f "[label=\"%a\", shape=%s, style=%s, fillcolor = %s]"
      (Model.print_rule ~env) r_id
      "invhouse" "filled" "lightblue"
  | INIT s ->
    Format.fprintf
      f "[label=\"Intro @[<h>%a@]\", shape=%s, style=%s, fillcolor=green]"
      (Pp.list Pp.comma (Model.print_agent ~env)) s "house" "filled"
  | PERT s ->
    Format.fprintf
      f "[label=\"%s\", shape=%s, style=%s, fillcolor = %s]"
      s "invhouse" "filled" "green"

type step =
  | Subs of int * int
  | Rule of
      int *
      Instantiation.concrete Instantiation.event *
      unit Simulation_info.t
  | Pert of
      string *
      Instantiation.concrete Instantiation.event *
      unit Simulation_info.t
  | Init of Instantiation.concrete Instantiation.action list
  | Obs of
      string *
      Instantiation.concrete Instantiation.test list list *
      unit Simulation_info.t
  | Dummy  of string

type t = step list

let subs_step a b = Subs (a,b)
let dummy_step x = Dummy x

let print_subs _f (_a,_b)  = ()

let print_site ?env f ((ag_id,ag),s) =
  Format.fprintf
    f "%a_%i.%a"
    (Model.print_agent ?env) ag ag_id
    (match env with
     | Some env ->
       Signature.print_site (Model.signatures env) ag
     | None -> Format.pp_print_int) s

let print_init ~compact ?env log actions =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Model.signatures env) in
  if compact then
    Format.fprintf
      log "INIT"
  else
    Format.fprintf
      log "***@[<1>INIT:%a@]***"
      (Pp.list Pp.space (Instantiation.print_concrete_action ?sigs)) actions

let print_side_effects ?env =
  Pp.list
    (fun f -> Format.pp_print_string f " ")
    (fun f (site,state) ->
       Format.fprintf
         f "Side_effects(%a,%a)"
         (print_site ?env) site
         (Instantiation.print_concrete_binding_state
            ?sigs:(Option_util.map Model.signatures env)) state)

let print_event ~compact ?env log (ev_kind,e) =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Model.signatures env) in
  if compact then print_event_kind ?env log ev_kind
  else
    Format.fprintf
      log "@[***Refined event:***@,* Kappa_rule %a Story encoding:%a%a%a@]"
      (print_event_kind ?env) ev_kind
      (Pp.list Pp.empty
         (Pp.list Pp.empty (Instantiation.print_concrete_test ?sigs)))
      (e.Instantiation.tests @ [e.Instantiation.connectivity_tests])
      (Pp.list Pp.empty (Instantiation.print_concrete_action ?sigs))
      e.Instantiation.actions
      (print_side_effects ?env) e.Instantiation.side_effects_src

let print_obs ~compact ?env f (ev_kind,tests,_) =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Model.signatures env) in
  if compact then
    Format.fprintf f "OBS %s" ev_kind
  else
    Format.fprintf
      f "***@[<1>OBS %s:%a@]***" ev_kind
      (Pp.list Pp.space
         (Pp.list Pp.space (Instantiation.print_concrete_test ?sigs)))
      tests

let print_step ?(compact=false) ?env f = function
  | Subs (a,b) -> print_subs f (a,b)
  | Rule (x,y,_z) -> print_event ~compact ?env f (RULE x,y)
  | Pert (x,y,_z) -> print_event ~compact ?env f (PERT x,y)
  | Init a -> print_init ~compact ?env f a
  | Obs (a,b,c) -> print_obs ~compact ?env f (a,b,c)
  | Dummy _  -> ()

let get_types_from_init a =
  List.fold_left
    (fun acc action ->
      match action with
        Instantiation.Create ((_,atype),_) -> atype::acc
      | Instantiation.Mod_internal _ | Instantiation.Bind _
        | Instantiation.Bind_to _ | Instantiation.Free _
        | Instantiation.Remove _ -> acc) [] a

let print_label_of_step ?env f x =  match env with
  | None ->
     (match x with
      | Subs _ -> ()
      | Rule (x,_,_) -> Format.fprintf f "%i" x
      | Pert (x,_,_) -> Format.fprintf f "%s" x
      | Init a ->
         let l = get_types_from_init a in
         Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
      | Obs (x,_,_) -> Format.fprintf f "%s" x
      | Dummy _  -> ())
  | Some env -> match x with
      | Subs _ -> ()
      | Rule (x,_,_) ->
         Model.print_rule ~env f x
      | Pert (x,_,_) -> Format.pp_print_string f x
      | Init a ->
         let l = get_types_from_init a in
         Format.fprintf
           f "Intro @[<h>%a@]"
           (Pp.list Pp.comma (Model.print_agent ~env)) l
      | Obs (x,_,_) -> Format.pp_print_string f x
      | Dummy _  -> ()

let step_to_yojson = function
  | Subs (a,b) -> `List [`String "Subs"; `Int a; `Int b]
  | Rule (x,y,z) ->
    `List [`String "Rule";
           `Int x;
           Instantiation.event_to_json Agent.to_json y;
           Simulation_info.to_json (fun () -> `Null) z]
  | Pert (x,y,z) ->
    `List [`String "Pert";
           `String x;
           Instantiation.event_to_json Agent.to_json y;
           Simulation_info.to_json (fun () -> `Null) z]
  | Init a ->
    `List
      [`String "Init";
       `List (List.map (Instantiation.action_to_json Agent.to_json) a)]
  | Obs (x,y,z) ->
    `List [`String "Obs";
           `String x;
           `List
             (List.map (fun z ->
                  `List (List.map (Instantiation.test_to_json Agent.to_json)
                           z)) y);
           Simulation_info.to_json (fun () -> `Null) z]
  | Dummy _ -> `Null
let step_of_yojson = function
  | `List [`String "Subs"; `Int a; `Int b] -> Subs (a,b)
  | `List [`String "Rule";`Int x;y;z] ->
    Rule (x,
          Instantiation.event_of_json Agent.of_json y,
          Simulation_info.of_json (function _ -> ()) z)
  | `List [`String "Pert";`String x;y;z] ->
    Pert (x,
          Instantiation.event_of_json Agent.of_json y,
          Simulation_info.of_json (function _ -> ()) z)
  | `List [`String "Init"; `List l ] ->
    Init (List.map (Instantiation.action_of_json Agent.of_json) l)
  | `List [`String "Obs"; `String x; `List l; z] ->
    Obs (x,
         (List.map (function  `List ccl ->
              List.map (Instantiation.test_of_json Agent.of_json) ccl
                            | _ as x ->
                              raise (Yojson.Basic.Util.Type_error
                                       ("Not a test list",x))) l),
         Simulation_info.of_json (function _ -> ()) z)
  | `Null -> Dummy ""
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect trace step",x))

let to_yojson t = `List (List.rev_map step_to_yojson (List.rev t))
let of_yojson = function
  | `List l -> List.rev (List.rev_map step_of_yojson l)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a trace",x))

let write_step ob f =
  Yojson.Basic.to_outbuf ob (step_to_yojson f)

let string_of_step ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_step ob x;
  Bi_outbuf.contents ob

let read_step p lb =
  step_of_yojson (Yojson.Basic.from_lexbuf ~stream:true p lb)

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
  | Obs (_,_,info) | Rule (_,_,info) | Pert (_,_,info) -> Some info
  | Init _ -> Some (Simulation_info.dummy ())
  | Subs _ | Dummy _ -> None

let creation_of_actions op actions =
  List.fold_left
    (fun l -> function
       | Instantiation.Create (x,_) -> op x :: l
       | (Instantiation.Mod_internal _ | Instantiation.Bind _
         | Instantiation.Bind_to _ | Instantiation.Free _
         | Instantiation.Remove _) -> l) [] actions
let creation_of_step = function
  | (Rule (_,{ Instantiation.actions = ac; _ },_)
    | Pert (_,{ Instantiation.actions = ac; _ },_)
    | Init ac) -> creation_of_actions fst ac
  | Obs _ | Dummy _ | Subs _ -> []
let has_creation_of_step x = creation_of_step x <> []

let tests_of_step = function
  | Subs _ -> []
  | Rule (_,e,_) | Pert (_,e,_) ->
    List.fold_right
      List.append e.Instantiation.tests e.Instantiation.connectivity_tests
  | Init _ -> []
  | Obs (_,x,_) -> List.concat x
  | Dummy _ -> []

let actions_of_step = function
  | Subs _ -> ([],[])
  | Rule (_,e,_) | Pert (_,e,_) ->
    (e.Instantiation.actions,e.Instantiation.side_effects_src)
  | Init y -> (y,[])
  | Obs (_,_,_) -> ([],[])
  | Dummy _ -> ([],[])

let side_effects_of_step = function
  | Rule ((_,e,_)) | Pert ((_,e,_)) -> e.Instantiation.side_effects_dst
  | Subs _ | Obs _ | Dummy _ | Init _ -> []

let fold_trace_file f init fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let () =
    JsonUtil.read_between_spaces Yojson.Basic.read_lcurl lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let ident' =
    if ident = "uuid" then
      let () =
        JsonUtil.read_between_spaces Yojson.Basic.read_colon lex_st lex_buf in
      let _ = Yojson.Basic.read_string lex_st lex_buf in
      let () =
        JsonUtil.read_between_spaces Yojson.Basic.read_comma lex_st lex_buf in
      Yojson.Basic.read_ident lex_st lex_buf
    else ident in
  let () = assert (ident' = "env") in
  let () =
    JsonUtil.read_between_spaces Yojson.Basic.read_colon lex_st lex_buf in
  let env = Model.of_yojson
      (Yojson.Basic.read_json lex_st lex_buf) in
  let () =
    JsonUtil.read_between_spaces Yojson.Basic.read_comma lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "trace") in
  let () =
    JsonUtil.read_between_spaces Yojson.Basic.read_colon lex_st lex_buf in
  let out = Yojson.Basic.read_sequence
      (fun acc x y ->
         f env acc (step_of_yojson (Yojson.Basic.read_json x y)))
      (init env) lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = try Yojson.Basic.read_object_end lex_buf
    with Yojson.End_of_object -> () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = close_in desc in
  (env,out)
