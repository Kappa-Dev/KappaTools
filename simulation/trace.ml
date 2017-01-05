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
    `Assoc [
      ("id",`Int x.story_id);
      ("time",`Float x.story_time);
      ("event",`Int x.story_event);
      ("profiling",f x.profiling_info)]

  let of_json f = function
    | `Assoc l as x when List.length l = 4 ->
      begin
        try
          { story_id =
              (match List.assoc "id" l with `Int i -> i | _ -> raise Not_found);
            story_time = (match List.assoc "time" l
                          with `Float i -> i | _ -> raise Not_found);
            story_event = (match List.assoc "event" l
                           with `Int i -> i | _ -> raise Not_found);
            profiling_info = f (List.assoc "profiling" l)}
        with Not_found ->
          raise (Yojson.Basic.Util.Type_error ("Not a simulation_info",x))
      end
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a simulation_info",x))
end

type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (* the agents *)
  | PERT of string (* the rule *)

let print_event_kind ?env f x =
  match env with
  | None ->
    (match x with
     | OBS i -> Format.fprintf f "OBS(%s)" i
     | RULE i -> Format.fprintf f "RULE(%i)" i
     | INIT l ->
       Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
     | PERT s -> Format.fprintf f "PERT(%s)" s)
  | Some env ->
    match x with
    | OBS name -> Format.pp_print_string f name
    | PERT s -> Format.pp_print_string f s
    | RULE r_id -> Environment.print_ast_rule ~env f r_id
    | INIT s ->
      Format.fprintf
        f "Intro @[<h>%a@]"
        (Pp.list Pp.comma (Environment.print_agent ~env)) s

let event_kind_to_json = function
  | OBS s -> `List [`String "OBS"; `String s]
  | RULE i -> `List [`String "RULE"; `Int i]
  | INIT l -> `List [`String "INIT"; `List (List.map (fun x -> `Int x) l)]
  | PERT s -> `List [`String "PERT"; `String s]
let event_kind_of_json = function
  | `List [`String "OBS"; `String s] -> OBS s
  | `List [`String "RULE"; `Int i] -> RULE i
  | `List [`String "INIT"; `List l] ->
    INIT (List.map Yojson.Basic.Util.to_int l)
  | `List [`String "PERT"; `String s] -> PERT s
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid event_kind",x))

let print_event_kind_dot_annot env f = function
  | RULE r_id  ->
    Format.fprintf
      f "[label=\"%a\", shape=%s, style=%s, fillcolor = %s]"
      (Environment.print_ast_rule ~env) r_id "invhouse" "filled" "lightblue"
  | OBS name  ->
    Format.fprintf
      f "[label=\"%s\", style=filled, fillcolor=red]" name
  | INIT s ->
    Format.fprintf
      f "[label=\"Intro @[<h>%a@]\", shape=%s, style=%s, fillcolor=green]"
      (Pp.list Pp.comma (Environment.print_agent ~env)) s "house" "filled"
  | PERT s ->
    Format.fprintf
      f "[label=\"%s\", shape=%s, style=%s, fillcolor = %s]"
      s "invhouse" "filled" "green"

type event =
  event_kind *
  Instantiation.concrete Instantiation.event *
  unit Simulation_info.t
type obs =
  event_kind *
  Instantiation.concrete Instantiation.test list *
  unit Simulation_info.t
type step =
  | Subs of int * int
  | Event of event
  | Init of Instantiation.concrete Instantiation.action list
  | Obs of obs
  | Dummy  of string

type t = step list

let subs_step a b = Subs (a,b)
let dummy_step x = Dummy x

let print_subs _f (_a,_b)  = ()

let print_site ?env f ((ag_id,ag),s) =
  Format.fprintf
    f "%a_%i.%a"
    (Environment.print_agent ?env) ag ag_id
    (match env with
     | Some env ->
       Signature.print_site (Environment.signatures env) ag
     | None -> Format.pp_print_int) s

let print_init ~compact ?env log actions =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Environment.signatures env) in
  if compact then
    Format.fprintf
      log "INIT:%a"
      (Pp.list Pp.space (Instantiation.print_concrete_action ?sigs)) actions
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
            ?sigs:(Tools.option_map Environment.signatures env)) state)

let print_event ~compact ?env log (ev_kind,(tests,(actions,side_sites,_))) =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Environment.signatures env) in
  if compact then print_event_kind ?env log ev_kind
  else
    Format.fprintf
      log "@[***Refined event:***@,* Kappa_rule %a Story encoding:%a%a%a@]"
      (print_event_kind ?env) ev_kind
      (Pp.list Pp.empty (Instantiation.print_concrete_test ?sigs)) tests
      (Pp.list Pp.empty (Instantiation.print_concrete_action ?sigs)) actions
      (print_side_effects ?env) side_sites

let print_obs ~compact ?env f (ev_kind,tests,_) =
  let sigs = match env with
    | None -> None
    | Some env -> Some (Environment.signatures env) in
  if compact then
    Format.fprintf f "OBS %a" (print_event_kind ?env) ev_kind
  else
    Format.fprintf
      f "***@[<1>OBS %a:%a@]***" (print_event_kind ?env) ev_kind
      (Pp.list Pp.space (Instantiation.print_concrete_test ?sigs)) tests

let print_step ?(compact=false) ?env f = function
  | Subs (a,b) -> print_subs f (a,b)
  | Event (x,y,_z) -> print_event ~compact ?env f (x,y)
  | Init a -> print_init ~compact ?env f a
  | Obs a -> print_obs ~compact ?env f a
  | Dummy _  -> ()

let step_to_yojson = function
  | Subs (a,b) -> `List [`String "Subs"; `Int a; `Int b]
  | Event (x,y,z) ->
    `List [`String "Event";
           event_kind_to_json x;
           Instantiation.event_to_json Agent.to_json y;
           Simulation_info.to_json (fun () -> `Null) z]
  | Init a ->
    `List
      [`String "Init";
       `List (List.map (Instantiation.action_to_json Agent.to_json) a)]
  | Obs (x,y,z) ->
    `List [`String "Obs";
           event_kind_to_json x;
           `List (List.map (Instantiation.test_to_json Agent.to_json) y);
           Simulation_info.to_json (fun () -> `Null) z]
  | Dummy _ -> `Null
let step_of_yojson = function
  | `List [`String "Subs"; `Int a; `Int b] -> Subs (a,b)
  | `List [`String "Event";x;y;z] ->
    Event (event_kind_of_json x,
           Instantiation.event_of_json Agent.of_json y,
           Simulation_info.of_json (function _ -> ()) z)
  | `List [`String "Init"; `List l ] ->
    Init (List.map (Instantiation.action_of_json Agent.of_json) l)
  | `List [`String "Obs"; x; `List l; z] ->
    Obs (event_kind_of_json x,
         List.map (Instantiation.test_of_json Agent.of_json) l,
         Simulation_info.of_json (function _ -> ()) z)
  | `Null -> Dummy ""
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect trace step",x))

let to_yojson t = `List (List.rev_map step_to_yojson (List.rev t))
let of_yojson = function
  | `List l -> List.rev (List.rev_map step_of_yojson l)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a trace",x))

let step_is_obs = function
  | Obs _ -> true
  | Event _ | Subs _ | Dummy _ | Init _ -> false
let step_is_init = function
  | Init _ -> true
  | Event _ | Subs _ | Dummy _ | Obs _ -> false
let step_is_subs = function
  | Subs _ -> true
  | Event _ | Init _ | Dummy _ | Obs _ -> false
let step_is_event = function
  | Event _ -> true
  | Init _ | Subs _ | Dummy _ | Obs _ -> false

let simulation_info_of_step = function
  | Obs (_,_,info) | Event (_,_,info) -> Some info
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
  | (Event (_,(_,(ac,_,_)),_) | Init ac) -> creation_of_actions fst ac
  | Obs _ | Dummy _ | Subs _ -> []
let has_creation_of_step x = creation_of_step x <> []

let tests_of_step = function
  | Subs _ -> []
  | Event (_,(y,(_,_,_)),_) -> y
  | Init _ -> []
  | Obs (_,x,_) -> x
  | Dummy _ -> []

let actions_of_step = function
  | Subs _ -> ([],[])
  | Event (_,(_,(x,y,_)),_) -> (x,y)
  | Init y -> (y,[])
  | Obs (_,_,_) -> ([],[])
  | Dummy _ -> ([],[])

let check_create_quarks aid sites quarks =
  List.for_all
    (fun (site,internal) ->
      match internal with
      | Some _ -> ((List.mem (2,(aid,site,0)) quarks)&&
                     (List.mem (2,(aid,site,1)) quarks))
      | None -> (List.mem (2,(aid,site,1)) quarks)) sites

let check_modified_quarks ((aid,_),site) modif quarks =
  List.exists
    (fun (c,(n,s,m)) ->
      ((c=2)||(c=3))&&(n=aid)&&(s=site)&&(m=modif)) quarks

let check_tested_quarks ((aid,_),site) modif quarks =
  List.exists
    (fun (c,(n,s,m)) ->
      ((c=1)||(c=3))&&(n=aid)&&(s=site)&&(m=modif)) quarks

let check_event_quarks actions tests quarks =
  (List.for_all
    (function
     | Instantiation.Create ((aid,_),sites) ->
        check_create_quarks aid sites quarks
     | Instantiation.Free asite ->
        check_modified_quarks asite 1 quarks
     | Instantiation.Bind_to (asite1,asite2)
       | Instantiation.Bind (asite1,asite2) ->
        (check_modified_quarks asite1 1 quarks)&&
          (check_modified_quarks asite2 1 quarks)
     | Instantiation.Mod_internal (asite,_) ->
        check_modified_quarks asite 0 quarks
     | Instantiation.Remove (aid,_) ->
        List.exists
          (fun (c,(n,_,_)) ->
            ((c=2)||(c=3))&&(n=aid)) quarks)
    actions)&&
  (List.for_all
    (function
     | Instantiation.Is_Here (aid,_) ->
        List.exists
          (fun (c,(n,_,_)) ->
            ((c=1)||(c=3))&&(n=aid)) quarks
     | Instantiation.Has_Internal (asite,_) ->
        check_tested_quarks asite 0 quarks
     | Instantiation.Is_Free asite | Instantiation.Is_Bound asite
       | Instantiation.Has_Binding_type (asite,_) ->
        check_tested_quarks asite 1 quarks
     | Instantiation.Is_Bound_to (asite1,asite2) ->
        (check_tested_quarks asite1 1 quarks)&&
          (check_tested_quarks asite2 1 quarks))
    tests)

let log_event id quarks event_kind steps =
  match event_kind with
  | INIT _ ->
     let stp =
       List.find
         (function
          | Init actions ->
             List.for_all
               (function
                | Instantiation.Create ((aid,_),sites) ->
                   check_create_quarks aid sites quarks
                | Instantiation.Free _ | Instantiation.Bind_to _
                  | Instantiation.Bind _-> true
                | Instantiation.Mod_internal _ | Instantiation.Remove _ ->
                   raise (ExceptionDefn.Internal_Error
                            (Location.dummy_annot
                               "init event has actions not allowed"))) actions
          | Event _ | Obs _ | Subs _ | Dummy _ -> false) steps in
     `List [`Int id; step_to_yojson stp ]
  | RULE rid ->
     let stp =
       List.find
         (function
          | Event (ekind,(tests,(actions,_,_)),_) ->
             (match ekind with
              | RULE rid' ->
                 ((rid=rid')&&(check_event_quarks actions tests quarks))
             | PERT _ | OBS _ | INIT _ -> false)
          | Obs _ | Subs _ | Dummy _ | Init _ -> false) steps in
     `List [`Int id; step_to_yojson stp]
  | OBS _ ->
     let stp =
       List.find
         (function
          |  Obs _ -> true
          | Subs _ | Dummy _ | Init _ | Event _ -> false) steps in
     `List [`Int id; step_to_yojson stp]
  | PERT pert ->
     let stp =
       List.find
         (function
          | Event (ekind,(tests,(actions,_,_)),_) ->
             (match ekind with
              | PERT pert' ->((pert=pert')&&
                                (check_event_quarks actions tests quarks))
              | OBS _ | INIT _ | RULE _ -> false)
          | Obs _ | Subs _ | Dummy _ | Init _ -> false) steps in
     `List [`Int id; step_to_yojson stp]
