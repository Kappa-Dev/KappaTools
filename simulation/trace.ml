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

  let current c =
  { story_id = Counter.current_story c;
    story_time = Counter.current_time c;
    story_event = Counter.current_event c;
    profiling_info = (); }
  let next_story c =
    let () = Counter.inc_stories c in
    current c

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

let step_to_json = function
  | Subs (a,b) -> `List [`String "Subs"; `Int a; `Int b]
  | Event (x,y,z) ->
    `List [`String "Event";
           event_kind_to_json x;
           Instantiation.event_to_json Edges.agent_to_json y;
           Simulation_info.to_json (fun () -> `Null) z]
  | Init a ->
    `List
      [`String "Init";
       `List (List.map (Instantiation.action_to_json Edges.agent_to_json) a)]
  | Obs (x,y,z) ->
    `List [`String "Obs";
           event_kind_to_json x;
           `List (List.map (Instantiation.test_to_json Edges.agent_to_json) y);
           Simulation_info.to_json (fun () -> `Null) z]
  | Dummy _ -> `Null
let step_of_json = function
  | `List [`String "Subs"; `Int a; `Int b] -> Subs (a,b)
  | `List [`String "Event";x;y;z] ->
    Event (event_kind_of_json x,
           Instantiation.event_of_json Edges.agent_of_json y,
           Simulation_info.of_json (function _ -> ()) z)
  | `List [`String "Init"; `List l ] ->
    Init (List.map (Instantiation.action_of_json Edges.agent_of_json) l)
  | `List [`String "Obs"; x; `List l; z] ->
    Obs (event_kind_of_json x,
         List.map (Instantiation.test_of_json Edges.agent_of_json) l,
         Simulation_info.of_json (function _ -> ()) z)
  | `Null -> Dummy ""
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect trace step",x))

let to_json t = `List (List.map step_to_json t)
let of_json = function
  | `List l -> List.map step_of_json l
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

let store_event counter event step_list =
  match event with
  | INIT _,(_,(actions,_,_)) -> (Init actions)::step_list
  | OBS _,_ -> assert false
  | (RULE _ | PERT _ as k),x ->
    (Event (k,x,Simulation_info.current counter))::step_list
let store_obs counter (i,x) step_list =
  Obs(i,x,Simulation_info.next_story counter)::step_list
