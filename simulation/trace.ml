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
  unit Mods.simulation_info
type obs =
  event_kind *
  Instantiation.concrete Instantiation.test list *
  unit Mods.simulation_info
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

let dummy = {
  Mods.story_id = 0 ; Mods.story_time = 0. ;
  Mods.story_event = 0 ; Mods.profiling_info = ();
}
let simulation_info_of_step = function
  | Obs (_,_,info) | Event (_,_,info) -> Some info
  | Init _ -> Some dummy
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

let store_event event step_list =
  match event with
  | INIT _,(_,(actions,_,_)),_ -> (Init actions)::step_list
  | OBS _,_,_ -> assert false
  | (RULE _ | PERT _ as k),x,info -> (Event (k,x,info))::step_list
let store_obs (i,x,c) step_list = Obs(i,x,c)::step_list
