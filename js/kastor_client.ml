(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type state = {
  running : bool;
  progress : Story_json.progress_bar option;
  log : string list;
  stories :
    (unit Trace.Simulation_info.t list list * Graph_loggers_sig.graph)
      Mods.IntMap.t
}

let state_eq a b =
  a.running = b.running &&
  Option_util.equal (=) a.progress b.progress &&
  List.for_all2 String.equal a.log b.log &&
  Mods.IntMap.equal (=) a.stories b.stories

let initial_state = {
  running = false;
  progress = None;
  log = [];
  stories = Mods.IntMap.empty;
}

let controller s = function
  | Story_json.Progress p -> {
      running = s.running; progress = Some p; log = s.log; stories = s.stories;
    }
  | Story_json.Phase (Story_json.Start,m) -> {
      running = true; progress = None; log = [m]; stories = Mods.IntMap.empty;
    }
  | Story_json.Phase (Story_json.Inprogress,m) -> {
      running = s.running;
      progress = s.progress;
      log = m::s.log;
      stories = s.stories;
    }
  | Story_json.Phase (Story_json.Faillure,m) -> {
      running = s.running;
      progress = s.progress;
      log = m::s.log;
      stories = s.stories;
    }
  | Story_json.Phase (Story_json.Success,m) -> {
      running = false; progress = s.progress; log = m::s.log; stories = s.stories;
    }
  | Story_json.Story c ->
    match c.Story_json.story with
    | Story_json.New e -> {
        running = s.running;
        progress = s.progress;
        log = s.log;
        stories =
          Mods.IntMap.add
            e.Story_json.id
            ([c.Story_json.log_info],e.Story_json.graph)
            s.stories;
      }
    | Story_json.Same_as i ->
        match Mods.IntMap.find_option i s.stories with
          | Some (infos,graph) ->
            {
              running = s.running;
              progress = s.progress;
              log = s.log;
              stories =
                Mods.IntMap.add
                  i (c.Story_json.log_info::infos,graph) s.stories;
            }
          | None -> assert false

let current_state,set_state = React.S.create ~eq:state_eq initial_state

let receive x =
  set_state (controller (React.S.value current_state)
               (Story_json.message_of_json (Yojson.Basic.from_string x)))

class virtual new_client =
  object(self)
    method virtual post : string -> unit
    method virtual is_running : bool

    method config ~none ~weak ~strong =
      if self#is_running then
        let () = self#post
            (Yojson.Basic.to_string
               (`List [ `String "CONFIG";
                        `Assoc [ "none", `Bool none;
                                 "weak", `Bool weak;
                                 "strong", `Bool strong]])) in
        Lwt.return_ok ()
      else
        Lwt.return_error "KaStor agent is dead"
    method private raw_run trace_text =
      if self#is_running then
        let () = self#post ("[\"RUN\", "^trace_text^"]") in
        Lwt.return_ok ()
      else
        Lwt.return_error "KaStor agent is dead"
  end
