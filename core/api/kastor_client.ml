(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let init_compression_mode = function
  | Story_json.Causal ->
    { Api.causal = true; Api.weak = false; Api.strong = false }
  | Story_json.Weak ->
    { Api.causal = false; Api.weak = true; Api.strong = false }
  | Story_json.Strong ->
    { Api.causal = false; Api.weak = false; Api.strong = true }

let add_compression_mode { Api.causal; Api.weak; Api.strong } = function
  | Story_json.Causal -> { Api.causal = true; Api.weak; Api.strong }
  | Story_json.Weak -> { Api.causal; Api.weak = true; Api.strong }
  | Story_json.Strong -> { Api.causal; Api.weak; Api.strong = true }

let print_compression_modes f { Api.causal; Api.weak; Api.strong } =
  let () = if causal then Format.pp_print_string f "CAUSAL" in
  let () =
    if weak then
      Format.pp_print_string f
        (if causal then
           ", WEAK"
         else
           "WEAK")
  in
  if strong then
    Format.pp_print_string f
      (if causal || weak then
         ", STRONG"
       else
         "STRONG")

type state_t = {
  running: bool;
  progress: Story_json.progress_bar option;
  log: string list;
  stories:
    (Api.compression_modes
    * unit Trace.Simulation_info.t list list
    * Graph_loggers_sig.graph)
    Mods.IntMap.t;
}

type state = state_t ref

(*let state_eq a b =
  a.running = b.running &&
  Option_util.equal (=) a.progress b.progress &&
  (try List.for_all2 (fun x y -> String.compare x y = 0) a.log b.log
   with Invalid_argument _ -> false)
  && Mods.IntMap.equal (=) a.stories b.stories*)

let initial_state =
  { running = false; progress = None; log = []; stories = Mods.IntMap.empty }

let controller s = function
  | Story_json.Progress p ->
    { running = s.running; progress = Some p; log = s.log; stories = s.stories }
  | Story_json.Phase (Story_json.Start, m) ->
    {
      running = true;
      progress = None;
      log = [ m ];
      stories = Mods.IntMap.empty;
    }
  | Story_json.Phase (Story_json.Inprogress, m) ->
    {
      running = s.running;
      progress = s.progress;
      log = m :: s.log;
      stories = s.stories;
    }
  | Story_json.Phase (Story_json.Faillure, m) ->
    {
      running = s.running;
      progress = s.progress;
      log = m :: s.log;
      stories = s.stories;
    }
  | Story_json.Phase (Story_json.Success, m) ->
    {
      running = false;
      progress = s.progress;
      log = m :: s.log;
      stories = s.stories;
    }
  | Story_json.Story c ->
    (match c.Story_json.story with
    | Story_json.New e ->
      {
        running = s.running;
        progress = s.progress;
        log = s.log;
        stories =
          Mods.IntMap.add e.Story_json.id
            ( init_compression_mode c.Story_json.story_mode,
              [ c.Story_json.log_info ],
              e.Story_json.graph )
            s.stories;
      }
    | Story_json.Same_as i ->
      (match Mods.IntMap.find_option i s.stories with
      | Some (cm, infos, graph) ->
        {
          running = s.running;
          progress = s.progress;
          log = s.log;
          stories =
            Mods.IntMap.add i
              ( add_compression_mode cm c.Story_json.story_mode,
                c.Story_json.log_info :: infos,
                graph )
              s.stories;
        }
      | None -> assert false))

let receive update_state x =
  update_state (Story_json.message_of_json (Yojson.Basic.from_string x))

let init_state () =
  let current_state = ref initial_state in
  current_state, fun x -> current_state := controller !current_state x

class virtual new_client ~post current_state =
  object (self)
    method virtual is_running : bool

    method config_story_computation { Api.causal; Api.weak; Api.strong } =
      if self#is_running then (
        let () =
          post
            (Yojson.Basic.to_string
               (`List
                 [
                   `String "CONFIG";
                   `Assoc
                     [
                       "none", `Bool causal;
                       "weak", `Bool weak;
                       "strong", `Bool strong;
                     ];
                 ]))
        in
        Lwt.return_ok ()
      ) else
        Lwt.return_error "KaStor agent is dead"

    method raw_launch_story_computation trace_text =
      if self#is_running then (
        let () = current_state := { initial_state with running = true } in
        let () = post ("[\"RUN\", " ^ trace_text ^ "]") in
        Lwt.return_ok ()
      ) else
        Lwt.return_error "KaStor agent is dead"

    method story_log = !current_state.log
    method story_is_computing = !current_state.running
    method story_progress = !current_state.progress
    method story_list = !current_state.stories
  end
