(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let working_set_gutter = "working-set-checkbox-gutter"
let rule_enabled_checkbox = "menu-editor-rule-enabled-checkbox"

let with_file
    (handler : (string * string * bool) Api.result -> unit Api.lwt_result) =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__ (State_file.get_file () >>= handler) >>= fun _ ->
      Lwt.return_unit)

let set_content ~(filename : string) ~(filecontent : string) : unit =
  with_file
    (Api_common.result_bind_with_lwt ~ok:(fun (_, current_filename, _) ->
         if filename = current_filename then
           State_file.set_content filecontent >>= fun r ->
           State_project.sync () >>= fun r' ->
           Lwt.return (Api_common.result_combine [ r; r' ])
         else (
           let msg =
             Format.sprintf "file name mismatch %s %s" filename current_filename
           in
           Lwt.return (Api_common.err_result_of_string msg)
         )))


let enable_or_disable_rule rule_id (enable : bool) =
  let rule_id = int_of_string rule_id in
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( State_project.eval_with_project ~label:__LOC__ (fun manager ->
              manager#enable_or_disable_rule rule_id enable)
        >>= fun r ->
          State_project.sync_no_compilation () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)

let rule_checkbox rule_id is_checked =
  let checked_attribute =
    if is_checked then
      [ Html.a_checked () ]
    else
      []
  in
  Html.input
    ~a:
      ([
         Html.a_input_type `Checkbox;
         Html.a_class [ rule_enabled_checkbox ];
         Html.a_user_data "rule-id" rule_id;
       ]
      @ checked_attribute)
    ()

let add_checkbox_to_working_set_rules (cm : Codemirror.codemirror Js.t)
    (rules : Public_data.working_set_rules) (gutter_id : string) : unit =
  let () = Codemirror.clearGutter ~cm ~gutter_id in
  let build_checkbox rule_id enabled =
    Tyxml_js.To_dom.of_element
      (Html.div
         ~a:
           [
             Html.a_class [ "checkbox-control-div" ];
             Html.a_user_data "rule-id" rule_id;
           ]
         [ rule_checkbox rule_id enabled ])
  in
  let add_widget (rule : Public_data.rule_in_working_set) : unit =
    let range = rule.Public_data.rule_ws_position in
    match React.S.value State_file.current_filename with
    | None -> ()
    | Some file_id ->
      if range.Loc.file = file_id then
        Codemirror.setGutterMarker
          ~line:(range.Loc.from_position.Loc.line - 1)
          ~cm
          ~dom:
            (build_checkbox
               ("ws-rule-" ^ string_of_int rule.Public_data.rule_ws_id)
               rule.Public_data.rule_ws_enabled)
          ~gutter_id
      else
        ()
  in
  List.iter (fun r -> add_widget r) rules

let set_working_set_rules codemirror =
  State_project.on_project_change_async ~on:(React.S.const true) () (React.S.const ()) ()
    (fun (manager : Api.concrete_manager) () ->
      let () = Js.Unsafe.global##.process##.stdout##write (Js.string ("UPDATING THE CHECKBOXES\n")) in
      manager#get_working_set_rules >|= fun x ->
      match x.Result_util.value with
      | Result.Ok rules ->
        let () =
          add_checkbox_to_working_set_rules codemirror rules working_set_gutter
        in
        ()
      | Result.Error _ -> ())
  |> ignore
