(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module UIState = Ui_state
module Html = Tyxml_js.Html5
open Lwt.Infix

let navli (_ : Ui_simulation.t) = []

let navcontent (t : Ui_simulation.t) =
  let state_log , set_state_log =
    React.S.create ("" : string)
  in
  let simulation_output = (Ui_simulation.simulation_output t) in
  let _ = React.S.l1
      (fun _ ->
         Ui_simulation.manager_operation
           t
           (fun
             manager
             project_id
             simulation_id ->
             (Api_v1.assemble_log_message manager project_id simulation_id)
             >>=
             (Api_common.result_map
                ~ok:(fun _ (log_messages : Api_types_j.log_message list) ->
                    let () = set_state_log (String.concat "\n" log_messages) in
                    Lwt.return_unit)
                ~error:(fun _ errors  ->
                    let () = Ui_state.set_model_error __LOC__ errors in
                    Lwt.return_unit)
             )
           )
      )
      simulation_output
  in
    [ Html.div
      ~a:[Html.a_class ["panel-pre" ]]
      [ Tyxml_js.R.Html.pcdata state_log ]
  ]
let onload (_ : Ui_simulation.t) = ()
let onresize (_ : Ui_simulation.t) : unit = ()
