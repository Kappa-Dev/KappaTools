(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let navli () = []

let content () =
  let state_log , set_state_log =
    React.S.create ("" : string)
  in
  let _ = React.S.l1
      (fun _ ->
         State_simulation.when_ready
           ~label:__LOC__
           (fun
             manager
             project_id
             simulation_id ->
             (manager#simulation_detail_log_message
                project_id simulation_id)
             >>=
             (Api_common.result_bind_lwt
                ~ok:(fun (log_messages : Api_types_j.log_message) ->
                    let () = set_state_log log_messages in
                    Lwt.return (Api_common.result_ok ()))
             )
           )
      )
      State_simulation.model
  in
    [ Html.div
      ~a:[Html.a_class ["panel-pre" ]]
      [ Tyxml_js.R.Html.pcdata state_log ]
  ]
let onload () = ()
let onresize () : unit = ()
