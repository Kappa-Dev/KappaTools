module UIState = Ui_state
module ApiTypes = Api_types_v1_j
module Html = Tyxml_js.Html5

let navli (_ : Ui_simulation.t) = []

let state_log
    (state : ApiTypes.state option) : string list =
  match state with
  | None -> []
  | Some state -> state.ApiTypes.log_messages

let navcontent (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  [ Html.div
      ~a:[Html.a_class ["panel-pre" ]]
      [ Tyxml_js.R.Html.pcdata
          (React.S.bind
             simulation_output
             (fun state -> React.S.const
		 (String.concat ""
                    (state_log state)
		 )
             )
          )
      ]
  ]
let onload (_ : Ui_simulation.t) = ()
let onresize (_ : Ui_simulation.t) : unit = ()
