module UIState = Ui_state
module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5

let navli = []

let state_log
    (state : ApiTypes.state option) : string list =
  match state with
  | None -> []
  | Some state -> state.ApiTypes.log_messages

let navcontent =
  [ Html.div
      ~a:[Html.a_class ["panel-pre" ]]
      [ Tyxml_js.R.Html.pcdata
          (React.S.bind
             UIState.model_runtime_state
             (fun state -> React.S.const
		 (String.concat ""
                    (state_log state)
		 )
             )
          )
      ]
  ]
let onload () = ()
