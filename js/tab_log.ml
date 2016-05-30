module UIState = Ui_state
module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5

let navli = []

let navcontent =
  [ Html5.div
      ~a:[Html5.a_class ["panel-pre" ]]
      [ Tyxml_js.R.Html5.pcdata
          (React.S.bind
             UIState.model_runtime_state
             (fun state -> React.S.const
               (match (state : ApiTypes.state option) with
                 Some state -> String.concat "" state.ApiTypes.log_messages
               | _ -> ""
               ))) ]
  ]
let onload () = ()
