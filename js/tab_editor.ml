module Html = Tyxml_js.Html5

let navli (_ : Ui_simulation.t) = []
let rightsubpanel_id : string = "rightsubpanel"
let rightsubpanel (t : Ui_simulation.t) =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (React.S.bind
              Ui_state.editor_full
              (fun editor_full ->
                 React.S.const
                   (if editor_full then
                      ["hidden"]
                    else
                      ["col-md-6"])
              )
           )
       ]
    [Ui_common.navtabs "subnavtab"
       [ "contact", (Tab_contact.navli t)
       ; "log",     (Tab_log.navli t) ];
     Ui_common.navcontent
       ~id:rightsubpanel_id
       []
       [ "contact", (Tab_contact.navcontent t)
       ; "log",     (Tab_log.navcontent t) ]]

let navcontent (t : Ui_simulation.t) =
  [Html.div ~a:[Html.a_class ["row"]]
     [Html.div
        ~a:[ Tyxml_js.R.Html.a_class
               (React.S.bind
                  Ui_state.editor_full
                  (fun editor_full ->
                     React.S.const
                       (if editor_full then
                          ["col-md-12"]
                        else
                          ["col-md-6"])
                  )
               )
           ]
        (Subpanel_editor.xml t);
      (rightsubpanel t) ]]

let editor_full , set_editor_full =
  React.S.create (false : bool)



let onload (t : Ui_simulation.t) =
  let () = Subpanel_editor.onload t in
  let () = Tab_contact.onload t in
  let () = Tab_log.onload t in
  ()

let onunload () =
  Subpanel_editor.onunload ()
let onresize (_ : Ui_simulation.t) : unit = ()
