module Html = Tyxml_js.Html5

let navli (_ : Ui_simulation.t) = []
let rightsubpanel_id : string = "rightsubpanel"
let rightsubpanel (t : Ui_simulation.t) =
  Html.div
    ~a:[Html.a_class ["col-md-6"]]
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
        ~a:[Html.a_class ["col-md-6"]]
        (Subpanel_editor.xml t);
      (rightsubpanel t) ]]

let onload (t : Ui_simulation.t) =
  let () = Subpanel_editor.onload t in
  let () = Tab_contact.onload t in
  let () = Tab_log.onload t in
  ()

let onunload () =
  Subpanel_editor.onunload ()
let onresize (_ : Ui_simulation.t) : unit = ()
