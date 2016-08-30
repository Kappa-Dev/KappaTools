module Html = Tyxml_js.Html5

let navli = []

let rightsubpanel =
  Html.div
    ~a:[Html.a_class ["col-md-6"]]
    [Ui_common.navtabs "subnavtab"
       [ "contact", Tab_contact.navli
       ; "log",     Tab_log.navli ];
     Ui_common.navcontent
       [ "contact", Tab_contact.navcontent
       ; "log",     Tab_log.navcontent ]]

let navcontent =
  [Html.div ~a:[Html.a_class ["row"]]
    [Html.div
       ~a:[Html.a_class ["col-md-6"]] Subpanel_editor.xml;
    rightsubpanel]]

let onload () =
  let () = Subpanel_editor.onload () in
  let () = Tab_contact.onload () in
  let () = Tab_log.onload () in
  ()

let onunload () =
  Subpanel_editor.onunload ()
