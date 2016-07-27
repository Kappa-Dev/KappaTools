open Panel_editor

module Html5 = Tyxml_js.Html5
open Js

module Html = Tyxml_js.Html5

let prod _ =
  let%html main_container = {|<div class="row">
                            |}[Panel_editor.xml;
                               Panel_tab.xml]{|
                             </div>|}
  in
  let main =
    Js.Opt.get (Ui_common.document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  let skeleton = Tyxml_js.To_dom.of_div main_container in
  let () = Dom.appendChild main skeleton in
  let _ = Panel_editor.onload ();
    Panel_tab.onload ()
  in Js._true

let dev _ =
  let configuration =
    JsNode.create_process_configuration
      ~onStdout:(Some (fun msg -> Common.debug msg))
      ~onStderr:(Some (fun msg -> Common.debug msg))
      "cat"
      []
      ~onClose:(Some (fun () -> Common.debug "close"))
  in
  let%html main_container = {|<div class="row">
                            |}[Panel_editor.xml;
                               Panel_tab.xml]{|
                             </div>|}
  in
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
      (fun () -> assert false)
  in
  let skeleton = Tyxml_js.To_dom.of_div main_container in
  let () = Dom.appendChild main skeleton in
  let _ =
    Panel_editor.onload ();
    Panel_tab.onload ()
  in Js._true

let onunload _ =
  let () = Panel_editor.onunload () in
  let () = Panel_tab.onunload () in
  Js._true

let _ = Dom_html.window##.onbeforeunload :=
    Dom_html.handler
      onunload

let _ = Dom_html.window##.onload :=
    Dom_html.handler
      (Ui_common.version
	 ~prod:prod
	 ~dev:dev)
