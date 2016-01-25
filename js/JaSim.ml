open Js

open Lwt
open Code
open Visualization
open Codemirror

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let main_container = <:html5<<div class="row">
                                     $Code.code$
                                     $visualization_column$
                             </div> >>

let onload _ =
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
               (fun () -> assert false) in
  let skeleton = Tyxml_js.To_dom.of_div main_container in
  let () = Dom.appendChild main skeleton;
           Code.onload ();
  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload
