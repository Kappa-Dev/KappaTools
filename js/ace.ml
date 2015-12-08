class type t = object end

let create s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr
       "ace")##edit
    [| Js.Unsafe.inject s |]

let get_editor_value editor =
  Js.Unsafe.fun_call
    (Js.Unsafe.coerce editor)##getValue
    [| Js.Unsafe.inject () |]

let set_editor_value editor str =
  Js.Unsafe.fun_call
    (Js.Unsafe.coerce editor)##setValue
    [| Js.Unsafe.inject str |]

let starter editor x =
  (*  let script = Dom_html.createScript Dom_html.document in
  let () =
    script##text <- Js.string "var editor = ace.edit(\"editor\");
editor.setTheme(\"ace/theme/eclipse\");" in*)
  Js.Unsafe.fun_call
    (Js.Unsafe.coerce editor)##on
    [|Js.Unsafe.inject (Js.string "change");
      Js.Unsafe.inject
	(Dom_html.handler (fun _ ->
			   let () = x := true in
			   let () = Storage.set_model_text
				      (Js.to_string (get_editor_value editor)) in
			   Js._true))|]
