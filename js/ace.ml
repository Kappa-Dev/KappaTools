let get_editor_value () =
  let res = Js.Unsafe.fun_call
    (Js.Unsafe.variable
       "editor")##getValue
    [| Js.Unsafe.inject () |] in
  (*Js.to_string*) res

let set_editor_value str =
  ignore (Js.Unsafe.fun_call
    (Js.Unsafe.variable
       "editor")##setValue
    [| Js.Unsafe.inject str |])

let starter =
  let script = Dom_html.createScript Dom_html.document in
  let () =
    script##text <- Js.string "var editor = ace.edit(\"editor\");
editor.setTheme(\"ace/theme/eclipse\");" in
  script

