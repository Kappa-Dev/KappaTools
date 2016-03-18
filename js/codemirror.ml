(* Js util *)
(* references
   http://toss.sourceforge.net/ocaml.html
   http://peppermint.jp/temp/ao/ao.ml
*)
open Js
module Html5 = Tyxml_js.Html5

class type event = object
end
class type configuration = object
                             (* The starting value of the editor. *)
                             val value: Js.js_string Js.t Js.prop
                             (* The mode to use. *)
                             val mode : Js.js_string Js.t Js.prop
                             (* Explicitly set the line separator for the editor.  *)
                             val lineSeparator : Js.js_string Js.t Js.prop
                             (* The theme to style the editor with. *)
                             val theme: Js.js_string Js.t Js.prop
                             (* How many spaces a block should be indented.  *)
                             val indentUnit: int Js.t Js.prop
                             (* Whether to use the context-sensitive indentation *)
                             val smartIndent: bool Js.t Js.prop
                             (* The width of a tab character. *)
                             val tabSize: int Js.t Js.prop
                             (* The first N*tabSize in indentation should N tabs. *)
                             val indentWithTabs: bool Js.t Js.prop
                             (* The editor should re-indent the current line. *)
                             val electricChars: bool Js.t Js.prop
                             (* A regular expression used to determine special placeholder. *)
                             val specialChars: Js.regExp Js.t Js.prop
                             (* A function identifies specialChars and produces a DOM node *)
                             (* val specialCharPlaceholder: function(char) → Element *)
                             (* Horizontal cursor movement through right-to-left. *)
                             val rtlMoveVisually: bool Js.t Js.prop
                             (* Configures the key map to use. *)
                             val keyMap: string Js.t Js.prop
                             (* extraKeys: object *)
                             (* val extraKeys: object *)
                             (* Scroll or wrap for long lines *)
                             val lineWrapping: bool Js.t Js.prop
                             (*  Show line numbers to the left of the editor *)
                             val lineNumbers : bool Js.t Js.prop
                             (* A function used to format line numbers. *)
                             val lineNumberFormatter: (int Js.t -> Js.js_string) Js.t Js.prop
                             (* Add extra gutters *)
                             val gutters : Js.string_array Js.t Js.prop
                             (* Gutter scrolls along with the content horizontally *)


end
let constructor_configuration : configuration Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_configuration () : configuration Js.t  = jsnew constructor_configuration ()

class type codemirror = object
 method getValue : Js.js_string Js.t meth
 method setValue : Js.js_string Js.t -> unit meth
 method on : (Js.js_string Js.t) -> (event Js.t -> unit) -> unit Js.meth
end;;

let fromTextArea
      (dom : Dom_html.element Js.t)
      (configuration : configuration Js.t)
    : codemirror Js.t =
  (* let () = Js.debugger() in *)
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "CodeMirror")##fromTextArea
    [| Js.Unsafe.inject dom ; Js.Unsafe.inject configuration |]
