(* Js util *)
(* references
   http://toss.sourceforge.net/ocaml.html
   http://peppermint.jp/temp/ao/ao.ml
*)
open Js
module Html5 = Tyxml_js.Html5

let prototype = Js.Unsafe.js_expr "CodeMirror.prototype"
let create_handler label =
  let head : char = Char.uppercase (String.get label 0) in
  let tail : string = String.sub label 1 ((String.length label) -1) in
  let on_label = "on"^(Char.escaped head)^tail in
  let wrapper handler = (Js.Unsafe.variable "this")##on(label,handler) in
  let () = Js.Unsafe.set prototype
                         (Js.string on_label)
                         wrapper
  in
  ()
(* add on handlers to the prototypes so they can be strongly typed *)
let () = List.iter create_handler ["beforeChange";"beforeCursorEnter"
                                  ;"beforeSelectionChange";"blur"
                                  ;"changes";"clear"
                                  ;"contextmenu";"copy"
                                  ;"cursorActivity";"cut"
                                  ;"dblclick";"delete"
                                  ;"dragenter";"dragleave"
                                  ;"dragover";"dragstart"
                                  ;"drop";"electricInput"
                                  ;"focus";"gutterClick"
                                  ;"gutterContextMenu";"hide"
                                  ;"inputRead";"keyHandled"
                                  ;"keypress";"keyup"
                                  ;"mousedown";"paste"
                                  ;"redraw";"renderLine"
                                  ;"scroll";"scrollCursorIntoView"
                                  ;"swapDoc";"touchstart"
                                  ;"unhide";"update"
                                  ;"viewportChange";"change"
                                  ;"keydown"]
class type lint_configuration = object
                                  val delay : int Js.t Js.prop
                                  val async : bool Js.t Js.prop
                                end
let constructor_lint_configuration : lint_configuration Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_lint_configuration () : lint_configuration Js.t  =
  jsnew constructor_lint_configuration ()

class type configuration =
    object
      (* The starting value of the editor. *)
      val value: Js.js_string Js.t Js.prop
      (* The mode to use. *)
      val mode : Js.js_string Js.t Js.prop
      (* Explicitly set the line separator for the editor.  *)
      val lineSeparator : Js.js_string Js.opt Js.t Js.prop
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
      val specialCharPlaceholder: (int -> Dom_html.element Js.t) Js.prop
      (* Horizontal cursor movement through right-to-left. *)
      val rtlMoveVisually: bool Js.t Js.prop
      (* Configures the key map to use. *)
      val keyMap: string Js.t Js.prop
      (* specify extra key bindings for the editor *)
      val extraKeys : 'a Js.prop
      (* Scroll or wrap for long lines *)
      val lineWrapping: bool Js.t Js.prop
      (*  Show line numbers to the left of the editor *)
      val lineNumbers : bool Js.t Js.prop
      (* A function used to format line numbers. *)
      val lineNumberFormatter: (int Js.t -> Js.js_string) Js.t Js.prop
      (* Add extra gutters *)
      val gutters : Js.string_array Js.t Js.prop
      (* Gutter scrolls along with the content horizontally *)
      val fixedGutter: bool Js.t Js.prop
      (* Chooses a scrollbar implementation. *)
      val scrollbarStyle: string Js.t Js.prop
      (* cover gutter with with class CodeMirror-gutter-filler. *)
      val coverGutterNextToScrollbar: bool Js.t Js.prop
      (* Selects the way CodeMirror handles input and focus. *)
      val inputStyle: string Js.t Js.prop
      (* disable editing of the editor content *)
      val readOnly: bool Js.t Js.prop
      (* the cursor should be drawn when a selection is active. *)
      val showCursorWhenSelecting: bool Js.t Js.prop
      (* copy or cut when there is no selection will copy or cut whole lines *)
      val lineWiseCopyCut: bool Js.t Js.prop
      (* maximum number of undo *)
      val undoDepth: int Js.t Js.prop
      (* milliseconds of inactivity to create a new history event *)
      val historyEventDelay: int Js.t Js.prop
      (* tab index of editor *)
      val tabindex: int Js.t Js.prop
      (* CodeMirror focus itself on initialization *)
      val autofocus: bool Js.t Js.prop
      (* enable drag-and-drop *)
      val dragDrop: bool Js.t Js.prop
      (* when set files wit mime type can be dropped into the editor *)
      val allowDropFileTypes: Js.string_array Js.opt Js.t Js.prop
      (* Half-period in milliseconds used for cursor blinking. *)
      val cursorBlinkRate: int Js.t Js.prop
      (* How much extra space to always keep above and below the cursor *)
      val cursorScrollMargin: int Js.t Js.prop
      (* Determines the height of the cursor. *)
      val cursorHeight: int Js.t Js.prop
      (* the context menu is opened with a click outside of the
         current selection, move cursor to the point of the click*)
      val resetSelectionOnContextMenu: bool Js.t Js.prop
      (* time to run highlighting thread *)
      val workTime: int Js.t Js.prop
      (* delay to run highlighting thread *)
      val workDelay: int Js.t Js.prop
      (* how often to poll for changes *)
      val pollInterval: int Js.t Js.prop
      (* combine tokens to a single span *)
      val flattenSpans: bool Js.t Js.prop
      (* prefix css *)
      val addModeClass: bool Js.t Js.prop
      (* length to highlight *)
      val maxHighlightLength: bool Js.t Js.prop
      (* amount of lines that are rendered above and below the
         visible document*)
      val viewportMargin: int Js.t Js.prop
      val lint : lint_configuration Js.t Js.prop
end
let constructor_configuration : configuration Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_configuration () : configuration Js.t  =
  jsnew constructor_configuration ()

class type position = object
                        val ch : int Js.t Js.prop
                        val line : int Js.t Js.prop
                      end

let constructor_position : position Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_position ~(ch : int) ~(line : int) : position Js.t  =
  let result = jsnew constructor_position () in
  let () = (Js.Unsafe.coerce result)##ch <- ch in
  let () = (Js.Unsafe.coerce result)##line <- line in
  result

class type change = object
                      method from : position Js.t Js.prop
                      method to_ : position Js.t Js.prop
                      method text : Js.string_array Js.t Js.prop
                      method removed : string Js.t Js.prop
                      method origin : string Js.t Js.prop
                    end;;

let constructor_change : change Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_change () : change Js.t  = jsnew constructor_change ()

type severity = Error | Warning
class type lint = object
                    val message: Js.js_string Js.t Js.prop
                    val severity: Js.js_string Js.t Js.prop
                    val from : position Js.t Js.prop
                    val to_ : position Js.t Js.prop
                  end

let constructor_lint : lint Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_lint ~(message : string)
                ~(severity : severity)
                ~(from : position Js.t)
                ~(to_ :  position Js.t) : lint Js.t  =
  let result = jsnew constructor_lint () in
  let () = (Js.Unsafe.coerce result)##message <- message in
  let () = (Js.Unsafe.coerce result)##severity <- match severity with
                                                    Error -> Js.string "error"
                                                  | Warning -> Js.string "warning"
  in
  let () = (Js.Unsafe.coerce result)##from <- from in
  let () = (Js.Unsafe.coerce result)##to_ <- to_ in
  result

class type codemirror =
object
  method getValue : Js.js_string Js.t meth
  method setValue : Js.js_string Js.t -> unit meth
  method focus    : unit Js.t Js.meth

  method on : (Js.js_string Js.t) -> (Dom_html.event Js.t -> unit) -> unit Js.meth
  (* fired when content is changed *)
  method onChange : (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth
  (* batched changed operation *)
  method onChanges : (codemirror Js.t -> change Js.js_array Js.t -> unit) -> unit Js.meth
  (* before a change is applied *)
  method onBeforeChange : (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth
  (* cursor moves, or any change *)
  method onCursorActivity : (codemirror Js.t -> unit) -> unit Js.meth
  (* when new input is read *)
  method onKeyHandled : (codemirror Js.t -> Js.js_string -> Dom_html.event -> unit) -> unit Js.meth
  (* when new input is read *)
  method onInputRead : (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth
  (* when text matches electric pattern *)
  method onElectricInput : (codemirror Js.t -> int Js.t -> unit) -> unit Js.meth
  (* before a selection is moved TODO *)
  method onBeforeSelectionChange : (codemirror Js.t -> 'a -> unit) -> unit Js.meth
  (* the view port changed *)
  method onViewportChange : (codemirror Js.t -> int Js.t -> int Js.t -> unit) -> unit Js.meth
  (*  document swapped *)
  method onSwapDoc : (codemirror Js.t -> 'a -> unit) -> unit Js.meth
  (* gutter clicked *)
  method onGutterClick : (codemirror Js.t -> int Js.t -> Js.js_string -> Dom_html.event Js.t -> unit) -> unit Js.meth
  (* context menu event from gutter *)
  method onGutterContextMenu : (codemirror Js.t -> int Js.t -> Js.js_string -> Dom_html.event Js.t -> unit) -> unit Js.meth
  (* focus *)
  method onFocus : (codemirror Js.t -> unit) -> unit Js.meth
  (* blur *)
  method onBlur : (codemirror Js.t -> unit) -> unit Js.meth
  (* scroll *)
  method onScroll : (codemirror Js.t -> unit) -> unit Js.meth
  (* cursor scrolled in view*)
  method onScrollCursorIntoView : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  (* dom updated *)
  method onUpdate : (codemirror Js.t -> unit) -> unit Js.meth
  (* line rendered *)
  method onRenderLine : (codemirror Js.t -> 'a -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onMousedown : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDblclick : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onTouchstart : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onContextmenu : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onKeydown : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onKeypress : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onKeyup : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onCut : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onCopy : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onPaste : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDragstart : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDragenter : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDragover : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDragleave : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDrop : (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth
  method onDelete : (unit -> unit) -> unit Js.meth
  method onBeforeCursorEnter : (unit -> unit) -> unit Js.meth
  method onClear : (position Js.t -> position Js.t -> unit) -> unit Js.meth
  method onHide : (unit -> unit) -> unit Js.meth
  method onUnhide : (unit -> unit) -> unit Js.meth
  method onRedraw : (unit -> unit) -> unit Js.meth

end;;

let fromTextArea
      (dom : Dom_html.element Js.t)
      (configuration : configuration Js.t)
    : codemirror Js.t =
  (* let () = Js.debugger() in *)
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "CodeMirror")##fromTextArea
    [| Js.Unsafe.inject dom ; Js.Unsafe.inject configuration |]
