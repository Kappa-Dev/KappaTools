(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* Js util *)
(* references
   http://toss.sourceforge.net/ocaml.html
   http://peppermint.jp/temp/ao/ao.ml
*)
let prototype = Js.Unsafe.js_expr "CodeMirror.prototype"

let create_handler label =
  let head : char = Char.uppercase_ascii (String.get label 0) in
  let tail : string = String.sub label 1 (String.length label - 1) in
  let on_label = "on" ^ Char.escaped head ^ tail in
  let wrapper handler = (Js.Unsafe.pure_js_expr "this")##on label handler in
  let () = Js.Unsafe.set prototype (Js.string on_label) wrapper in
  ()

(* add on handlers to the prototypes so they can be strongly typed *)
let () =
  List.iter create_handler
    [
      "beforeChange";
      "beforeCursorEnter";
      "beforeSelectionChange";
      "blur";
      "changes";
      "clear";
      "contextmenu";
      "copy";
      "cursorActivity";
      "cut";
      "dblclick";
      "delete";
      "dragenter";
      "dragleave";
      "dragover";
      "dragstart";
      "drop";
      "electricInput";
      "focus";
      "gutterClick";
      "gutterContextMenu";
      "hide";
      "inputRead";
      "keyHandled";
      "keypress";
      "keyup";
      "mousedown";
      "paste";
      "redraw";
      "renderLine";
      "scroll";
      "scrollCursorIntoView";
      "swapDoc";
      "touchstart";
      "unhide";
      "update";
      "viewportChange";
      "change";
      "keydown";
    ]

class type position = object
  method ch : int Js.readonly_prop
  method line : int Js.readonly_prop
end

class type dimension = object
  method left : int Js.readonly_prop
  method right : int Js.readonly_prop
  method top : int Js.readonly_prop
  method bottom : int Js.readonly_prop
end

let position : (int -> int -> position Js.t) Js.constr =
  (Js.Unsafe.js_expr "CodeMirror")##._Pos

type severity = Error | Warning

class type lint = object
  method message : Js.js_string Js.t Js.prop
  method severity : Js.js_string Js.t Js.prop
  method from : position Js.t Js.prop
  method to_ : position Js.t Js.prop
end

let constructor_lint : lint Js.t Js.constr = Js.Unsafe.pure_js_expr "Object"

let create_lint ~(message : string) ~(severity : severity)
    ~(from : position Js.t) ~(to_ : position Js.t) : lint Js.t =
  let result = new%js constructor_lint in
  let () = result##.message := Js.string message in
  let () =
    result##.severity :=
      match severity with
      | Error -> Js.string "error"
      | Warning -> Js.string "warning"
  in
  let () = result##.from := from in
  let () = result##.to_ := to_ in
  result

class type change = object
  method from : position Js.t Js.prop
  method to_ : position Js.t Js.prop
  method text : Js.string_array Js.t Js.prop
  method removed : string Js.t Js.prop
  method origin : string Js.t Js.prop
end

let constructor_change : change Js.t Js.constr = Js.Unsafe.pure_js_expr "Object"
let create_change () : change Js.t = new%js constructor_change

class type codemirror = object
  method getValue : Js.js_string Js.t Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method focus : unit Js.meth

  (* Programmatically set the size of the editor (overriding the
     applicable CSS rules). width and height can be either numbers
     (interpreted as pixels) or CSS units ("100%", for example).
     You can pass null for either of them to indicate that that
     dimension should not be changed.
  *)
  method setSize : int Js.t Js.opt -> int Js.t Js.opt -> unit Js.meth

  (* Scroll the editor to a given (pixel) position. Both arguments
     may be left as null or undefined to have no effect. *)
  method scrollTo : int Js.opt -> int Js.opt -> unit Js.meth

  method charCoords :
    position Js.t -> Js.js_string Js.t Js.opt -> dimension Js.t Js.meth

  method getScrollerElement : Dom_html.element Js.t Js.meth
  method on : Js.js_string Js.t -> (Dom_html.event Js.t -> unit) -> unit Js.meth

  (* fired when content is changed *)
  method onChange : (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth

  (* batched changed operation *)
  method onChanges :
    (codemirror Js.t -> change Js.js_array Js.t -> unit) -> unit Js.meth

  (* before a change is applied *)
  method onBeforeChange :
    (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth

  (* cursor moves, or any change *)
  method onCursorActivity : (codemirror Js.t -> unit) -> unit Js.meth

  (* when new input is read *)
  method onKeyHandled :
    (codemirror Js.t -> Js.js_string -> Dom_html.event -> unit) -> unit Js.meth

  (* when new input is read *)
  method onInputRead : (codemirror Js.t -> change Js.t -> unit) -> unit Js.meth

  (* when text matches electric pattern *)
  method onElectricInput : (codemirror Js.t -> int Js.t -> unit) -> unit Js.meth

  (* before a selection is moved TODO *)
  method onBeforeSelectionChange :
    (codemirror Js.t -> 'a -> unit) -> unit Js.meth

  (* the view port changed *)
  method onViewportChange :
    (codemirror Js.t -> int Js.t -> int Js.t -> unit) -> unit Js.meth

  (*  document swapped *)
  method onSwapDoc : (codemirror Js.t -> 'a -> unit) -> unit Js.meth

  (* gutter clicked *)
  method onGutterClick :
    (codemirror Js.t -> int Js.t -> Js.js_string -> Dom_html.event Js.t -> unit) ->
    unit Js.meth

  (* context menu event from gutter *)
  method onGutterContextMenu :
    (codemirror Js.t -> int Js.t -> Js.js_string -> Dom_html.event Js.t -> unit) ->
    unit Js.meth

  (* focus *)
  method onFocus : (codemirror Js.t -> unit) -> unit Js.meth

  (* blur *)
  method onBlur : (codemirror Js.t -> unit) -> unit Js.meth

  (* scroll *)
  method onScroll : (codemirror Js.t -> unit) -> unit Js.meth

  (* cursor scrolled in view*)
  method onScrollCursorIntoView :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  (* dom updated *)
  method onUpdate : (codemirror Js.t -> unit) -> unit Js.meth

  (* line rendered *)
  method onRenderLine :
    (codemirror Js.t -> 'a -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onMousedown :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDblclick :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onTouchstart :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onContextmenu :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onKeydown :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onKeypress :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onKeyup :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onCut :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onCopy :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onPaste :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDragstart :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDragenter :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDragover :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDragleave :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDrop :
    (codemirror Js.t -> Dom_html.event Js.t -> unit) -> unit Js.meth

  method onDelete : (unit -> unit) -> unit Js.meth
  method onBeforeCursorEnter : (unit -> unit) -> unit Js.meth
  method onClear : (position Js.t -> position Js.t -> unit) -> unit Js.meth
  method onHide : (unit -> unit) -> unit Js.meth
  method onUnhide : (unit -> unit) -> unit Js.meth
  method onRedraw : (unit -> unit) -> unit Js.meth
  method setCursor : position Js.t -> unit Js.meth
  method getCursor : position Js.t Js.meth
  method setSelection : position Js.t -> position Js.t -> unit Js.meth
  method performLint : unit Js.meth
end

class type lint_configuration = object
  method delay : int Js.t Js.prop
  method async : bool Js.t Js.prop

  method getAnnotations :
    (Js.js_string ->
    lint_configuration Js.t ->
    codemirror Js.t ->
    lint Js.t Js.js_array Js.t)
    Js.writeonly_prop

  method lintOnChange : bool Js.t Js.prop
end

let constructor_lint_configuration : lint_configuration Js.t Js.constr =
  Js.Unsafe.pure_js_expr "Object"

let create_lint_configuration () : lint_configuration Js.t =
  new%js constructor_lint_configuration

class type configuration = object
  (* The starting value of the editor. *)
  method value : Js.js_string Js.t Js.prop

  (* The mode to use. *)
  method mode : Js.js_string Js.t Js.prop

  (* Explicitly set the line separator for the editor.  *)
  method lineSeparator : Js.js_string Js.opt Js.t Js.prop

  (* The theme to style the editor with. *)
  method theme : Js.js_string Js.t Js.prop

  (* How many spaces a block should be indented.  *)
  method indentUnit : int Js.t Js.prop

  (* Whether to use the context-sensitive indentation *)
  method smartIndent : bool Js.t Js.prop

  (* The width of a tab character. *)
  method tabSize : int Js.t Js.prop

  (* The first N*tabSize in indentation should N tabs. *)
  method indentWithTabs : bool Js.t Js.prop

  (* The editor should re-indent the current line. *)
  method electricChars : bool Js.t Js.prop

  (* A regular expression used to determine special placeholder. *)
  method specialChars : Js.regExp Js.t Js.prop

  (* A function identifies specialChars and produces a DOM node *)
  method specialCharPlaceholder : (int -> Dom_html.element Js.t) Js.prop

  (* Horizontal cursor movement through right-to-left. *)
  method rtlMoveVisually : bool Js.t Js.prop

  (* Configures the key map to use. *)
  method keyMap : string Js.t Js.prop

  (* specify extra key bindings for the editor *)
  method extraKeys : 'a Js.prop

  (* Scroll or wrap for long lines *)
  method lineWrapping : bool Js.t Js.prop

  (*  Show line numbers to the left of the editor *)
  method lineNumbers : bool Js.t Js.prop

  (* At which number to start counting lines. Default is 1. *)
  method firstLineNumber : int Js.t Js.prop

  (* A function used to format line numbers. *)
  method lineNumberFormatter : (int Js.t -> Js.js_string) Js.t Js.prop

  (* Add extra gutters *)
  method gutters : Js.string_array Js.t Js.prop

  (* Gutter scrolls along with the content horizontally *)
  method fixedGutter : bool Js.t Js.prop

  (* Chooses a scrollbar implementation. *)
  method scrollbarStyle : string Js.t Js.prop

  (* cover gutter with with class CodeMirror-gutter-filler. *)
  method coverGutterNextToScrollbar : bool Js.t Js.prop

  (* Selects the way CodeMirror handles input and focus. *)
  method inputStyle : string Js.t Js.prop

  (* disable editing of the editor content *)
  method readOnly : bool Js.t Js.prop

  (* the cursor should be drawn when a selection is active. *)
  method showCursorWhenSelecting : bool Js.t Js.prop

  (* copy or cut when there is no selection will copy or cut whole lines *)
  method lineWiseCopyCut : bool Js.t Js.prop

  (* When pasting something from an external source (not from the
     editor itself), if the number of lines matches the number of
     selection, CodeMirror will by default insert one line per
     selection. You can set this to false to disable that
     behavior. *)
  method pasteLinesPerSelection : bool Js.t Js.prop

  (* Determines whether multiple selections are joined as soon as
     they touch (the default) or only when they overlap (true). *)
  method selectionsMayTouch : bool Js.t Js.prop

  (* maximum number of undo *)
  method undoDepth : int Js.t Js.prop

  (* milliseconds of inactivity to create a new history event *)
  method historyEventDelay : int Js.t Js.prop

  (* tab index of editor *)
  method tabindex : int Js.t Js.prop

  (* CodeMirror focus itself on initialization *)
  method autofocus : bool Js.t Js.prop

  (* enable drag-and-drop *)
  method dragDrop : bool Js.t Js.prop

  (* when set files wit mime type can be dropped into the editor *)
  method allowDropFileTypes : Js.string_array Js.opt Js.t Js.prop

  (* Half-period in milliseconds used for cursor blinking. *)
  method cursorBlinkRate : int Js.t Js.prop

  (* How much extra space to always keep above and below the cursor *)
  method cursorScrollMargin : int Js.t Js.prop

  (* Determines the height of the cursor. *)
  method cursorHeight : int Js.t Js.prop

  (* the context menu is opened with a click outside of the
     current selection, move cursor to the point of the click*)
  method resetSelectionOnContextMenu : bool Js.t Js.prop

  (* time to run highlighting thread *)
  method workTime : int Js.t Js.prop

  (* delay to run highlighting thread *)
  method workDelay : int Js.t Js.prop

  (* how often to poll for changes *)
  method pollInterval : int Js.t Js.prop

  (* combine tokens to a single span *)
  method flattenSpans : bool Js.t Js.prop

  (* prefix css *)
  method addModeClass : bool Js.t Js.prop

  (* length to highlight *)
  method maxHighlightLength : bool Js.t Js.prop

  (* amount of lines that are rendered above and below the
     visible document*)
  method viewportMargin : int Js.t Js.prop

  (* ADDON selection/active-line.js *)
  method styleActiveLine : bool Js.t Js.prop

  (* ADDON lint/lint.js *)
  method lint : lint_configuration Js.t Js.prop

  (* ADDON edit/matchbrackets.js *)
  method matchBrackets : bool Js.t Js.prop
end

let default_configuration : configuration Js.t =
  (Js.Unsafe.js_expr "CodeMirror")##.defaults

let fromTextArea (dom : Dom_html.element Js.t)
    (configuration : configuration Js.t) : codemirror Js.t =
  (* let () = Js.debugger() in *)
  (Js.Unsafe.js_expr "CodeMirror")##fromTextArea
    (Js.Unsafe.inject dom)
    (Js.Unsafe.inject configuration)

class type commands = object
  method save : (codemirror Js.t -> unit) Js.prop
end

let commands : commands Js.t = (Js.Unsafe.js_expr "CodeMirror")##.commands
