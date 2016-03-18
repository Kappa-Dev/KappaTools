let id value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "id")
    [| Js.Unsafe.inject value |]

let debug value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "debug")
    [| Js.Unsafe.inject value |]

let info value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "info")
    [| Js.Unsafe.inject value |]

let notice value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "notice")
    [| Js.Unsafe.inject value |]

let warning value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "warning")
    [| Js.Unsafe.inject value |]

let error value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "error")
    [| Js.Unsafe.inject value |]

let fatal value =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "fatal")
    [| Js.Unsafe.inject value |]
let jquery_on (selector : string)
              (event : string) handler =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "jqueryOn")
    [| Js.Unsafe.inject (Js.string selector);
       Js.Unsafe.inject (Js.string event);
       Js.Unsafe.inject handler |]
