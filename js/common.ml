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
let jquery_on
    (selector : string)
    (event : string)
    handler =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "jqueryOn")
    [| Js.Unsafe.inject (Js.string selector);
       Js.Unsafe.inject (Js.string event);
       Js.Unsafe.inject handler |]

let option_string (id : string option) =
  match id with
    Some id -> Js.some (Js.string id)
  | None -> Js.null

let plotPNG (plotDivId : string)
    (title:string)
    (plotName : string)
    (plotStyleId : string option) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "plotPNG")
    [| Js.Unsafe.inject (Js.string plotDivId);
       Js.Unsafe.inject (Js.string title);
       Js.Unsafe.inject (Js.string plotName);
       Js.Unsafe.inject (option_string plotStyleId)
    |]

let plotSVG
    (plotDivId : string)
    (title:string)
    (plotName : string)
    (plotStyleId : string option) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "plotSVG")
    [| Js.Unsafe.inject (Js.string plotDivId);
       Js.Unsafe.inject (Js.string title);
       Js.Unsafe.inject (Js.string plotName);
       Js.Unsafe.inject (option_string plotStyleId)
    |]

let saveFile
    ~(data : string)
    ~(mime : string)
    ~(filename : string) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "saveFile")
    [| Js.Unsafe.inject (Js.string data);
       Js.Unsafe.inject (Js.string mime);
       Js.Unsafe.inject (Js.string filename)
    |]

let input_enter
    ~(id : string)
    ~(handler : (unit -> unit))  =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "inputEnter")
    [| Js.Unsafe.inject (Js.string id) ;
       Js.Unsafe.inject handler ;
    |]

type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]
let method_to_string : meth -> string =
  function
  | `DELETE -> "DELETE"
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"
  | `PATCH -> "PATCH"
  | `POST -> "POST"
  | `PUT -> "PUT"

let ajax_request
    ~(url : string)
    ~(meth : meth)
    ~(data : string option)
    ~(handler : int -> string -> unit) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "ajaxRequest")
    [| Js.Unsafe.inject
         (Js.string url);
       Js.Unsafe.inject
         (Js.string (method_to_string meth));
       Js.Unsafe.inject
         (Js.Opt.option
            (match data with
             | None -> None
             | Some data -> Some (Js.string data)));
       Js.Unsafe.inject
         (Js.wrap_callback
            (fun status response ->
               let () = debug (Js.string "ajax_request") in
               let () = debug status in
               let () = debug response in
               handler
                 status
                 (Js.to_string response)))
    |]

(* This is to handle errors being lost in asyncs
   so there should be no other async calls in the
   code.
*)
let async (task : unit -> 'a Lwt.t) : unit =
  Lwt_js_events.async
    (fun () ->
       Lwt.catch
         task
         (fun exn ->
            let () = debug exn in
            Lwt.return_unit
         )
    )
