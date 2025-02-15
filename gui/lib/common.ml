(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* need to specify messages where there is an error *)
let toss : 'a 'b. 'a -> 'b =
 fun e ->
  let () =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "toss") [| Js.Unsafe.inject e |]
  in
  assert false

let id value =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "id") [| Js.Unsafe.inject value |]

let debug ~loc value =
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "debug")
      [| Js.Unsafe.inject value; Js.Unsafe.inject (Js.string ("\n  " ^ loc)) |]
  in
  ()

let log ~loc value =
  let () =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "log")
      [| Js.Unsafe.inject value; Js.Unsafe.inject (Js.string ("\n  " ^ loc)) |]
  in
  ()

let info ~loc value =
  let () =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "info")
      [| Js.Unsafe.inject value; Js.Unsafe.inject (Js.string ("\n  " ^ loc)) |]
  in
  ()

let warn ~loc value =
  let () =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "warn")
      [| Js.Unsafe.inject value; Js.Unsafe.inject (Js.string ("\n  " ^ loc)) |]
  in
  ()

let error ~loc value =
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "error")
      [| Js.Unsafe.inject value; Js.Unsafe.inject (Js.string ("\n  " ^ loc)) |]
  in
  ()

let log_group label =
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "log_group")
      [| Js.Unsafe.inject label |]
  in
  ()

let log_group_end () : unit =
  let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "log_group_end") [||] in
  ()

let jquery_on (selector : string) (event : string) handler =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "jqueryOn")
    [|
      Js.Unsafe.inject (Js.string selector);
      Js.Unsafe.inject (Js.string event);
      Js.Unsafe.inject handler;
    |]

let option_string (id : string option) =
  match id with
  | Some id -> Js.some (Js.string id)
  | None -> Js.null

let plotPNG ?(plotStyleId : string option) (plotDivId : string) (title : string)
    (plotName : string) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "plotPNG")
    [|
      Js.Unsafe.inject (Js.string plotDivId);
      Js.Unsafe.inject (Js.string title);
      Js.Unsafe.inject (Js.string plotName);
      Js.Unsafe.inject (option_string plotStyleId);
    |]

let plotSVG ?(plotStyleId : string option) (plotDivId : string) (title : string)
    (plotName : string) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "plotSVG")
    [|
      Js.Unsafe.inject (Js.string plotDivId);
      Js.Unsafe.inject (Js.string title);
      Js.Unsafe.inject (Js.string plotName);
      Js.Unsafe.inject (option_string plotStyleId);
    |]

let saveFile ~(data : 'a Js.t) ~(mime : string) ~(filename : string) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "saveFile")
    [|
      Js.Unsafe.inject data;
      Js.Unsafe.inject (Js.string mime);
      Js.Unsafe.inject (Js.string filename);
    |]

type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

let method_to_string : meth -> string = function
  | `DELETE -> "DELETE"
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"
  | `PATCH -> "PATCH"
  | `POST -> "POST"
  | `PUT -> "PUT"

let ajax_request ?(timeout : float option) ~(url : string) ~(meth : meth)
    ?(data : string option) ~(handler : int -> string -> unit) =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "ajaxRequest")
    [|
      Js.Unsafe.inject (Js.string url);
      Js.Unsafe.inject (Js.string (method_to_string meth));
      Js.Unsafe.inject
        (Js.Opt.option
           (match data with
           | None -> None
           | Some data -> Some (Js.string data)));
      Js.Unsafe.inject
        (Js.wrap_callback (fun status response ->
             let () =
               debug ~loc:__LOC__
                 (Js.string
                    ("request " ^ url ^ " answer: " ^ string_of_int status))
             in
             let () = debug ~loc:__LOC__ response in
             handler status (Js.to_string response)));
      Js.Unsafe.inject
        (Js.Opt.option
           (match timeout with
           | None -> None
           | Some timeout -> Some timeout));
    |]

(* This is to handle errors being lost in asyncs
   so there should be no other async calls in the
   code.
*)
let async loc (task : unit -> 'a Lwt.t) : unit =
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      Lwt.catch task (fun exn ->
          let () =
            warn ~loc:__LOC__
              (Js.string
                 ("Async error at " ^ loc ^ ":\n" ^ Printexc.to_string exn))
          in
          let log_trace =
            if Printexc.backtrace_status () then (
              let trace = Printexc.get_backtrace () in
              if String.length trace > 0 then
                "Backtrace: " ^ trace
              else
                "Couldn't get backtrace while backtrace record is enabled, \
                 might be because of jsoo or react :/"
            ) else
              "Enable backtrace recording to (hopefully?) get backtrace"
          in
          let () = warn ~loc:__LOC__ (Js.string log_trace) in
          Lwt.return_unit))

let guid () : string =
  Js.to_string (Js.Unsafe.fun_call (Js.Unsafe.js_expr "guid") [||])

let modal ~(id : string) ~(action : string) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "modal")
    [| Js.Unsafe.inject (Js.string id); Js.Unsafe.inject (Js.string action) |]

let element_data (element : Dom_html.element Js.t) (label : string) :
    Js.js_string Js.t Js.opt =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "elementData")
    [| Js.Unsafe.inject element; Js.Unsafe.inject (Js.string label) |]

let create_sort (id : string) (handler : Dom_html.event Js.t -> 'b -> unit) :
    unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "createSort")
    [| Js.Unsafe.inject (Js.string id); Js.Unsafe.inject handler |]

let children_value (element : Dom_html.element Js.t) (selector : string)
    (map : Dom_html.element Js.t -> 'a) : 'a list =
  Array.to_list
    (Js.to_array
       (Js.Unsafe.fun_call
          (Js.Unsafe.js_expr "childrenValue")
          [|
            Js.Unsafe.inject element;
            Js.Unsafe.inject (Js.string selector);
            Js.Unsafe.inject map;
          |]))

let hide_codemirror () : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "hideCodeMirror") [||]

let show_codemirror () : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "showCodeMirror") [||]

let string_of_option string_of_value i_opt =
  match i_opt with
  | None -> "None"
  | Some i -> "Some " ^ string_of_value i
