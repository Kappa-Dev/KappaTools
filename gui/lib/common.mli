(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val toss : 'a -> 'b
val id : 'a -> 'b
val debug : loc:string -> 'a -> unit
val info : loc:string -> 'a -> unit
val log : loc:string -> 'a -> unit
val warn : loc:string -> 'a -> unit
val error : loc:string -> 'a -> unit
val log_group : string -> unit
val log_group_end : unit -> unit
val jquery_on : string -> string -> 'a -> 'b
val option_string : string option -> Js.js_string Js.t Js.opt
val plotPNG : ?plotStyleId:string -> string -> string -> string -> unit
val plotSVG : ?plotStyleId:string -> string -> string -> string -> unit
val saveFile : data:'a Js.t -> mime:string -> filename:string -> unit

type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

val method_to_string : meth -> string

val ajax_request :
  ?timeout:float ->
  url:string ->
  meth:meth ->
  ?data:string ->
  handler:(int -> string -> unit) ->
  'a

val async : string -> (unit -> unit Lwt.t) -> unit
val guid : unit -> string
val modal : id:string -> action:string -> unit
val element_data : Dom_html.element Js.t -> string -> Js.js_string Js.t Js.opt
val create_sort : string -> (Dom_html.event Js.t -> 'b -> unit) -> unit

val children_value :
  Dom_html.element Js.t -> string -> (Dom_html.element Js.t -> 'a) -> 'a list

val hide_codemirror : unit -> unit
val show_codemirror : unit -> unit

(* simple print of int option *)
val string_of_option : ('a -> string) -> 'a option -> string
