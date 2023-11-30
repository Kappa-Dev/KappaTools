(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val error_msg :
  ?severity:Logs.level -> ?range:Locality.range -> string -> Result_util.message

val result_error_msg :
  ?severity:Logs.level ->
  ?range:Locality.range ->
  ?result_code:Result_util.status ->
  string ->
  'ok Api.result

val result_messages :
  ?result_code:Result_util.status -> Result_util.message list -> 'ok Api.result

val result_error_exception :
  ?severity:Logs.level ->
  ?result_code:Result_util.status ->
  exn ->
  'ok Api.result

val method_handler_errors :
  ?severity:Logs.level ->
  Exception_without_parameter.method_handler ->
  Result_util.message list

val method_handler_messages :
  ?severity:Logs.level ->
  ?result_code:Result_util.status ->
  Exception_without_parameter.method_handler ->
  'a Api.result

val result_kasa :
  ('a, Exception_without_parameter.method_handler) Result.result ->
  'a Api.result

val result_bind_lwt :
  ok:('ok -> 'a_ok Api.result Lwt.t) -> 'ok Api.result -> 'a_ok Api.result Lwt.t

val result_fold_lwt :
  f:('ok Api.result -> 'value -> 'ok Api.result Lwt.t) ->
  id:'ok Api.result ->
  'value list ->
  'ok Api.result Lwt.t

val result_combine : unit Api.result list -> unit Api.result
val md5sum : string -> string
