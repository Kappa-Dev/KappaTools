(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val error_msg :
  ?severity:Logs.level -> ?range:Loc.t -> string -> Result_util.message

(* Below are multiple result helpers *)
val message_of_caught_exception :
  ?severity:Logs.level ->
  Exception_without_parameter.caught_exception ->
  Result_util.message

val message_of_uncaught_exception :
  ?severity:Logs.level ->
  Exception_without_parameter.uncaught_exception ->
  Result_util.message

val err_result_of_string :
  ?severity:Logs.level ->
  ?range:Loc.t ->
  ?result_code:Result_util.status ->
  string ->
  'ok Api.result

val err_result_of_msgs :
  ?result_code:Result_util.status -> Result_util.message list -> 'ok Api.result

val err_result_of_msg :
  ?result_code:Result_util.status -> Result_util.message -> 'ok Api.result

val err_result_of_exception :
  ?severity:Logs.level ->
  ?result_code:Result_util.status ->
  exn ->
  'ok Api.result

val err_result_of_exceptions :
  ?severity:Logs.level ->
  ?result_code:Result_util.status ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  'ok Api.result

(* In the functions below, the `with_lwt` suffix specifies that the initial "result" is `Api.result` and not a `Api.lwt_result = Api.result Lwt.t` . The original lwt library only provides interface to only use `Lwt_result.t = result Lwt.t` which the equivalent here use the suffix `_lwt`. *)

val result_bind_with_lwt :
  ok:('ok -> 'a_ok Api.lwt_result) -> 'ok Api.result -> 'a_ok Api.lwt_result

val result_fold_with_lwt :
  f:('ok Api.result -> 'value -> 'ok Api.lwt_result) ->
  id:'ok Api.result ->
  'value list ->
  'ok Api.lwt_result

val result_bind_lwt :
  ok:('ok -> 'a_ok Api.lwt_result) -> 'ok Api.lwt_result -> 'a_ok Api.lwt_result

val result_combine : unit Api.result list -> unit Api.result
val md5sum : string -> string
