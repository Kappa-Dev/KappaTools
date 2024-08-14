(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(* Helper functions for result *)
let error_msg ?(severity = Logs.Error) ?range text : Result_util.message =
  { Result_util.severity; Result_util.text; Result_util.range }

let err_result_of_string ?severity ?range ?result_code (message : string) :
    'ok Api.result =
  Result_util.error ?status:result_code [ error_msg ?severity ?range message ]

let err_result_of_msgs ?result_code messages : 'ok Api.result =
  Result_util.error ?status:result_code messages

let err_result_of_msg ?result_code message : 'ok Api.result =
  err_result_of_msgs ?result_code [ message ]

let err_result_of_exception ?severity ?result_code (e : exn) : 'ok Api.result =
  let message =
    try Printexc.to_string e with _ -> "unspecified exception thrown"
  in
  err_result_of_string ?severity ?result_code message

let message_of_caught_exception ?severity caught_exception =
  error_msg ?severity
    (Format.asprintf "%a" Exception_without_parameter.pp_caught caught_exception)

let message_of_uncaught_exception ?severity uncaught_exception =
  error_msg ?severity
    (Format.asprintf "%a" Exception_without_parameter.pp_uncaught
       uncaught_exception)

let messages_of_exceptions ?(severity : Logs.level option)
    (excs : Exception_without_parameter.exceptions_caught_and_uncaught) :
    Result_util.message list =
  let uncaught =
    Exception_without_parameter.get_uncaught_exception_list_to_ui excs
  in
  let caught =
    Exception_without_parameter.get_caught_exception_list_to_ui excs
  in
  List.fold_right
    (fun x l -> message_of_caught_exception ?severity x :: l)
    caught
    (List.map (message_of_uncaught_exception ?severity) uncaught)

let err_result_of_exceptions ?(severity : Logs.level option)
    ?(result_code : Result_util.status option)
    (excs : Exception_without_parameter.exceptions_caught_and_uncaught) :
    'a Api.result =
  err_result_of_msgs ?result_code (messages_of_exceptions ?severity excs)

let result_bind_with_lwt :
    ok:('ok -> 'a_ok Api.lwt_result) -> 'ok Api.result -> 'a_ok Api.lwt_result =
 fun ~(ok : 'ok -> 'a_ok Api.lwt_result) { Result_util.value; status } ->
  match value with
  | Result.Ok data -> ok data
  | Result.Error e -> Lwt.return { Result_util.value = Result.Error e; status }

let rec result_fold_with_lwt :
    f:('ok Api.result -> 'value -> 'ok Api.lwt_result) ->
    id:'ok Api.result ->
    'value list ->
    'a_ok Api.lwt_result =
 fun ~(f : 'ok Api.result -> 'value -> 'ok Api.lwt_result)
     ~(id : 'ok Api.result) (l : 'value list) ->
  match l with
  | [] -> Lwt.return id
  | h :: t -> f id h >>= fun result -> result_fold_with_lwt ~f ~id:result t

let result_bind_lwt :
    ok:('ok -> 'a_ok Api.lwt_result) ->
    'ok Api.lwt_result ->
    'a_ok Api.lwt_result =
 fun ~(ok : 'ok -> 'a_ok Api.lwt_result) lwt_result ->
  lwt_result >>= result_bind_with_lwt ~ok

let rec result_combine : unit Api.result list -> unit Api.result = function
  | [] -> Result_util.ok ()
  | l :: t ->
    Result_util.fold
      ~ok:(fun () -> result_combine t)
      ~error:(fun data_1 ->
        Result_util.fold
          ~ok:(fun () -> l)
          ~error:(fun data_r ->
            Result_util.error ~status:l.Result_util.status (data_1 @ data_r))
          (result_combine t))
      l

let md5sum text = Digest.to_hex (Digest.string text)
