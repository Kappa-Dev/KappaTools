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

let result_error_msg ?severity ?range ?result_code (message : string) :
    'ok Api.result =
  Result_util.error ?status:result_code [ error_msg ?severity ?range message ]

let result_messages ?result_code messages : 'ok Api.result =
  Result_util.error ?status:result_code messages

let result_error_exception ?severity ?result_code (e : exn) : 'ok Api.result =
  let message =
    try Printexc.to_string e with _ -> "unspecified exception thrown"
  in
  result_error_msg ?severity ?result_code message

let method_handler_errors ?severity mh =
  let uncaught =
    Exception_without_parameter.get_uncaught_exception_list_to_ui mh
  in
  let caught = Exception_without_parameter.get_caught_exception_list_to_ui mh in
  List.fold_right
    (fun x l ->
      error_msg ?severity
        (Format.asprintf "%a" Exception_without_parameter.pp_caught x)
      :: l)
    caught
    (List.map
       (fun x ->
         error_msg ?severity
           (Format.asprintf "%a" Exception_without_parameter.pp_uncaught x))
       uncaught)

let method_handler_messages ?severity ?result_code mh =
  result_messages ?result_code (method_handler_errors ?severity mh)

let result_kasa = function
  | Result.Ok x -> Result_util.ok x
  | Result.Error mh -> method_handler_messages ~severity:Logs.Error mh

let result_bind_lwt :
    ok:('ok -> ('a_ok, 'a_code) Api_types_t.result Lwt.t) ->
    ('ok, 'a_code) Api_types_t.result ->
    ('a_ok, 'a_code) Api_types_t.result Lwt.t =
 fun ~(ok : 'ok -> ('a_ok, 'a_code) Api_types_t.result Lwt.t)
     { Result_util.value; status; messages } ->
  match value with
  | Result.Ok data -> ok data
  | Result.Error e ->
    Lwt.return { Result_util.value = Result.Error e; status; messages }

let rec result_fold_lwt :
    f:
      (('ok, 'a_code) Api_types_t.result ->
      'value ->
      ('ok, 'a_code) Api_types_t.result Lwt.t) ->
    id:('ok, 'a_code) Api_types_t.result ->
    'value list ->
    ('a_ok, 'a_code) Api_types_t.result Lwt.t =
 fun ~(f :
        ('ok, 'a_code) Api_types_t.result ->
        'value ->
        ('ok, 'a_code) Api_types_t.result Lwt.t)
     ~(id : ('ok, 'a_code) Api_types_t.result) (l : 'value list) ->
  match l with
  | [] -> Lwt.return id
  | h :: t -> f id h >>= fun result -> result_fold_lwt ~f ~id:result t

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
