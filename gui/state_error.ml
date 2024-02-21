(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type t = {
  state_error_errors: Result_util.message list;
  _state_error_location: string;
}

let state_error, set_state_error = React.S.create ([] : t list)

let clear_errors location =
  let () =
    Common.debug (Js.string (Format.sprintf "Clear_errors %s " location))
  in
  set_state_error []

let has_errors () =
  match React.S.value state_error with
  | [] -> false
  | _ :: _ -> true

let add_error (location : string) (errors : Result_util.message list) =
  (* log location and errors if debugging is enabled *)
  let () =
    Common.debug
      (Js.string
         (Format.asprintf "set_errors { location : \"%s\" , errors : [@[%a@]] }"
            location
            (Pp.list Pp.space Result_util.print_message)
            errors))
  in
  let current_state_error : t list = React.S.value state_error in
  let new_state_error : t list =
    { state_error_errors = errors; _state_error_location = location }
    :: current_state_error
  in
  set_state_error new_state_error

let errors : Result_util.message list React.signal =
  React.S.map
    (fun (state_error : t list) ->
      List.fold_left
        (fun acc value -> value.state_error_errors @ acc)
        [] state_error)
    state_error

let wrap :
      'a. ?append:bool -> string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t =
 fun ?(append = false) loc r ->
  r
  >>=
  let () = if not append then clear_errors loc in
  Result_util.fold
    ~ok:(fun r -> Lwt.return (Result_util.ok r))
    ~error:(fun errors ->
      let () = add_error loc errors in
      Lwt.return (Api_common.result_messages errors))
