(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type t =
  { state_error_errors : Api_types_j.errors ;
    state_error_location : string }
let state_error, set_state_error = React.S.create ([] : t list)

let clear_errors location =
  let () = Common.debug
      (Js.string (Format.sprintf "Clear_errors %s " location)) in
  set_state_error []

let has_errors () =
  match React.S.value state_error with
  | [] -> false
  | _::_ -> true

let add_error (location : string) (errors : Api_types_j.errors) =
  (* log location and errors if debugging is enabled *)
  let debug =
    Format.sprintf
      "{ location : \"%s\" , errors : %s }"
      location
      (Api_types_j.string_of_errors errors)
  in
  let () = Common.debug (Js.string (Format.sprintf "set_errors %s "debug)) in
  let current_state_error : t list = React.S.value state_error in
  let new_state_error : t list =
    { state_error_errors = errors;
      state_error_location =  location; }::current_state_error in
  set_state_error new_state_error

let errors : Api_types_j.errors React.signal =
  React.S.map
    (fun (state_error : t list) ->
       List.fold_left
         (fun acc value -> value.state_error_errors@acc)
         []
         state_error)
    state_error

let wrap : 'a . ?append:bool -> string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t =
  fun ?(append=false) loc r ->
    r >>=
    (let () = if not append then clear_errors loc in
     Api_common.result_map
       ~ok:(fun _ r ->
           Lwt.return (Api_common.result_ok r))
       ~error:(fun _ (errors : Api_types_j.errors) ->
           let () = add_error loc errors in
           Lwt.return (Api_common.result_messages errors)
         ))
