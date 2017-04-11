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

let clear_errors () = set_state_error []

let has_errors () =
  match React.S.value state_error with
  | [] -> false
  | _::_ -> true

let set_errors ~append (location : string) (errors : Api_types_j.errors) =
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
      state_error_location =  location; }::
    (if append then current_state_error else [])
  in
  set_state_error new_state_error

let errors : Api_types_j.errors option React.signal =
  React.S.map
    (fun (state_error : t list) ->
       let errors =
         List.fold_left
           (fun acc value -> value.state_error_errors@acc)
           []
           state_error in
       match errors with
       | [] -> None
       | errors -> Some errors)
    state_error

let wrap : 'a . ?append:bool -> string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t =
  fun ?(append = false) loc r ->
    r >>=
    (Api_common.result_map
       ~ok:(fun _ r ->
           let () = if not append then clear_errors () in
           Lwt.return (Api_common.result_ok r))
       ~error:(fun _ (errors : Api_types_j.errors) ->
           let () = set_errors ~append loc errors in
           Lwt.return (Api_common.result_messages errors)
         ))
