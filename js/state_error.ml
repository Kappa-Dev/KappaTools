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
let state_error, set_state_error = React.S.create (None : t option)

let clear_errors () = set_state_error None

let has_errors () =
  match React.S.value state_error with
  | None -> false
  | Some _ -> true

let set_errors (location : string) (errors : Api_types_j.errors) =
  (* log location and errors if debugging is enabled *)
  let debug =
    Format.sprintf
      "{ location : \"%s\" , errors : %s }"
      location
      (Api_types_j.string_of_errors errors)
  in
  let () = Common.debug (Js.string (Format.sprintf "set_errors %s "debug)) in
  set_state_error (Some ({ state_error_errors = errors;
                           state_error_location =  location; }))

let errors : Api_types_j.errors option React.signal =
  React.S.map (function
               | None -> None
               | Some t -> Some t.state_error_errors)
  state_error

let wrap : 'a . string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t =
fun loc r ->
  let () = clear_errors () in
  r >>=
  (Api_common.result_map
     ~ok:(fun _ r ->
         Lwt.return (Api_common.result_ok r))
     ~error:(fun _ (errors : Api_types_j.errors) ->
         let () = set_errors loc errors in
         Lwt.return (Api_common.result_messages errors)
       ))
