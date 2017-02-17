(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val create_project : Api_types_j.project_id -> unit Api.result Lwt.t
type model = { model_current : Api_types_j.project_id option ;
               model_project_ids : Api_types_j.project_id list ;
               model_contact_map : Api_types_j.contact_map option ;
             }
val model : model React.signal
val set_project : Api_types_j.project_id -> unit Api.result Lwt.t
val remove_project : unit -> unit Api.result Lwt.t
(* run on application init *)
val init : unit -> unit Lwt.t
(* to synch state of application with runtime *)
val sync : unit -> unit Api.result Lwt.t
val with_project : label:string -> (Api.manager -> Api_types_j.project_id -> 'a  Api.result Lwt.t) -> 'a  Api.result Lwt.t
