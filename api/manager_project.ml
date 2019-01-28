(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt

let stop_simulation (system_process:Kappa_facade.system_process) :
  Api_environment.simulation option -> unit Api.result Lwt.t =
  function
  | None -> Lwt.return (Result_util.ok ())
  | Some current ->
    let t : Kappa_facade.t = current#get_runtime_state () in
    Kappa_facade.stop ~system_process:system_process ~t:t

class manager_project
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) =
object
  method project_parse overwrites :
    Api_types_j.project_parse Api.result Lwt.t =
    (project#get_state () >>= function
      | Some x -> Lwt.return x
      | None ->
        let overwrites =
          List.rev_map
            (fun x -> (x.Api_types_t.overwrite_var,x.Api_types_t.overwrite_val))
            overwrites in
        let harakiri,_ = Lwt.task () in
        let cand =
          Lwt.pick [
            Kappa_facade.parse
              ~system_process
              ~kappa_files:(project#get_files ())
              ~overwrites;
            harakiri >>= fun () ->
            Lwt.return (Result_util.error
                          [Api_common.error_msg
                             "Parse cancelled by modified files"])
          ] in
             let _ = project#set_state cand in
        cand)
    >>=
    (fun state ->
       Lwt.return
         (Result_util.fold
            ~ok:(fun kappa_facade ->
                Result_util.ok {
                  Api_types_t.project_parse_project_version =
                    project#get_version ();
                  Api_types_t.project_parse_raw_ast =
                    Kappa_facade.get_raw_ast kappa_facade;
                })
            ~error:(fun error -> Api_common.result_messages error)
            state
         )
    )
end
