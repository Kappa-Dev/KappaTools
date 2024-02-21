(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process = object
  method log : ?exn:exn -> string -> unit Lwt.t
  method yield : unit -> unit Lwt.t
  method min_run_duration : unit -> float
end

type t
(** State of the running simulation.
*)

class null_process : system_process
(** Trivial implementation *)

val parse :
  patternSharing:Pattern.sharing_level ->
  Ast.parsing_compil ->
  (string * Nbr.t) list ->
  system_process ->
  (t, Result_util.message list) Result_util.t Lwt.t

val start :
  system_process:system_process ->
  parameter:Api_types_t.simulation_parameter ->
  t:t ->
  (unit, Result_util.message list) Result_util.t Lwt.t

val pause :
  system_process:system_process ->
  t:t ->
  (unit, Result_util.message list) Result_util.t Lwt.t

val stop :
  system_process:system_process ->
  t:t ->
  (unit, Result_util.message list) Result_util.t Lwt.t

val perturbation :
  system_process:system_process ->
  t:t ->
  perturbation:Api_types_t.simulation_intervention ->
  (string, Result_util.message list) Result_util.t Lwt.t

val continue :
  system_process:system_process ->
  t:t ->
  pause_condition:string ->
  (unit, Result_util.message list) Result_util.t Lwt.t

val progress :
  system_process:system_process ->
  t:t ->
  (Api_types_t.simulation_progress, Result_util.message list) Result_util.t
  Lwt.t

val outputs :
  system_process:system_process ->
  t:t ->
  (Api_data.simulation_detail_output, Result_util.message list) Result_util.t
  Lwt.t

val efficiency : t -> Counter.Efficiency.t
val get_raw_trace : t -> string
val get_raw_ast : t -> string
