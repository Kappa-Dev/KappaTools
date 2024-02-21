(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val lwt_reporter : Lwt_io.output_channel option -> Logs.reporter
val serve : Lwt_io.input_channel -> char -> (string -> unit Lwt.t) -> unit Lwt.t
