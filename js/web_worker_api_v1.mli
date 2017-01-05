(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class runtime : ?timeout:float -> unit ->  object
    method post_message : string -> unit
    method sleep : float -> unit Lwt.t
    method receive : string -> unit
    inherit Api_v1.api_runtime
  end
