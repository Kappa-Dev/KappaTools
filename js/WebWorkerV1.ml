(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class webworker ()  = object
  method yield = Lwt_js.yield
  method log ?exn (_: string) = Lwt.return_unit
  inherit Api_v1.Base.base_runtime 0.1
end

let runtime = (new webworker () :> Api_v1.api_runtime)

let on_message (text_message : string) : unit =
  Lwt.ignore_result
    (Api_mpi_v1.on_message
       runtime
       (fun s -> let () = Worker.post_message s in Lwt.return_unit)
       text_message)
let () = Worker.set_onmessage on_message
