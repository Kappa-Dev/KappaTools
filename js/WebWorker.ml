open Lwt

class webworker ()  = object
  method yield = Lwt_js.yield
  method log ?exn (_: string) = Lwt.return_unit
  inherit Api_v1.Base.runtime 0.1
end

let runtime = (new webworker () :> Api_v1.runtime)

let on_message (text_message : string) : unit =
  Api_mpi.on_message
    runtime
    Worker.post_message
    text_message
let () = Worker.set_onmessage on_message
