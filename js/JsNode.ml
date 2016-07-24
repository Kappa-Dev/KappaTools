module ApiTypes = ApiTypes_j

class type process_configuration =
  object
    val command : string Js.t Js.prop
    val args : Js.string_array Js.t Js.prop
    val onStdout : (string Js.t -> unit) Js.t Js.prop
    val onStderr : (string Js.t -> unit) Js.t Js.prop
    val onClose : (unit -> unit) Js.t Js.prop
    val onError : (unit -> unit) Js.t Js.prop
  end

let constructor_process_configuration : process_configuration Js.t Js.constr =
  (Js.Unsafe.variable "Object")

let create_process_configuration
    ?(onStdout : (Js.js_string Js.t -> unit) option = None)
    ?(onStderr : (Js.js_string Js.t -> unit) option = None)
    ?(onClose : (unit -> unit) option = None)
    (command : string)
    (args : string list)
  : process_configuration Js.t  =
  let configuration : process_configuration Js.t =
    new%js constructor_process_configuration in
  let () =
    (Js.Unsafe.coerce configuration)##.command := Js.string command;
    (Js.Unsafe.coerce configuration)##.args :=
      Js.array (Array.of_list (List.map Js.string args));
    (match onStdout with
     | Some onStdout -> (Js.Unsafe.coerce configuration)##.onStdout := onStdout
     | None -> ()
    );
    (match onStderr with
     | Some onStderr -> (Js.Unsafe.coerce configuration)##.onStderr := onStderr
     | None -> ()
    );
    (match onClose with
     | Some onClose -> (Js.Unsafe.coerce configuration)##.onClose := onClose
     | None -> ()
    );
    ()
  in configuration

class type process =
  object
    method write : Js.js_string Js.t -> unit Js.meth
    method kill : unit Js.meth
  end

let spawn_process (configuration : process_configuration  Js.t) : process Js.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "spawnProcess")
    [| Js.Unsafe.inject configuration |]

let kappa_process : process option ref = ref None


class runtime
    ?(timeout : float = 10.)
    (command : string)
    (args : string list) =
  object(self)
    val mutable process : process Js.t option = None
    initializer

      let configuration : process_configuration Js.t  =
        create_process_configuration
          ~onStdout:(Some (fun msg -> self#receive (Js.to_string msg)))
          command
          args
      in
      let () = (Js.Unsafe.coerce configuration)##.onError :=
          (fun () ->
             failwith
               (Format.sprintf
                  "failed to start process : %s"
                  command)
          )
      in
      let p : process Js.t = spawn_process configuration in
      let () = process <- Some p in
      ()
    method sleep timeout = Lwt_js.sleep timeout
    method private post_message (message_text : string) : unit =
      match process with
      | None -> ()
      | Some process -> process##write(Js.string message_text)
    inherit Api_mpi.runtime ~timeout:timeout ()
  end
