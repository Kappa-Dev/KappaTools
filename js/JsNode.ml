(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type process_configuration =
  object
    method command : Js.js_string Js.t Js.prop
    method args : Js.js_string Js.t Js.js_array Js.t Js.prop
    method onStdout : (Js.js_string Js.t -> unit) Js.prop
    method onStderr : (Js.js_string Js.t -> unit) Js.prop
    method onClose : (unit -> unit) Js.prop
    method onError : (unit -> unit) Js.prop
  end

let constructor_process_configuration : process_configuration Js.t Js.constr =
  (Js.Unsafe.variable "Object")

let create_process_configuration
    ?(onStdout : (Js.js_string Js.t -> unit) option = None)
    ?(onStderr : (Js.js_string Js.t -> unit) option = None)
    ?(onClose : (unit -> unit) option = None)
    ?(onError : (unit -> unit) option = None)
    (command : string)
    (args : string list)
  : process_configuration Js.t  =
  let configuration : process_configuration Js.t =
    new%js constructor_process_configuration in
  let () =
    configuration##.command := Js.string command;
    configuration##.args :=
      Js.array (Array.of_list (List.map Js.string args));
    (match onStdout with
     | Some onStdout -> configuration##.onStdout := onStdout
     | None -> ()
    );
    (match onStderr with
     | Some onStderr -> configuration##.onStderr := onStderr
     | None -> ()
    );
    (match onClose with
     | Some onClose -> configuration##.onClose := onClose
     | None -> ()
    );
    (match onError with
     | Some onError -> configuration##.onError := onError
     | None -> ()
    );
    ()
  in configuration

class type process =
  object
    method write : Js.js_string Js.t -> unit Js.meth
    method kill : unit Js.meth
  end

let spawn_process (configuration : process_configuration  Js.t) : process Js.t Js.opt =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "spawnProcess")
    [| Js.Unsafe.inject configuration |]

let kappa_process : process option ref = ref None


class manager
    ?(message_delimiter : char = Mpi_api.default_message_delimter)
    (command : string)
    (args : string list) =
  object(self)
    val mutable process : process Js.t option = None
    val mutable process_is_running : bool = false
    val buffer = Buffer.create 1024
    initializer
      let configuration : process_configuration Js.t  =
        create_process_configuration
          ~onStdout:(Some (fun msg -> self#onStdout (Js.to_string msg)))
          ~onClose:(Some (fun () -> process_is_running <- true))
          ~onError:(Some (fun () -> process_is_running <- true))
          command
          args
      in
      let p : process Js.t Js.opt = spawn_process configuration in
      let () = process <- Js.Opt.to_option p in
      ()

    method private onStdout (msg : string) : unit =
      let () = Common.debug (Js.string msg) in
      match Utility.split msg message_delimiter with
      | (prefix,None) ->
        ignore(Common.debug (Js.string "onStdout:none"));
        Buffer.add_string buffer prefix
      | (prefix,Some suffix) ->
        ignore(Common.debug (Js.string "onStdout:some"));
        Buffer.add_string buffer prefix;
        self#receive (Buffer.contents buffer);
        Buffer.reset buffer;
        self#onStdout suffix

    method sleep timeout = Lwt_js.sleep timeout
    method private post_message (message_text : string) : unit =
      match process with
      | None -> ()
      | Some process ->
        process##write
          (Js.string
             (Format.sprintf
                "%s%c"
                message_text
                message_delimiter))

    method is_running () : bool =
      match process with
      | Some _ -> true
      | None -> false

    method terminate () : unit =
      match process with
      | Some process -> process##kill
      | None -> ()

    inherit Mpi_api.manager ()
  end
