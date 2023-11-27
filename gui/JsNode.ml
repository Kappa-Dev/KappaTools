(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type process_configuration = object
  method command : Js.js_string Js.t Js.prop
  method args : Js.js_string Js.t Js.js_array Js.t Js.prop
  method onStdout : (Js.js_string Js.t -> unit) Js.prop
  method onStderr : (Js.js_string Js.t -> unit) Js.prop
  method onClose : (unit -> unit) Js.prop
  method onError : (unit -> unit) Js.prop
end

let constructor_process_configuration : process_configuration Js.t Js.constr =
  Js.Unsafe.pure_js_expr "Object"

let create_process_configuration
    ?(onStdout : (Js.js_string Js.t -> unit) option)
    ?(onStderr : (Js.js_string Js.t -> unit) option)
    ?(onClose : (unit -> unit) option) ?(onError : (unit -> unit) option)
    (command : string) (args : string list) : process_configuration Js.t =
  let configuration : process_configuration Js.t =
    new%js constructor_process_configuration
  in
  let () =
    configuration##.command := Js.string command;
    configuration##.args := Js.array (Array.of_list (List.map Js.string args));
    (match onStdout with
    | Some onStdout -> configuration##.onStdout := onStdout
    | None -> ());
    (match onStderr with
    | Some onStderr -> configuration##.onStderr := onStderr
    | None -> ());
    (match onClose with
    | Some onClose -> configuration##.onClose := onClose
    | None -> ());
    (match onError with
    | Some onError -> configuration##.onError := onError
    | None -> ());
    ()
  in
  configuration

class type process = object
  method write : Js.js_string Js.t -> unit Js.meth
  method kill : unit Js.meth
end

let spawn_process (configuration : process_configuration Js.t) :
    process Js.t Js.opt =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "spawnProcess")
    [| Js.Unsafe.inject configuration |]

let launch_agent onClose message_delimiter command args handler =
  let buffer = Buffer.create 512 in
  let rec onStdout msg =
    match Tools.string_split_on_char message_delimiter (Js.to_string msg) with
    | prefix, None -> Buffer.add_string buffer prefix
    | prefix, Some suffix ->
      let () = Buffer.add_string buffer prefix in
      let () = handler (Buffer.contents buffer) in
      let () = Buffer.reset buffer in
      onStdout (Js.string suffix)
  in
  let configuration : process_configuration Js.t =
    create_process_configuration ~onStdout ~onClose command args
  in
  Js.Opt.case
    (spawn_process configuration)
    (fun () ->
      let () = onClose () in
      failwith ("Launching '" ^ command ^ "' failed"))
    (fun x -> x)

class manager ?(message_delimiter : char = Tools.default_message_delimter)
  (command : string) (args : string list) :
  Api.concrete_manager =
  let switch_re = Re.compile (Re.str "KappaSwitchman") in
  let stor_command = Re.replace_string switch_re ~by:"KaStor" command in
  let switch_mailbox = Switchman_client.new_mailbox () in
  let stor_state, update_stor_state = Kastor_client.init_state () in
  let running_ref = ref true in
  let onClose () = running_ref := false in
  let stor_process =
    launch_agent onClose message_delimiter stor_command args
      (Kastor_client.receive update_stor_state)
  in
  let switch_process =
    launch_agent onClose message_delimiter command args
      (Switchman_client.receive switch_mailbox)
  in
  object (self)
    method private sleep timeout = Js_of_ocaml_lwt.Lwt_js.sleep timeout
    method is_running = !running_ref

    method terminate =
      let () = switch_process##kill in
      let () = stor_process##kill in
      ()

    method is_computing =
      Switchman_client.is_computing switch_mailbox || self#story_is_computing

    inherit
      Kastor_client.new_client
        ~post:(fun message_text ->
          stor_process##write
            (Js.string (Format.sprintf "%s%c" message_text message_delimiter)))
        stor_state

    inherit
      Switchman_client.new_client
        ~is_running:(fun () -> true)
        ~post:(fun message_text ->
          switch_process##write
            (Js.string (Format.sprintf "%s%c" message_text message_delimiter)))
        switch_mailbox
  end
