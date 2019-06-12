(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

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
    ?(onStdout : (Js.js_string Js.t -> unit) option)
    ?(onStderr : (Js.js_string Js.t -> unit) option)
    ?(onClose : (unit -> unit) option)
    ?(onError : (unit -> unit) option)
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

let launch_agent onClose message_delimiter command args handler =
  let buffer = Buffer.create 512 in
  let rec onStdout msg =
    match Tools.string_split_on_char message_delimiter (Js.to_string msg) with
    | (prefix,None) ->
      Buffer.add_string buffer prefix
    | (prefix,Some suffix) ->
      let () = Buffer.add_string buffer prefix in
      let () = handler (Buffer.contents buffer) in
      let () = Buffer.reset buffer in
      onStdout (Js.string suffix) in
  let configuration : process_configuration Js.t  =
    create_process_configuration ~onStdout ~onClose command args in
  Js.Opt.case
    (spawn_process configuration)
    (fun () ->
       let () = onClose () in
       failwith ("Launching '"^command^"' failed"))
    (fun x -> x)

class manager
    ?(message_delimiter : char = Tools.default_message_delimter)
    (command : string)
    (args : string list) : Api.concrete_manager =
  let sim_re = Re.compile (Re.str "KaSimAgent") in
  let sa_re = Re.compile (Re.str "KaSaAgent") in
  let sim_command,sa_command,stor_command,moha_command =
    if Re.execp sim_re command then
      command, Re.replace_string  sim_re ~by:"KaSaAgent" command,
      Re.replace_string  sim_re ~by:"KaStor" command,
      Re.replace_string  sim_re ~by:"KaMoHa" command
    else if Re.execp sa_re command then
      Re.replace_string sa_re ~by:"KaSimAgent" command, command,
      Re.replace_string  sa_re ~by:"KaStor" command,
      Re.replace_string  sa_re ~by:"KaMoHa" command
    else
      failwith ("Unrecognized command: "^command) in
  let sa_mailbox = Kasa_client.new_mailbox () in
  let moha_mailbox = Kamoha_client.new_mailbox () in
  let stor_state,update_stor_state = Kastor_client.init_state () in
  let running_ref = ref true in
  let onClose () = running_ref := false in
  let stor_process =
    launch_agent
      onClose message_delimiter
      stor_command args (Kastor_client.receive update_stor_state) in
  let sa_process =
    launch_agent
      onClose message_delimiter
      sa_command args (Kasa_client.receive sa_mailbox) in
  let moha_process =
    launch_agent
      onClose message_delimiter
      moha_command args (Kamoha_client.receive moha_mailbox) in
  object(self)
    val mutable sim_process : process Js.t option = None
    val buffer = Buffer.create 1024
    initializer
      let sim_configuration : process_configuration Js.t  =
        create_process_configuration
          ~onStdout:(fun msg -> self#onSimStdout (Js.to_string msg))
          ~onClose sim_command args in
      let p : process Js.t Js.opt = spawn_process sim_configuration in
      let () = sim_process <- Js.Opt.to_option p in
      ()

    method private onSimStdout (msg : string) : unit =
      let () = Common.debug (Js.string msg) in
      match Tools.string_split_on_char message_delimiter msg with
      | (prefix,None) ->
        ignore(Common.debug (Js.string "onStdout:none"));
        Buffer.add_string buffer prefix
      | (prefix,Some suffix) ->
        ignore(Common.debug (Js.string "onStdout:some"));
        Buffer.add_string buffer prefix;
        self#receive (Buffer.contents buffer);
        Buffer.reset buffer;
        self#onSimStdout suffix

    method private sleep timeout = Js_of_ocaml_lwt.Lwt_js.sleep timeout
    method private post_message (message_text : string) : unit =
      match sim_process with
      | None -> ()
      | Some process ->
        process##write
          (Js.string
             (Format.sprintf
                "%s%c"
                message_text
                message_delimiter))

    method is_running : bool =
      match sim_process with
      | Some _ -> !running_ref
      | None -> false

    method terminate =
      let () = sa_process##kill in
      let () = stor_process##kill in
      match sim_process with
      | Some process -> process##kill
      | None -> ()

    inherit Mpi_api.manager ()
    inherit Kasa_client.new_client
        ~post:(fun message_text ->
            sa_process##write
              (Js.string
                 (Format.sprintf
                    "%s%c"
                    message_text
                    message_delimiter)))
        sa_mailbox

    method is_computing =
      self#sim_is_computing || Kasa_client.is_computing sa_mailbox ||
      self#story_is_computing || Kamoha_client.is_computing moha_mailbox

    inherit Kastor_client.new_client
        ~post:(fun message_text ->
            stor_process##write
              (Js.string
                 (Format.sprintf "%s%c" message_text message_delimiter)))
        stor_state

    inherit Kamoha_client.new_client
        ~post:(fun message_text ->
            moha_process##write
              (Js.string
                 (Format.sprintf "%s%c" message_text message_delimiter)))
        moha_mailbox

    method project_parse overwrites =
      self#secret_project_parse >>=
      Api_common.result_bind_lwt
        ~ok:(fun out ->
            self#secret_simulation_load out overwrites >>=
            Api_common.result_bind_lwt
              ~ok:(fun () -> self#init_static_analyser out >|= Api_common.result_kasa))
  end
