(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(** Manages worker message reception.

    Opens modal to avert the user by default, can be disabled to keep original exception in developer tools by providing to the app the `?no_exc_modal=true` argument *)
let worker_onmessage ~(debug_printing : bool) ~(worker_name : string)
    ~(exception_creates_modal : bool) ~(worker_receive_f : string -> unit) :
    ( (string, string) Worker.worker Js.t,
      string Worker.messageEvent Js.t )
    Dom.event_listener =
  let process_message
      (response_message :
        < data : < get : string ; .. > Js.gen_prop ; .. > Js.t) : bool Js.t =
    let response_text = response_message##.data in
    if debug_printing then (
      let () = Common.log_group ("Message received from " ^ worker_name) in
      let () = Common.debug ~loc:__LOC__ response_text in
      Common.log_group_end ()
    );
    let () = worker_receive_f response_text in
    Js._true
  in
  let handler_f : string Worker.messageEvent Js.t -> bool Js.t =
    if exception_creates_modal then (
      fun response_message ->
    try process_message response_message
    with _ as e ->
      Ui_common.open_modal_error ~is_critical:true
        ~error_content:
          ("Worker " ^ worker_name ^ " raised the following exception: \r"
         ^ Printexc.to_string e);
      raise e
    ) else
      process_message
  in
  Dom.handler handler_f

class manager () =
  let kasa_worker = Worker.create "KaSaWorker.js" in
  let kasa_mailbox = Kasa_client.new_mailbox () in
  let kamoha_worker = Worker.create "KaMoHaWorker.js" in
  let kamoha_mailbox = Kamoha_client.new_mailbox () in
  let kastor_worker = Worker.create "KaStorWorker.js" in
  let stor_state, update_stor_state = Kastor_client.init_state () in

  object (self)
    val sim_worker = Worker.create "KaSimWorker.js"
    val mutable is_running = true

    initializer
      (* The url argument `no_exc_modal=true` disables exception modals to keep exception info clear in console. *)
      let no_exc_modal_arg : string list =
        Common_state.url_args "no_exc_modal"
      in
      let send_worker_exceptions_to_modal =
        not
          (match no_exc_modal_arg with
          | s :: _ when String.equal s "true" -> true
          | _ -> false)
      in

      kasa_worker##.onmessage :=
        worker_onmessage ~debug_printing:true ~worker_name:"kasa_worker"
          ~exception_creates_modal:send_worker_exceptions_to_modal
          ~worker_receive_f:(Kasa_client.receive kasa_mailbox);
      kamoha_worker##.onmessage :=
        worker_onmessage ~debug_printing:true ~worker_name:"kamoha_worker"
          ~exception_creates_modal:send_worker_exceptions_to_modal
          ~worker_receive_f:(Kamoha_client.receive kamoha_mailbox);
      kastor_worker##.onmessage :=
        worker_onmessage ~debug_printing:true ~worker_name:"kastor_worker"
          ~exception_creates_modal:send_worker_exceptions_to_modal
          ~worker_receive_f:(Kastor_client.receive update_stor_state);
      sim_worker##.onmessage :=
        worker_onmessage ~debug_printing:false ~worker_name:"sim_worker"
          ~exception_creates_modal:send_worker_exceptions_to_modal
          ~worker_receive_f:self#receive;

      let onerror ~worker_name =
        Dom.handler (fun _ ->
            let error_string : string =
              "Error in " ^ worker_name ^ ", it might not be running anymore."
            in
            let () = Common.debug ~loc:__LOC__ error_string in
            Ui_common.open_modal_error ~is_critical:false
              ~error_content:error_string;

            let () = is_running <- false in
            Js._true)
      in
      sim_worker##.onerror := onerror ~worker_name:"sim_worker";
      kasa_worker##.onerror := onerror ~worker_name:"kasa_worker";
      kamoha_worker##.onerror := onerror ~worker_name:"kamoha_worker";
      kastor_worker##.onerror := onerror ~worker_name:"kastor_worker"

    method private sleep timeout = Js_of_ocaml_lwt.Lwt_js.sleep timeout

    method private post_message (message_text : string) : unit =
      sim_worker##postMessage message_text

    inherit Mpi_api.manager ()

    inherit
      Kasa_client.new_uniform_client
        ~is_running:(fun () -> true)
        ~post:(fun message_text ->
          let () = Common.log_group "Message posted to kasa_worker" in
          let () = Common.debug ~loc:__LOC__ (Js.string message_text) in
          let () = Common.log_group_end () in
          kasa_worker##postMessage message_text)
        kasa_mailbox

    inherit
      Kamoha_client.new_client
        ~post:(fun message_text ->
          let () = Common.log_group "Message posted to kamoha_worker" in
          let () = Common.debug ~loc:__LOC__ (Js.string message_text) in
          let () = Common.log_group_end () in
          kamoha_worker##postMessage message_text)
        kamoha_mailbox

    inherit
      Kastor_client.new_client
        ~post:(fun message_text ->
          let () = Common.log_group "Message posted to kastor_worker" in
          let () = Common.debug ~loc:__LOC__ (Js.string message_text) in
          let () = Common.log_group_end () in
          kastor_worker##postMessage message_text)
        stor_state

    val mutable kasa_locator = []

    method project_parse ~patternSharing overwrites =
      self#secret_project_parse
      >>= Api_common.result_bind_lwt ~ok:(fun out ->
              let load =
                self#secret_simulation_load patternSharing out overwrites
              in
              let init = self#init_static_analyser out in
              let locators =
                init
                >>= Result_util.fold
                      ~error:(fun e ->
                        let () = kasa_locator <- [] in
                        Lwt.return (Result_util.error e))
                      ~ok:(fun () ->
                        self#secret_get_pos_of_rules_and_vars
                        >>= Result_util.fold
                              ~ok:(fun infos ->
                                let () = kasa_locator <- infos in
                                Lwt.return (Result_util.ok ()))
                              ~error:(fun e ->
                                let () = kasa_locator <- [] in
                                Lwt.return (Result_util.error e)))
              in
              load >>= Api_common.result_bind_lwt ~ok:(fun () -> locators))

    method get_influence_map_node_at ~filename pos : _ Api.result Lwt.t =
      List.find_opt
        (fun (_, x) -> Loc.is_included_in filename pos x)
        kasa_locator
      |> Option_util.map fst
      |> Result_util.ok ?status:None
      |> Lwt.return

    method is_running = is_running

    method terminate =
      let () = sim_worker##terminate in
      kasa_worker##terminate

    method is_computing =
      self#sim_is_computing
      || Kasa_client.is_computing kasa_mailbox
      || self#story_is_computing
  end
