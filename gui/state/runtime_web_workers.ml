(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(** Manages worker message reception.

    Manages the message reception, and opens an error modal to avert the user by default.
    The modal can be disabled to keep original exception in developer tools by providing to the app the `?no_exc_modal=true` argument *)
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
      Kappa_webapp_lib.Ui_common.open_modal_error ~is_critical:true
        ~error_content:
          ("Worker " ^ worker_name ^ " raised the following exception: \r"
         ^ Printexc.to_string e);
      raise e
    ) else
      process_message
  in
  Dom.handler handler_f

let no_exc_modal_arg : string list = Common_state.url_args "no_exc_modal"

let send_worker_exceptions_to_modal =
  not
    (match no_exc_modal_arg with
    | s :: _ when String.equal s "true" -> true
    | _ -> false)

let onerror ~worker_name ~apply_onerror =
  Dom.handler (fun _ ->
      let error_string : string =
        "Error in " ^ worker_name ^ ", it might not be running anymore."
      in
      let () = Common.debug ~loc:__LOC__ error_string in
      Ui_common.open_modal_error ~is_critical:false ~error_content:error_string;
      apply_onerror ();
      Js._true)

class type concrete_manager_without_kasim = object
  inherit Api.manager_model
  inherit Api.manager_static_analysis
  inherit Api.manager_stories

  method get_influence_map_node_at :
    filename:string ->
    Loc.position ->
    Public_data.refined_influence_node option Api.lwt_result

  method is_running : bool
  method terminate : unit
  method is_computing : bool

  (* protected: used in inheritance, so cannot be private,  *)
  method private apply_onerror : unit

  method private project_parse_without_kasim :
    simulation_load:
      (Pattern.sharing_level ->
      Ast.parsing_compil ->
      (string * Nbr.t) list ->
      unit Api.lwt_result) ->
    patternSharing:Pattern.sharing_level ->
    (string * Nbr.t) list ->
    unit Api.lwt_result
end

class virtual manager_without_kasim () : concrete_manager_without_kasim =
  let kasa_worker = Worker.create "KaSaWorker.js" in
  let kasa_mailbox = Kasa_client.new_mailbox () in
  let kamoha_worker = Worker.create "KaMoHaWorker.js" in
  let kamoha_mailbox = Kamoha_client.new_mailbox () in
  let kastor_worker = Worker.create "KaStorWorker.js" in
  let stor_state, update_stor_state = Kastor_client.init_state () in

  object (self)
    val mutable is_running = true
    method private apply_onerror = is_running <- false

    initializer
      (* The url argument `no_exc_modal=true` disables exception modals to keep exception info clear in console. *)
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

      let apply_onerror () = self#apply_onerror in
      kasa_worker##.onerror := onerror ~worker_name:"kasa_worker" ~apply_onerror;
      kamoha_worker##.onerror :=
        onerror ~worker_name:"kamoha_worker" ~apply_onerror;
      kastor_worker##.onerror :=
        onerror ~worker_name:"kastor_worker" ~apply_onerror

    inherit
      Kasa_client.new_client
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

    method private project_parse_without_kasim ~simulation_load ~patternSharing
        overwrites =
      self#secret_project_parse
      >>= Api_common.result_bind_with_lwt ~ok:(fun out ->
              (* load the sim so that kasa can run on it *)
              let load : unit Api.lwt_result =
                simulation_load patternSharing out overwrites
              in
              let init_kasa = self#init_static_analyser out in
              let locators =
                init_kasa
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
              load >>= Api_common.result_bind_with_lwt ~ok:(fun () -> locators))

    method get_influence_map_node_at ~filename pos : _ Api.lwt_result =
      List.find_opt
        (fun (_, x) -> Loc.is_included_in filename pos x)
        kasa_locator
      |> Option_util.map fst
      |> Result_util.ok ?status:None
      |> Lwt.return

    method is_running = is_running
    method terminate = kasa_worker##terminate

    method is_computing =
      Kasa_client.is_computing kasa_mailbox || self#story_is_computing
  end

class runtime_kasim_as_web_worker () : Api.concrete_manager =
  let kasim_worker = Worker.create "KaSimWorker.js" in
  object (self)
    inherit manager_without_kasim () as without_kasim

    initializer
      kasim_worker##.onmessage :=
        worker_onmessage ~debug_printing:false ~worker_name:"kasim_worker"
          ~exception_creates_modal:send_worker_exceptions_to_modal
          ~worker_receive_f:self#receive;
      let apply_onerror () = without_kasim#apply_onerror in
      kasim_worker##.onerror :=
        onerror ~worker_name:"kasim_worker" ~apply_onerror

    inherit
      Kasim_client.new_client
        ~post:(fun message_text ->
          let () = Common.log_group "Message posted to kasim_worker" in
          let () = Common.debug ~loc:__LOC__ (Js.string message_text) in
          let () = Common.log_group_end () in
          kasim_worker##postMessage message_text)
        ()

    method private sleep timeout = Js_of_ocaml_lwt.Lwt_js.sleep timeout

    method terminate =
      without_kasim#terminate;
      kasim_worker##terminate

    method is_computing = without_kasim#is_computing || self#sim_is_computing

    method project_parse ~patternSharing overwrites =
      let simulation_load patternSharing out overwrites =
        self#secret_simulation_load patternSharing out overwrites
      in
      without_kasim#project_parse_without_kasim ~simulation_load ~patternSharing
        overwrites
  end

class runtime_kasim_embedded_in_main_thread () : Api.concrete_manager =
  let system_process : Kappa_facade.system_process =
    object
      method min_run_duration () = 0.1
      method yield = Js_of_ocaml_lwt.Lwt_js.yield

      method log ?exn (msg : string) =
        let () = ignore exn in
        let () =
          Common.debug ~loc:__LOC__
            (Js.string
               (Format.sprintf
                  "[State_runtime.embedded] embedded_manager#log: %s" msg))
        in
        Lwt.return_unit
    end
  in
  object (self)
    inherit manager_without_kasim () as without_kasim

    (* Use embedded kasim runtime here *)
    inherit Kasim_runtime.manager system_process
    method is_running = true

    method terminate =
      without_kasim#terminate;
      () (*TODO*)

    method is_computing = true (*TODO*)

    method project_parse ~patternSharing overwrites =
      let simulation_load patternSharing out overwrites =
        self#secret_simulation_load patternSharing out overwrites
      in
      without_kasim#project_parse_without_kasim ~simulation_load ~patternSharing
        overwrites
  end
