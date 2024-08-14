open Lwt.Infix

let serve chan delimiter process_command : unit Lwt.t =
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char_opt chan >>= function
    | Some char ->
      if char = delimiter then (
        let m = Buffer.contents buffer in
        process_command m;
        let () = Buffer.reset buffer in
        aux_serve ()
      ) else (
        let () = Buffer.add_char buffer char in
        aux_serve ()
      )
    | None -> Lwt.return_unit
  in
  aux_serve ()

let post chan message_delimiter message_text =
  Lwt.ignore_result
    (Lwt_io.atomic
       (fun chan ->
         Lwt_io.write chan message_text >>= fun () ->
         Lwt_io.write_char chan message_delimiter)
       chan)

(* Manager of part of Api.concrete_manager, interfacing with processes of kamoha, kasim, kasa, but not kastor
   kastor is directly run by the main thread in the runtime_processes module
   Does not match exactly Api.concrete_manager as kastor is missing *)
class t exec_command message_delimiter =
  let switch_re = Re.compile (Re.str "KappaSwitchman") in
  let kamoha_process =
    let command = Re.replace_string switch_re ~by:"KaMoHa" exec_command in
    Lwt_process.open_process_full
      (command, [| command; "--delimiter"; Char.escaped message_delimiter |])
  in
  let kasim_process =
    let command = Re.replace_string switch_re ~by:"KaSimAgent" exec_command in
    Lwt_process.open_process_full
      (command, [| command; "--delimiter"; Char.escaped message_delimiter |])
  in
  (*let kastor_process =
    let command = Re.replace_string  switch_re ~by:"KaSimAgent" exec_command in
    Lwt_process.open_process_full
      (command,[|comman;"--delimiter";Char.escaped message_delimiter|]) in*)
  let kasa_process =
    let command = Re.replace_string switch_re ~by:"KaSaAgent" exec_command in
    Lwt_process.open_process_full
      (command, [| command; "--delimiter"; Char.escaped message_delimiter |])
  in
  let moha_mailbox = Kamoha_client.new_mailbox () in
  let sa_mailbox = Kasa_client.new_mailbox () in
  (*let stor_state,update_stor_state = Kastor_client.init_state () in*)
  object (self)
    initializer
      let () =
        serve kamoha_process#stdout message_delimiter
          (Kamoha_client.receive moha_mailbox)
        |> Lwt.ignore_result
      in
      (*let () =
        serve
          kastor_process#stdout message_delimiter (Kastor_client.receive update_stor_state)
        |> Lwt.ignore_result in*)
      let () =
        serve kasa_process#stdout message_delimiter
          (Kasa_client.receive sa_mailbox)
        |> Lwt.ignore_result
      in
      serve kasim_process#stdout message_delimiter self#receive
      |> Lwt.ignore_result

    method terminate =
      kamoha_process#terminate;
      kasim_process#terminate;
      (*kastor_process#terminate;*)
      kasa_process#terminate

    method is_running =
      kamoha_process#state = Lwt_process.Running
      && kasim_process#state = Lwt_process.Running
      && (*kastor_process#state = Lwt_process.Running &&*)
      kasa_process#state = Lwt_process.Running

    method is_computing =
      self#sim_is_computing
      || Kasa_client.is_computing sa_mailbox
      || (*self#story_is_computing ||*) Kamoha_client.is_computing moha_mailbox

    inherit
      Kasa_client.new_client
        ~is_running:(fun () -> kasa_process#state = Lwt_process.Running)
        ~post:(post kasa_process#stdin message_delimiter)
        sa_mailbox

    (*inherit Kastor_client.new_client
        ~post:(post kastor_process#stdin message_delimiter)
        stor_state*)

    inherit
      Kamoha_client.new_client
        ~post:(post kamoha_process#stdin message_delimiter)
        moha_mailbox

    method private sleep timeout = Lwt_unix.sleep timeout

    inherit
      Kasim_client.new_client
        ~post:(fun message_text ->
          post kasim_process#stdin message_delimiter message_text)
        ()

    val mutable kasa_locator = []

    method project_parse ~patternSharing overwrites =
      self#secret_project_parse
      >>= Api_common.result_bind_with_lwt ~ok:(fun out ->
              let load =
                self#secret_simulation_load patternSharing out overwrites
              in
              let init = self#init_static_analyser out in
              let locators =
                init >>= fun result ->
                match result.value with
                | Result.Error _ ->
                  let () = kasa_locator <- [] in
                  Lwt.return result
                | Result.Ok () ->
                  (* TODO: check why secret, use sth else *)
                  self#secret_get_pos_of_rules_and_vars >>= fun result ->
                  (match result.value with
                  | Result.Ok infos ->
                    let () = kasa_locator <- infos in
                    Lwt.return (Result_util.ok ())
                  | Result.Error _ ->
                    let () = kasa_locator <- [] in
                    Lwt.return (Result_util.map (fun _ -> ()) result))
              in
              load >>= Api_common.result_bind_with_lwt ~ok:(fun () -> locators))

    method get_influence_map_node_at ~filename pos : _ Api.lwt_result =
      List.find_opt
        (fun (_, x) -> Loc.is_included_in filename pos x)
        kasa_locator
      |> Option_util.map fst
      |> Result_util.ok ?status:None
      |> Lwt.return
  end
