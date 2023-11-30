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

class t exec_command message_delimiter =
  let switch_re = Re.compile (Re.str "KappaSwitchman") in
  let kamoha =
    let command = Re.replace_string switch_re ~by:"KaMoHa" exec_command in
    Lwt_process.open_process_full
      (command, [| command; "--delimiter"; Char.escaped message_delimiter |])
  in
  let kasim =
    let command = Re.replace_string switch_re ~by:"KaSimAgent" exec_command in
    Lwt_process.open_process_full
      (command, [| command; "--delimiter"; Char.escaped message_delimiter |])
  in
  (*let kastor =
    let command = Re.replace_string  switch_re ~by:"KaSimAgent" exec_command in
    Lwt_process.open_process_full
      (command,[|comman;"--delimiter";Char.escaped message_delimiter|]) in*)
  let kasa =
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
        serve kamoha#stdout message_delimiter
          (Kamoha_client.receive moha_mailbox)
        |> Lwt.ignore_result
      in
      (*let () =
        serve
          kastor#stdout message_delimiter (Kastor_client.receive update_stor_state)
        |> Lwt.ignore_result in*)
      let () =
        serve kasa#stdout message_delimiter (Kasa_client.receive sa_mailbox)
        |> Lwt.ignore_result
      in
      serve kasim#stdout message_delimiter self#receive |> Lwt.ignore_result

    method terminate =
      kamoha#terminate;
      kasim#terminate;
      (*kastor#terminate;*)
      kasa#terminate

    method is_running =
      kamoha#state = Lwt_process.Running
      && kasim#state = Lwt_process.Running
      && (*kastor#state = Lwt_process.Running &&*)
      kasa#state = Lwt_process.Running

    method is_computing =
      self#sim_is_computing
      || Kasa_client.is_computing sa_mailbox
      || (*self#story_is_computing ||*) Kamoha_client.is_computing moha_mailbox

    inherit
      Kasa_client.new_client
        ~is_running:(fun () -> kasa#state = Lwt_process.Running)
        ~post:(post kasa#stdin message_delimiter)
        sa_mailbox

    (*inherit Kastor_client.new_client
        ~post:(post kastor#stdin message_delimiter)
        stor_state*)

    inherit
      Kamoha_client.new_client
        ~post:(post kamoha#stdin message_delimiter)
        moha_mailbox

    method private sleep timeout = Lwt_unix.sleep timeout
    method private post_message txt = post kasim#stdin message_delimiter txt
    inherit Mpi_api.manager ()
    val mutable kasa_locator = []

    method project_parse ~patternSharing overwrites =
      self#secret_project_parse
      >>= Api_common.result_bind_lwt ~ok:(fun out ->
              let load =
                self#secret_simulation_load patternSharing out overwrites
              in
              let init = self#init_static_analyser out in
              let locators =
                init >>= function
                | Result.Error _ as e ->
                  let () = kasa_locator <- [] in
                  Lwt.return (Api_common.result_kasa e)
                | Result.Ok () ->
                  self#get_pos_of_rules_and_vars >>= ( function
                  | Result.Ok infos ->
                    let () = kasa_locator <- infos in
                    Lwt.return (Result_util.ok ())
                  | Result.Error _ as e ->
                    let () = kasa_locator <- [] in
                    Lwt.return (Api_common.result_kasa e) )
              in
              load >>= Api_common.result_bind_lwt ~ok:(fun () -> locators))

    method get_influence_map_node_at ~filename pos : _ Api.result Lwt.t =
      List.find_opt
        (fun (_, x) -> Loc.is_included_in filename pos x)
        kasa_locator
      |> Option_util.map fst
      |> Result_util.ok ?status:None
      |> Lwt.return
  end
