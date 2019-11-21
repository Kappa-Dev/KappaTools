open Lwt.Infix

let serve chan delimiter process_command : unit Lwt.t =
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char_opt chan >>= function
    | Some char ->
      if char = delimiter then
        let m = Buffer.contents buffer in
        process_command m;
        let () = Buffer.reset buffer in aux_serve ()
      else
        let () = Buffer.add_char buffer char in
        aux_serve ()
    | None -> Lwt.return_unit in
  aux_serve ()

let post chan message_delimiter message_text =
  Lwt.ignore_result
    (Lwt_io.atomic
       (fun chan ->
          Lwt_io.write chan message_text >>= fun () ->
          Lwt_io.write_char chan message_delimiter)
       chan)

class t bin_dir message_delimiter =
  let kamoha =
    Lwt_process.open_process_full
      (bin_dir ^ "KaMoHa",[|bin_dir^"KaMoHa";"--delimiter";Char.escaped message_delimiter|]) in
  let kasim =
    Lwt_process.open_process_full
      (bin_dir ^ "KaSimAgent",[|bin_dir^"KaSimAgent";"--delimiter";Char.escaped message_delimiter|]) in
  let kastor =
    Lwt_process.open_process_full
      (bin_dir ^ "KaStor",[|bin_dir^"KaStor";"--delimiter";Char.escaped message_delimiter|]) in
  let kasa =
    Lwt_process.open_process_full
      (bin_dir ^ "KaSaAgent",[|bin_dir^"KaSaAgent";"--delimiter";Char.escaped message_delimiter|]) in
  let moha_mailbox = Kamoha_client.new_mailbox () in
  let sa_mailbox = Kasa_client.new_mailbox () in
  let stor_state,update_stor_state = Kastor_client.init_state () in
  object(self)
    initializer
      let () =
        serve
          kamoha#stdout message_delimiter (Kamoha_client.receive moha_mailbox)
        |> Lwt.ignore_result in
      let () =
        serve
          kastor#stdout message_delimiter (Kastor_client.receive update_stor_state)
        |> Lwt.ignore_result in
      let () =
        serve
          kasa#stdout message_delimiter (Kasa_client.receive sa_mailbox)
        |> Lwt.ignore_result in
      serve kasim#stdout message_delimiter self#receive
      |> Lwt.ignore_result

    method terminate =
      kamoha#terminate;
      kasim#terminate;
      kastor#terminate;
      kasa#terminate

    method is_running =
      kamoha#state = Lwt_process.Running &&
      kasim#state = Lwt_process.Running &&
      kastor#state = Lwt_process.Running &&
      kasa#state = Lwt_process.Running

    method is_computing =
      self#sim_is_computing || Kasa_client.is_computing sa_mailbox ||
      self#story_is_computing || Kamoha_client.is_computing moha_mailbox

    inherit Kasa_client.new_client
        ~is_running:(fun () -> kasa#state = Lwt_process.Running)
        ~post:(post kasa#stdin message_delimiter)
        sa_mailbox

    inherit Kastor_client.new_client
        ~post:(post kastor#stdin message_delimiter)
        stor_state

    inherit Kamoha_client.new_client
        ~post:(post kamoha#stdin message_delimiter)
        moha_mailbox

    method private sleep timeout = Lwt_unix.sleep timeout
    method private post_message txt = post kasim#stdin message_delimiter txt
    inherit Mpi_api.manager ()

    method project_parse overwrites =
      self#secret_project_parse >>=
      Api_common.result_bind_lwt
        ~ok:(fun out ->
            self#secret_simulation_load out overwrites >>=
            Api_common.result_bind_lwt
              ~ok:(fun () -> self#init_static_analyser out >|= Api_common.result_kasa))
  end
