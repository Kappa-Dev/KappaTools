(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type mailbox = (int, Yojson.Basic.t Api.result Lwt.u) Hashtbl.t

let reply_of_string (x : string) : int option * Yojson.Basic.t Api.result =
  match Yojson.Basic.from_string x with
  | `Assoc [ ("id", `Int id); ("code", `String "ERROR"); ("data", err) ] ->
    ( Some id,
      Exception_without_parameter.of_json err
      |> Api_common.err_result_of_exceptions )
  | `Assoc [ ("id", `Int id); ("code", `String "SUCCESS"); ("data", data) ] ->
    Some id, Result_util.ok data
  | x ->
    ( None,
      Exception_without_parameter.build_uncaught_exception
        ~file_name:"kasa_client"
        ~message:("Invalid response from KaSa: " ^ Yojson.Basic.to_string x)
        Exit
      |> Api_common.message_of_uncaught_exception
      |> Api_common.err_result_of_msg )

let receive (mailbox : mailbox) (x : string) : unit =
  match reply_of_string x with
  | Some id, out ->
    let thread = Hashtbl.find mailbox id in
    let () = Hashtbl.remove mailbox id in
    Lwt.wakeup thread out
  | None, _ -> ()

let new_mailbox () = Hashtbl.create 2
let is_computing mailbox = Hashtbl.length mailbox <> 0

class new_client ~is_running ~post (mailbox : mailbox) :
  Api.manager_static_analysis =
  object (self)
    val mutable id = 0

    method private raw_message (post : string -> unit)
        (request : Buffer.t -> unit) : 'a Api.lwt_result =
      if is_running () then (
        let result, feeder = Lwt.task () in
        let outbuf = Buffer.create 1024 in
        let () = Buffer.add_string outbuf "{id:" in
        let () = Buffer.add_string outbuf (string_of_int id) in
        let () = Buffer.add_string outbuf ",data:" in
        let () = request outbuf in
        let () = Buffer.add_string outbuf "}" in
        let () = post (Buffer.contents outbuf) in
        let () = Hashtbl.replace mailbox id feeder in
        let () = id <- id + 1 in
        (* TODO: change mailbox to have Api.result type inside *)
        result
      ) else
        Exception_without_parameter.build_uncaught_exception
          ~file_name:"kasa_client" ~message:"KaSa agent is dead" Exit
        |> Api_common.message_of_uncaught_exception
        |> Api_common.err_result_of_msg |> Lwt.return

    method private message request =
      self#raw_message post (fun outb -> Yojson.Basic.to_buffer outb request)

    method init_static_analyser_raw compil =
      let request outbuf =
        let () = Buffer.add_string outbuf "[ \"INIT\", " in
        let () = Buffer.add_string outbuf compil in
        Buffer.add_string outbuf "]"
      in
      self#raw_message post request
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `Null -> Result_util.ok () |> Lwt.return
            | x ->
              Exception_without_parameter.build_uncaught_exception
                ~file_name:"kasa_client"
                ~message:
                  ("Not a KaSa INIT response: " ^ Yojson.Basic.to_string x)
                Exit
              |> Api_common.message_of_uncaught_exception
              |> Api_common.err_result_of_msg |> Lwt.return)

    method init_static_analyser compil =
      self#init_static_analyser_raw
        (Yojson.Basic.to_string (Ast.compil_to_json compil))

    method get_contact_map accuracy =
      let request =
        `List
          (`String "CONTACT_MAP"
          ::
          (match accuracy with
          | None -> []
          | Some a -> [ Public_data.accuracy_to_json a ]))
      in
      self#message request

    method secret_get_pos_of_rules_and_vars =
      let request = `List [ `String "INFLUENCE_MAP_NODES_LOCATION" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Result_util.ok (Public_data.pos_of_rules_and_vars_of_json x)
              |> Lwt.return)

    method get_influence_map_raw accuracy =
      let request =
        `List
          (`String "INFLUENCE_MAP"
          ::
          (match accuracy with
          | None -> []
          | Some a -> [ Public_data.accuracy_to_json a ]))
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Result_util.ok (Yojson.Basic.to_string x) |> Lwt.return)

    method get_local_influence_map ?fwd ?bwd ?origin ~total accuracy =
      let request =
        `List
          (`String "INFLUENCE_MAP"
          :: (fun accuracy l ->
               match accuracy with
               | None -> l
               | Some a -> Public_data.accuracy_to_json a :: l)
               accuracy
               [
                 JsonUtil.of_option JsonUtil.of_int fwd;
                 JsonUtil.of_option JsonUtil.of_int bwd;
                 JsonUtil.of_int total;
                 JsonUtil.of_option Public_data.short_influence_node_to_json
                   origin;
               ])
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Result_util.ok (Public_data.local_influence_map_of_json x)
              |> Lwt.return)

    method get_initial_node =
      let request = `List [ `String "INFLUENCE_MAP_ORIGINAL_NODE" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              JsonUtil.to_option Public_data.refined_influence_node_of_json x
              |> Result_util.ok |> Lwt.return)

    method get_next_node json =
      let request =
        `List
          [
            `String "INFLUENCE_MAP_NEXT_NODE";
            JsonUtil.of_option Public_data.short_influence_node_to_json json;
          ]
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              JsonUtil.to_option Public_data.refined_influence_node_of_json x
              |> Result_util.ok |> Lwt.return)

    method get_previous_node json =
      let request =
        `List
          [
            `String "INFLUENCE_MAP_PREVIOUS_NODE";
            JsonUtil.of_option Public_data.short_influence_node_to_json json;
          ]
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              JsonUtil.to_option Public_data.refined_influence_node_of_json x
              |> Result_util.ok |> Lwt.return)

    method get_nodes_of_influence_map accuracy =
      let request =
        `List
          (`String "INFLUENCE_MAP_ALL_NODES"
          ::
          (match accuracy with
          | None -> []
          | Some a -> [ Public_data.accuracy_to_json a ]))
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.nodes_of_influence_map_of_json x
              |> Result_util.ok |> Lwt.return)

    method get_dead_rules =
      let request = `List [ `String "DEAD_RULES" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.dead_rules_of_json x |> Result_util.ok |> Lwt.return)

    method get_dead_agents =
      let request = `List [ `String "DEAD_AGENTS" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.json_to_dead_agents x |> Result_util.ok |> Lwt.return)

    method get_non_weakly_reversible_transitions =
      let request = `List [ `String "NON_WEAKLY_REVERSIBLE_TRANSITIONS" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.separating_transitions_of_json x
              |> Result_util.ok |> Lwt.return)

    method get_constraints_list =
      let request = `List [ `String "CONSTRAINTS" ] in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.lemmas_list_of_json x |> Result_util.ok |> Lwt.return)

    method get_potential_polymers accuracy_cm accuracy_scc =
      let request =
        `List
          (`String "POLYMERS"
          ::
          (match accuracy_cm, accuracy_scc with
          | None, None -> []
          | Some a, None -> [ Public_data.accuracy_to_json a ]
          | Some a, Some b ->
            [ Public_data.accuracy_to_json a; Public_data.accuracy_to_json b ]
          | None, Some b ->
            [
              Public_data.accuracy_to_json Public_data.Low;
              Public_data.accuracy_to_json b;
            ]))
      in
      self#message request
      >>= Api_common.result_bind_with_lwt ~ok:(fun x ->
              Public_data.scc_of_json x |> Result_util.ok |> Lwt.return)
  end
