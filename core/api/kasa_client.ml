(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type mailbox =
  ( int,
    (Yojson.Basic.t, Exception_without_parameter.method_handler) Result.result
    Lwt.u )
  Hashtbl.t

let reply_of_string x =
  match Yojson.Basic.from_string x with
  | `Assoc [ ("id", `Int id); ("code", `String "ERROR"); ("data", err) ] ->
    Some id, Result.Error (Exception_without_parameter.of_json err)
  | `Assoc [ ("id", `Int id); ("code", `String "SUCCESS"); ("data", data) ] ->
    Some id, Result.Ok data
  | x ->
    ( None,
      Result.Error
        (Exception_without_parameter.add_uncaught_error
           (Exception_without_parameter.build_uncaught_exception
              ~file_name:"kasa_client"
              ~message:
                ("Invalid response from KaSa: " ^ Yojson.Basic.to_string x)
              Exit)
           Exception_without_parameter.empty_error_handler) )

let receive mailbox x =
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

    method private raw_message post request =
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
        result
      ) else
        Lwt.return_error
          (Exception_without_parameter.add_uncaught_error
             (Exception_without_parameter.build_uncaught_exception
                ~file_name:"kasa_client" ~message:"KaSa agent is dead" Exit)
             Exception_without_parameter.empty_error_handler)

    method private message request =
      self#raw_message post (fun outb -> Yojson.Basic.to_buffer outb request)

    method init_static_analyser_raw compil =
      let request outbuf =
        let () = Buffer.add_string outbuf "[ \"INIT\", " in
        let () = Buffer.add_string outbuf compil in
        Buffer.add_string outbuf "]"
      in
      Lwt_result.bind_result (self#raw_message post request) (function
        | `Null -> Result.Ok ()
        | x ->
          Result.Error
            (Exception_without_parameter.add_uncaught_error
               (Exception_without_parameter.build_uncaught_exception
                  ~file_name:"kasa_client"
                  ~message:
                    ("Not a KaSa INIT response: " ^ Yojson.Basic.to_string x)
                  Exit)
               Exception_without_parameter.empty_error_handler))

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
      Lwt_result.bind_result (self#message request) (fun x -> Result.Ok x)

    method get_pos_of_rules_and_vars =
      let request = `List [ `String "INFLUENCE_MAP_NODES_LOCATION" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.pos_of_rules_and_vars_of_json x))

    method get_influence_map_raw accuracy =
      let request =
        `List
          (`String "INFLUENCE_MAP"
          ::
          (match accuracy with
          | None -> []
          | Some a -> [ Public_data.accuracy_to_json a ]))
      in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Yojson.Basic.to_string x))

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
      Lwt_result.bind_result (self#message request) (fun x ->
          let o = Public_data.local_influence_map_of_json x in
          Result.Ok o)

    method get_initial_node =
      let request = `List [ `String "INFLUENCE_MAP_ORIGINAL_NODE" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          let o =
            JsonUtil.to_option Public_data.refined_influence_node_of_json x
          in
          Result.Ok o)

    method get_next_node json =
      let request =
        `List
          [
            `String "INFLUENCE_MAP_NEXT_NODE";
            JsonUtil.of_option Public_data.short_influence_node_to_json json;
          ]
      in
      Lwt_result.bind_result (self#message request) (fun x ->
          let o =
            JsonUtil.to_option Public_data.refined_influence_node_of_json x
          in
          Result.Ok o)

    method get_previous_node json =
      let request =
        `List
          [
            `String "INFLUENCE_MAP_PREVIOUS_NODE";
            JsonUtil.of_option Public_data.short_influence_node_to_json json;
          ]
      in
      Lwt_result.bind_result (self#message request) (fun x ->
          let o =
            JsonUtil.to_option Public_data.refined_influence_node_of_json x
          in
          Result.Ok o)

    method get_nodes_of_influence_map accuracy =
      let request =
        `List
          (`String "INFLUENCE_MAP_ALL_NODES"
          ::
          (match accuracy with
          | None -> []
          | Some a -> [ Public_data.accuracy_to_json a ]))
      in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.nodes_of_influence_map_of_json x))

    method get_dead_rules =
      let request = `List [ `String "DEAD_RULES" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.dead_rules_of_json x))

    method get_dead_agents =
      let request = `List [ `String "DEAD_AGENTS" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.json_to_dead_agents x))

    method get_non_weakly_reversible_transitions =
      let request = `List [ `String "NON_WEAKLY_REVERSIBLE_TRANSITIONS" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.separating_transitions_of_json x))

    method get_constraints_list =
      let request = `List [ `String "CONSTRAINTS" ] in
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.lemmas_list_of_json x))

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
      Lwt_result.bind_result (self#message request) (fun x ->
          Result.Ok (Public_data.scc_of_json x))
  end

class new_uniform_client ~is_running ~post (mailbox : mailbox) :
  Api.uniform_manager_static_analysis =
  let raw = new new_client ~is_running ~post mailbox in
  object
    method init_static_analyser_raw compil =
      raw#init_static_analyser_raw compil >|= Api_common.result_kasa

    method init_static_analyser compil =
      raw#init_static_analyser compil >|= Api_common.result_kasa

    method get_contact_map accuracy =
      raw#get_contact_map accuracy >|= Api_common.result_kasa

    method get_influence_map_raw accuracy =
      raw#get_influence_map_raw accuracy >|= Api_common.result_kasa

    method get_local_influence_map ?fwd ?bwd ?origin ~total accuracy =
      raw#get_local_influence_map ?fwd ?bwd ?origin ~total accuracy
      >|= Api_common.result_kasa

    method get_initial_node = raw#get_initial_node >|= Api_common.result_kasa

    method get_next_node json =
      raw#get_next_node json >|= Api_common.result_kasa

    method get_previous_node json =
      raw#get_previous_node json >|= Api_common.result_kasa

    method secret_get_pos_of_rules_and_vars =
      raw#get_pos_of_rules_and_vars >|= Api_common.result_kasa

    method get_nodes_of_influence_map accuracy =
      raw#get_nodes_of_influence_map accuracy >|= Api_common.result_kasa

    method get_dead_rules = raw#get_dead_rules >|= Api_common.result_kasa
    method get_dead_agents = raw#get_dead_agents >|= Api_common.result_kasa

    method get_non_weakly_reversible_transitions =
      raw#get_non_weakly_reversible_transitions >|= Api_common.result_kasa

    method get_constraints_list =
      raw#get_constraints_list >|= Api_common.result_kasa

    method get_potential_polymers accuracy_cm accuracy_scc =
      raw#get_potential_polymers accuracy_cm accuracy_scc
      >|= Api_common.result_kasa
  end
