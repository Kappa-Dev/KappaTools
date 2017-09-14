(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type mailbox =
  (int, (Yojson.Basic.json, string) Result.result Lwt.u) Hashtbl.t

let reply_of_string x =
  match Yojson.Basic.from_string x with
  | `Assoc [ "id", ` Int id; "code", `String "ERROR"; "data", err ] ->
    Some id,Result.Error (Yojson.Basic.to_string err)
  | `Assoc [ "id", ` Int id; "code", `String "SUCCESS"; "data", data ] ->
    Some id,Result.Ok data
  | x ->
    None, Result.Error ("Invalid response from KaSa: "^Yojson.Basic.to_string x)

let receive mailbox x =
  match reply_of_string x with
  | Some id, out -> Lwt.wakeup (Hashtbl.find mailbox id) out
  | None, _ -> ()

let new_mailbox () = Hashtbl.create 2

class virtual new_client ~post (mailbox : mailbox) :
  Api.manager_static_analysis =
  object(self)
    val mutable id = 0
    method virtual is_running : bool

    method private raw_message post request =
      if self#is_running then
        let result,feeder = Lwt.task () in
        let outbuf = Bi_outbuf.create 1024 in
        let () = Bi_outbuf.add_string outbuf "{id:" in
        let () = Bi_outbuf.add_string outbuf (string_of_int id) in
        let () = Bi_outbuf.add_string outbuf ",data:" in
        let () = request outbuf in
        let () = Bi_outbuf.add_string outbuf "}" in
        let () = post (Bi_outbuf.contents outbuf) in
        let () = Hashtbl.replace mailbox id feeder in
        let () = id <- id+1 in
        result
      else
        Lwt.return_error "KaSa agent is dead"
    method private message post request =
      self#raw_message post (fun outb -> Yojson.Basic.to_outbuf outb request)

    method init_static_analyser_raw compil =
      let request outbuf =
        let () = Bi_outbuf.add_string outbuf "[ \"INIT\", " in
        let () = Bi_outbuf.add_string outbuf compil in
        Bi_outbuf.add_string outbuf "]" in
      Lwt_result.bind_result
        (self#raw_message post request)
        (function `Null -> Result.Ok ()
                | x -> Result.Error
                         ("Not a KaSa INIT response: "^
                          Yojson.Basic.to_string x))
    method init_static_analyser compil =
      self#init_static_analyser_raw
        (Yojson.Basic.to_string (Ast.compil_to_json compil))

    method get_contact_map accuracy =
      let request =
        `List ( `String "CONTACT_MAP" :: match accuracy with
          | None -> []
          | Some a -> [Public_data.accuracy_to_json a]) in
      Lwt_result.bind_result
        (self#message post request)
        (fun x -> Result.Ok x)
    method get_influence_map accuracy =
      let request =
        `List ( `String "INFLUENCE_MAP" :: match accuracy with
          | None -> []
          | Some a -> [Public_data.accuracy_to_json a]) in
      Lwt_result.bind_result
        (self#message post request)
        (fun x -> Result.Ok x)
    method get_local_influence_map accuracy ?fwd ?bwd ?origin ~total =
          let request =
            `List ( `String "INFLUENCE_MAP" :: (
                (fun accuracy l ->
                 match accuracy with
                 | None -> l
                 | Some a -> (Public_data.accuracy_to_json a)::l)
                  accuracy
                  [JsonUtil.of_option JsonUtil.of_int fwd;
                   JsonUtil.of_option JsonUtil.of_int bwd;
                   JsonUtil.of_int total;
                   JsonUtil.of_option Public_data.short_influence_node_to_json origin]
              ))
                 in
          Lwt_result.bind_result
            (self#message post request)
            (fun x -> Result.Ok x)
    method get_initial_node =
      let request =
        `List [`String "INFLUENCE_MAP_ORIGINAL_NODE"]
      in
      Lwt_result.bind_result
        (self#message post request)
        (fun x -> Result.Ok x)
    method get_next_node json =
      let request =
        `List [`String "INFLUENCE_MAP_NEXT_NODE";
               JsonUtil.of_option Public_data.short_influence_node_to_json json]
      in
      Lwt_result.bind_result
        (self#message post request)
            (fun x -> Result.Ok x)
    method get_previous_node json =
      let request =
        `List [`String "INFLUENCE_MAP_PREVIOUS_NODE";
               JsonUtil.of_option Public_data.short_influence_node_to_json json]
      in
      Lwt_result.bind_result
        (self#message post request)
            (fun x -> Result.Ok x)
    method get_dead_rules =
      let request = `List [ `String "DEAD_RULES" ] in
      Lwt_result.bind_result
        (self#message post request)
            (fun x -> Result.Ok x)
    method get_constraints_list =
      let request = `List [ `String "CONSTRAINTS" ] in
      Lwt_result.bind_result
        (self#message post request)
        (fun x -> Result.Ok x)
  end
