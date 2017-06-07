(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type mailbox = (Yojson.Basic.json, string) Result.result Lwt.u option ref

let reply_of_string x =
  match Yojson.Basic.from_string x with
  | `Assoc [ "code", `String "ERROR"; "data", err ] ->
    Result.Error (Yojson.Basic.to_string err)
  | `Assoc [ "code", `String "SUCCESS"; "data", data ] ->
    Result.Ok data
  | x ->
    Result.Error ("Invalid response from KaSa: "^Yojson.Basic.to_string x)

let receive mailbox x =
  match !mailbox with
  | None -> ()
  | Some t ->
    let out = reply_of_string x in
    let () = mailbox := None in
    Lwt.wakeup t out

let raw_message mailbox post request =
  match !mailbox with
  | None ->
    let result,feeder = Lwt.task () in
    let () = post request in
    let () = mailbox := Some feeder in
    result
  | Some _ ->
    Lwt.return_error "KaSa agent already busy"

let message mailbox post request =
  raw_message mailbox post (Yojson.Basic.to_string request)

let new_mailbox () = ref None

class new_client ~post mailbox : Api.manager_static_analysis =
  object(self)
    method init_static_analyser_raw compil =
      Lwt_result.bind_result
        (raw_message mailbox post ("[ \"INIT\", "^compil^"]"))
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
        (message mailbox post request)
        (fun x -> Result.Ok x)
    method get_influence_map accuracy =
      let request =
        `List ( `String "INFLUENCE_MAP" :: match accuracy with
          | None -> []
          | Some a -> [Public_data.accuracy_to_json a]) in
      Lwt_result.bind_result
        (message mailbox post request)
        (fun x -> Result.Ok x)
    method get_dead_rules =
      let request = `List [ `String "DEAD_RULES" ] in
      Lwt_result.bind_result
        (message mailbox post request)
        (fun x -> Result.Ok x)
    method get_constraints_list =
      let request = `List [ `String "CONSTRAINTS" ] in
      Lwt_result.bind_result
        (message mailbox post request)
        (fun x -> Result.Ok x)
  end
