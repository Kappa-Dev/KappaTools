(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let catalog = Kfiles.create ()

let reply post write_v id v =
  let message =
    JsonUtil.string_of_write
      (fun b () ->
         JsonUtil.write_sequence b [
           (fun b -> Yojson.Basic.write_int b id);
           (fun b -> Result_util.write_t
               write_v (JsonUtil.write_list Result_util.write_message) b v);
         ]) () in
  post message

let write_catalog_items = JsonUtil.write_list Kfiles.write_catalog_item

let lift_answer = function
  | Result.Ok x -> Result_util.ok x
  | Result.Error text ->
    Result_util.error [{Result_util.range = None; severity = Logs.Error; text}]

let on_message yield post text =
  try
    JsonUtil.read_of_string
      (JsonUtil.read_variant Yojson.Basic.read_int
         (fun st b msg_id ->
            try
              JsonUtil.read_next_item
                (JsonUtil.read_variant Yojson.Basic.read_string
                   (fun st b -> function
                      | "FileCatalog" ->
                        let out = Kfiles.catalog catalog in
                        reply post write_catalog_items msg_id (Result_util.ok out)
                      | "FileCreate" ->
                        let position =
                          JsonUtil.read_next_item Yojson.Basic.read_int st b in
                        let id =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let content =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let out =
                          Kfiles.file_create ~position ~id ~content catalog in
                        reply post Yojson.Basic.write_null msg_id (lift_answer out)
                      | "FileGet" ->
                        let id =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let out = Kfiles.file_get ~id catalog in
                        reply
                          post
                          (JsonUtil.write_compact_pair
                             Yojson.Basic.write_string Yojson.Basic.write_int)
                          msg_id (lift_answer out)
                      | "FileMove" ->
                        let position =
                          JsonUtil.read_next_item Yojson.Basic.read_int st b in
                        let id =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let out = Kfiles.file_move ~position ~id catalog in
                        reply post Yojson.Basic.write_null msg_id (lift_answer out)
                      | "FileUpdate" ->
                        let id =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let content =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let out = Kfiles.file_patch ~id content catalog in
                        reply post Yojson.Basic.write_null msg_id (lift_answer out)
                      | "FileDelete" ->
                        let id =
                          JsonUtil.read_next_item Yojson.Basic.read_string st b in
                        let () = Kfiles.file_delete ~id catalog in
                        reply
                          post Yojson.Basic.write_null msg_id (Result_util.ok ())
                      | "ProjectParse" ->
                        Lwt.bind
                          (Kfiles.parse yield catalog)
                          (reply post Ast.write_parsing_compil msg_id)
                      | x ->
                        reply
                          post Yojson.Basic.write_null msg_id
                          (Result_util.error [{
                               Result_util.severity = Logs.Error;
                               range = None;
                               text = ("Invalid directive: "^x);
                             }])))
                st b
            with e ->
              reply
                post Yojson.Basic.write_null msg_id
                (Result_util.error [{Result_util.severity = Logs.Error;
                                     range = None;
                                     text = ("Exception raised: "^
                                             Printexc.to_string e);}])))
      text
  with Yojson.Json_error x ->
    post
      (Yojson.to_string
         (`String (x^"\nMessage format must be [ id, [\"Request\", ... ] ]")))
     | e ->
       post
         (Yojson.to_string
            (`String ("unexpected exception: "^
                      Printexc.to_string e^
                      "\nMessage format must be [ id, [\"Request\", ... ] ]")))
