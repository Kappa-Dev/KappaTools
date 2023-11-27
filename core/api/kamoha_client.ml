(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type _ handle =
  | Nothing : unit handle
  | Catalog : Kfiles.catalog_item list handle
  | Info : (string * int) handle
  | Ast : Ast.parsing_compil handle

type box =
  | B : 'a handle * ('a, Result_util.message list) Result_util.t Lwt.u -> box

type mailbox = (int, box) Hashtbl.t

let new_mailbox () = Hashtbl.create 2

let read_result f p lb =
  JsonUtil.read_next_item
    (Result_util.read_t f (Yojson.Basic.read_list Result_util.read_message))
    p lb

let receive mailbox x =
  JsonUtil.read_of_string
    (JsonUtil.read_variant Yojson.Basic.read_int (fun p lb id ->
         let () =
           match Hashtbl.find mailbox id with
           | B (Nothing, thread) ->
             Lwt.wakeup thread (read_result Yojson.Basic.read_null p lb)
           | B (Catalog, thread) ->
             Lwt.wakeup thread
               (read_result
                  (Yojson.Basic.read_list Kfiles.read_catalog_item)
                  p lb)
           | B (Info, thread) ->
             Lwt.wakeup thread
               (read_result
                  (JsonUtil.read_compact_pair Yojson.Basic.read_string
                     Yojson.Basic.read_int)
                  p lb)
           | B (Ast, thread) ->
             Lwt.wakeup thread (read_result Ast.read_parsing_compil p lb)
         in
         Hashtbl.remove mailbox id))
    x

let is_computing mailbox = Hashtbl.length mailbox <> 0

class virtual new_client ~post mailbox : Api.manager_model =
  object (self)
    val mutable id = 0
    method virtual is_running : bool

    method private message : type a.
        a handle ->
        (Buffer.t -> unit) ->
        (a, Result_util.message list) Result_util.t Lwt.t =
      fun handle request ->
        if self#is_running then (
          let result, feeder = Lwt.task () in
          let message =
            JsonUtil.string_of_write
              (fun b () ->
                JsonUtil.write_sequence b
                  [ (fun b -> Yojson.Basic.write_int b id); request ])
              ()
          in
          let () = post message in
          let () = Hashtbl.replace mailbox id (B (handle, feeder)) in
          let () = id <- succ id in
          result
        ) else
          Lwt.return
            (Result_util.error
               [
                 {
                   Result_util.severity = Logs.Error;
                   Result_util.range = None;
                   Result_util.text = "kamoha agent has died";
                 };
               ])

    method file_delete file_id =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileDelete");
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_update file_id file_content =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileUpdate");
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Yojson.Basic.write_string b file_content);
            ])

    method file_move file_position file_id =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileMove");
              (fun b -> Yojson.Basic.write_int b file_position);
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_get file_id =
      self#message Info (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileGet");
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_create file_position file_id file_content =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileCreate");
              (fun b -> Yojson.Basic.write_int b file_position);
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Yojson.Basic.write_string b file_content);
            ])

    method file_catalog =
      self#message Catalog (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "FileCatalog") ])

    method secret_project_parse =
      self#message Ast (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "ProjectParse") ])

    method project_overwrite file_id ast =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "ProjectOverwrite");
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Ast.write_parsing_compil b ast);
            ])
  end
