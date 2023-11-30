(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let catalog = Kfiles.create ()

type _ handle =
  | Nothing : unit handle
  | Catalog : Kfiles.catalog_item list handle
  | Info : (string * int) handle
  | Ast : Ast.parsing_compil handle

type box =
  | B : 'a handle * int * ('a, Result_util.message list) Result_util.t -> box

let reply post write_v id v =
  let message =
    JsonUtil.string_of_write
      (fun b () ->
        JsonUtil.write_sequence b
          [
            (fun b -> Yojson.Basic.write_int b id);
            (fun b ->
              Result_util.write_t write_v
                (JsonUtil.write_list Result_util.write_message)
                b v);
          ])
      ()
  in
  post message

let write_catalog_items = JsonUtil.write_list Kfiles.write_catalog_item

let lift_answer = function
  | Result.Ok x -> Result_util.ok x
  | Result.Error text ->
    Result_util.error
      [ { Result_util.range = None; severity = Logs.Error; text } ]

let on_message yield post =
  let current_id = ref None in
  fun text ->
    try
      Lwt.bind
        (JsonUtil.read_of_string
           (JsonUtil.read_variant Yojson.Basic.read_int (fun st b msg_id ->
                let () = current_id := Some msg_id in
                JsonUtil.read_next_item
                  (JsonUtil.read_variant Yojson.Basic.read_string (fun st b ->
                       function
                     | "FileCatalog" ->
                       let out = Kfiles.catalog catalog in
                       Lwt.return (B (Catalog, msg_id, Result_util.ok out))
                     | "FileCreate" ->
                       let position =
                         JsonUtil.read_next_item Yojson.Basic.read_int st b
                       in
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let out =
                         Kfiles.file_create ~position ~id ~content catalog
                       in
                       Lwt.return (B (Nothing, msg_id, lift_answer out))
                     | "FileGet" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let out = Kfiles.file_get ~id catalog in
                       Lwt.return (B (Info, msg_id, lift_answer out))
                     | "FileMove" ->
                       let position =
                         JsonUtil.read_next_item Yojson.Basic.read_int st b
                       in
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let out = Kfiles.file_move ~position ~id catalog in
                       Lwt.return (B (Nothing, msg_id, lift_answer out))
                     | "FileUpdate" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let out = Kfiles.file_patch ~id content catalog in
                       Lwt.return (B (Nothing, msg_id, lift_answer out))
                     | "FileDelete" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let out = Kfiles.file_delete ~id catalog in
                       Lwt.return (B (Nothing, msg_id, lift_answer out))
                     | "ProjectParse" ->
                       Lwt.bind (Kfiles.parse yield catalog) (fun out ->
                           Lwt.return (B (Ast, msg_id, out)))
                     | "ProjectOverwrite" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Ast.read_parsing_compil st b
                       in
                       let () = Kfiles.overwrite id content catalog in
                       Lwt.return (B (Nothing, msg_id, Result_util.ok ()))
                     | x ->
                       Lwt.return
                         (B
                            ( Nothing,
                              msg_id,
                              Result_util.error
                                [
                                  {
                                    Result_util.severity = Logs.Error;
                                    range = None;
                                    text = "Invalid directive: " ^ x;
                                  };
                                ] ))))
                  st b))
           text)
        (fun answer ->
          let () = current_id := None in
          match answer with
          | B (Catalog, msg_id, x) -> reply post write_catalog_items msg_id x
          | B (Nothing, msg_id, x) ->
            reply post Yojson.Basic.write_null msg_id x
          | B (Ast, msg_id, x) -> reply post Ast.write_parsing_compil msg_id x
          | B (Info, msg_id, x) ->
            reply post
              (JsonUtil.write_compact_pair Yojson.Basic.write_string
                 Yojson.Basic.write_int)
              msg_id x)
    with e ->
      (match !current_id with
      | Some msg_id ->
        reply post Yojson.Basic.write_null msg_id
          (Result_util.error
             [
               {
                 Result_util.severity = Logs.Error;
                 range = None;
                 text = "Exception raised: " ^ Printexc.to_string e;
               };
             ])
      | None ->
        (match e with
        | Yojson.Json_error x ->
          post
            (Yojson.to_string
               (`String
                 (x ^ "\nMessage format must be [ id, [\"Request\", ... ] ]")))
        | e ->
          post
            (Yojson.to_string
               (`String
                 ("unexpected exception: " ^ Printexc.to_string e
                ^ "\nMessage format must be [ id, [\"Request\", ... ] ]")))))
