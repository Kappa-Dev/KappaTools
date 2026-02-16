(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type item = { rank: int; content: string; working_set: bool }

type catalog = {
  elements: (string, item) Hashtbl.t;
  index: string option Mods.DynArray.t;
  ast: Ast.parsing_compil option ref;
}

type catalog_item = { position: int; id: string; working_set: bool }

let write_catalog_item ob { position; id; working_set } =
  let () = Buffer.add_char ob '{' in
  let () = JsonUtil.write_field "id" Yojson.Basic.write_string ob id in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "position" Yojson.Basic.write_int ob position in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "working_set" Yojson.Basic.write_bool ob working_set
  in
  Buffer.add_char ob '}'

let read_catalog_item p lb =
  let position, id, working_set, count =
    Yojson.Basic.read_fields
      (fun (pos, i, working_set, c) key p lb ->
        if key = "working_set" then
          pos, i, Yojson.Basic.read_bool p lb, succ c
        else if key = "position" then
          Yojson.Basic.read_int p lb, i, working_set, succ c
        else (
          let () = assert (key = "id") in
          pos, Yojson.Basic.read_string p lb, working_set, succ c
        ))
      (-1, "", false, 0) p lb
  in
  let () = assert (count = 3) in
  { position; id; working_set }

let create () =
  {
    elements = Hashtbl.create 1;
    index = Mods.DynArray.create 1 None;
    ast = ref None;
  }

let put ~position:rank ~id ~content ~working_set catalog =
  let () = Hashtbl.replace catalog.elements id { rank; content; working_set } in
  match Mods.DynArray.get catalog.index rank with
  | None ->
    let () = Mods.DynArray.set catalog.index rank (Some id) in
    let () = catalog.ast := None in
    Result.Ok ()
  | Some aie ->
    Result.Error
      ("Slot " ^ string_of_int rank ^ " is not available. There is already "
     ^ aie)

let file_create ~position ~id ~content ~working_set catalog =
  if Hashtbl.mem catalog.elements id then
    Result.Error
      ("A file called \"" ^ id ^ "\" is already present in the catalog")
  else
    put ~position ~id ~content ~working_set catalog

let file_move ~position ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Missing file \"" ^ id ^ "\" in the catalog")
  | _ :: _ :: _ -> Result.Error "File catalog has serious problems"
  | [ { rank; content; working_set } ] ->
    let () = Mods.DynArray.set catalog.index rank None in
    put ~position ~id ~content ~working_set catalog

let file_patch ~id ~working_set content catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Unknown file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> Result.Error "Serious problems in file catalog"
  | [ { rank; content = _; working_set = old_ws } ] ->
    let working_set =
      match working_set with
      | None -> old_ws
      | Some ws -> ws
    in
    let () =
      Hashtbl.replace catalog.elements id { rank; content; working_set }
    in
    let () = catalog.ast := None in
    Result.Ok ()

let file_set_working_set ~id working_set catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Unknown file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> Result.Error "Serious problems in file catalog"
  | [ { rank; content; _ } ] ->
    let () =
      Hashtbl.replace catalog.elements id { rank; content; working_set }
    in
    let () = catalog.ast := None in
    Result.Ok ()

(* let enable_or_disable_rule rule_id enable catalog = (*rTODO*)
   match !(catalog.ast) with
   | None -> Result.Error ("Compiled AST missing")
   | Some ast ->
   match Mods.IntMap.find_option rule_id ast.working_set_values with
               | None ->
                 Result.Error ("No rule with id "^ string_of_int rule_id ^" was found.")
               | Some old_bool ->
                 if old_bool = enable then
                    Result.Ok ()
                 else
                   let working_set_values =
                     Mods.IntMap.add rule_id enable ast.working_set_values
                   in
                   let () = catalog.ast := Some {ast with working_set_values} in
     Result.Ok () *)

let file_delete ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("No file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> failwith "Big troubles in file catalog"
  | [ { rank; _ } ] ->
    let () = Mods.DynArray.set catalog.index rank None in
    let () = Hashtbl.remove catalog.elements id in
    let () = catalog.ast := None in
    Result.Ok ()

let file_get ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("File \"" ^ id ^ "\" does not exist")
  | _ :: _ :: _ -> Result.Error "Corrupted file catalog"
  | [ { rank; content; working_set } ] -> Result.Ok (content, rank, working_set)

let catalog catalog =
  Mods.DynArray.fold_righti
    (fun position x acc ->
      match x with
      | None -> acc
      | Some id ->
        let file = Hashtbl.find catalog.elements id in
        { position; id; working_set = file.working_set } :: acc)
    catalog.index []

let parse yield catalog =
  match !(catalog.ast) with
  | Some compile -> Lwt.return (Result_util.ok compile)
  | None ->
    Mods.DynArray.fold_righti
      (fun _ x acc ->
        match x with
        | None -> acc
        | Some x ->
          let file = Hashtbl.find catalog.elements x in
          let lexbuf = Lexing.from_string file.content in
          let () =
            lexbuf.Lexing.lex_curr_p <-
              { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = x }
          in
          acc >>= fun (compile, err) ->
          let compile =
            { compile with Ast.filenames = x :: compile.Ast.filenames }
          in
          Lwt.catch
            (fun () ->
              Lwt.wrap1 Klexer4.model lexbuf >>= fun (insts, err') ->
              yield () >>= fun () ->
              Lwt.return
                ( Cst.append_to_ast_compil insts
                    ~all_rules_in_ws:file.working_set compile,
                  err' @ err ))
            (function
              | ExceptionDefn.Syntax_Error (message, range)
              | ExceptionDefn.Malformed_Decl (message, range)
              | ExceptionDefn.Internal_Error (message, range) ->
                Lwt.return (compile, (message, range) :: err)
              | Invalid_argument error ->
                Lwt.return
                  ( compile,
                    Loc.annot_with_dummy ("Runtime error " ^ error) :: err )
              | exn ->
                let message = Printexc.to_string exn in
                Lwt.return (compile, Loc.annot_with_dummy message :: err)))
      catalog.index
      (Lwt.return (Ast.empty_compil, []))
    >>= ( function
    | compile, [] ->
      let () = catalog.ast := Some compile in
      Lwt.return (Result_util.ok compile)
    | _, err ->
      let err =
        List.map
          (fun ((text, p) as x) ->
            let range =
              if Loc.is_annoted_with_dummy x then
                None
              else
                Some p
            in
            { Result_util.severity = Logs.Error; range; text })
          err
      in
      Lwt.return (Result_util.error err) )

let overwrite filename ~working_set ast catalog =
  let content = Format.asprintf "%a" Ast.print_parsing_compil_kappa ast in
  let it = { rank = 0; content; working_set } in
  let () = Hashtbl.reset catalog.elements in
  let () = Hashtbl.add catalog.elements filename it in
  let () =
    Mods.DynArray.iteri
      (fun i _ -> Mods.DynArray.set catalog.index i None)
      catalog.index
  in
  let () = Mods.DynArray.set catalog.index 0 (Some filename) in
  catalog.ast := Some ast
