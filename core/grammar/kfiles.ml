(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type ast_parsing_compil_option =
  | Current of Ast.parsing_compil
  | Patched
  | Empty

type item = { rank: int; content: string }

type catalog = {
  elements: (string, item) Hashtbl.t;
  index: string option Mods.DynArray.t;
  ast: ast_parsing_compil_option ref;
  current_ws: int option ref;
}

type catalog_item = { position: int; id: string }

let write_catalog_item ob { position; id } =
  let () = Buffer.add_char ob '{' in
  let () = JsonUtil.write_field "id" Yojson.Basic.write_string ob id in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "position" Yojson.Basic.write_int ob position in
  Buffer.add_char ob '}'

let read_catalog_item p lb =
  let position, id, count =
    Yojson.Basic.read_fields
      (fun (pos, i, c) key p lb ->
        if key = "position" then
          Yojson.Basic.read_int p lb, i, succ c
        else (
          let () = assert (key = "id") in
          pos, Yojson.Basic.read_string p lb, succ c
        ))
      (-1, "", 0) p lb
  in
  let () = assert (count = 2) in
  { position; id }

let create () =
  {
    elements = Hashtbl.create 1;
    index = Mods.DynArray.create 1 None;
    ast = ref Empty;
    current_ws = ref None;
  }

let put ~position:rank ~id ~content catalog =
  let () = Hashtbl.replace catalog.elements id { rank; content } in
  match Mods.DynArray.get catalog.index rank with
  | None ->
    let () = Mods.DynArray.set catalog.index rank (Some id) in
    let () = catalog.ast := Empty in
    Result.Ok ()
  | Some aie ->
    Result.Error
      ("Slot " ^ string_of_int rank ^ " is not available. There is already "
     ^ aie)

let file_create ~position ~id ~content catalog =
  if Hashtbl.mem catalog.elements id then
    Result.Error
      ("A file called \"" ^ id ^ "\" is already present in the catalog")
  else
    put ~position ~id ~content catalog

let file_move ~position ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Missing file \"" ^ id ^ "\" in the catalog")
  | _ :: _ :: _ -> Result.Error "File catalog has serious problems"
  | [ { rank; content } ] ->
    let () = Mods.DynArray.set catalog.index rank None in
    put ~position ~id ~content catalog

let file_patch ~id content catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Unknown file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> Result.Error "Serious problems in file catalog"
  | [ { rank; _ } ] ->
    let () = Hashtbl.replace catalog.elements id { rank; content } in
    let () =
      catalog.ast :=
        match !(catalog.ast) with
        | Current _ | Patched -> Patched
        | Empty -> Empty
    in
    Result.Ok ()

let file_set_working_set ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("Unknown file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> Result.Error "Serious problems in file catalog"
  | [ { rank; _ } ] ->
    let () = catalog.current_ws := Some rank in
    let () = catalog.ast := Empty in
    Result.Ok ()

let file_delete ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("No file \"" ^ id ^ "\"")
  | _ :: _ :: _ -> failwith "Big troubles in file catalog"
  | [ { rank; _ } ] ->
    let () = Mods.DynArray.set catalog.index rank None in
    let () = Hashtbl.remove catalog.elements id in
    let () = catalog.ast := Empty in
    Result.Ok ()

let file_get ~id catalog =
  match Hashtbl.find_all catalog.elements id with
  | [] -> Result.Error ("File \"" ^ id ^ "\" does not exist")
  | _ :: _ :: _ -> Result.Error "Corrupted file catalog"
  | [ { rank; content } ] -> Result.Ok (content, rank)

let catalog catalog =
  Mods.DynArray.fold_righti
    (fun position x acc ->
      match x with
      | None -> acc
      | Some id -> { position; id } :: acc)
    catalog.index []

let parse yield catalog ~force =
  let parse_one_file x filenames compile all_rules_in_ws err =
    let file = Hashtbl.find catalog.elements x in
    let lexbuf = Lexing.from_string file.content in
    let () =
      lexbuf.Lexing.lex_curr_p <-
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = x }
    in
    let compile = { compile with Ast.filenames = x :: filenames } in
    Lwt.catch
      (fun () ->
        Lwt.wrap1 Klexer4.model lexbuf >>= fun (insts, err') ->
        yield () >>= fun () ->
        Lwt.return
          (Cst.append_to_ast_compil insts ~all_rules_in_ws compile, err' @ err))
      (function
        | ExceptionDefn.Syntax_Error (message, range)
        | ExceptionDefn.Malformed_Decl (message, range)
        | ExceptionDefn.Internal_Error (message, range) ->
          Lwt.return (compile, (message, range) :: err)
        | Invalid_argument error ->
          Lwt.return
            (compile, Loc.annot_with_dummy ("Runtime error " ^ error) :: err)
        | exn ->
          let message = Printexc.to_string exn in
          Lwt.return (compile, Loc.annot_with_dummy message :: err))
  in
  let handle_error err =
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
    Lwt.return (Result_util.error err)
  in
  let parse_all_files () =
    Mods.DynArray.fold_righti
      (fun position x acc ->
        match x with
        | None -> acc
        | Some x ->
          acc >>= fun (compile, err) ->
          parse_one_file x compile.Ast.filenames compile
            (!(catalog.current_ws) = Some position)
            err)
      catalog.index
      (Lwt.return (Ast.empty_compil, []))
    >>= function
    | compile, [] ->
      let () = catalog.ast := Current compile in
      Lwt.return (Result_util.ok (compile, None))
    | _, err -> handle_error err
  in
  if force then 
    let () = catalog.ast := Empty in
    parse_all_files () 
  else
    match !(catalog.ast), !(catalog.current_ws) with
    | Current compile, _ -> Lwt.return (Result_util.ok (compile, None))
    | Patched, Some current ->
      (match Mods.DynArray.get catalog.index current with
      | None ->
        let () = catalog.ast := Empty in
        parse_all_files ()
      | Some x ->
        parse_one_file x [] Ast.empty_compil true [] >>= ( function
        | compile, [] -> Lwt.return (Result_util.ok (compile, Some x))
        | _, err -> handle_error err ))
    | Empty, _ | _, None -> parse_all_files ()

let overwrite filename ast catalog =
  let content = Format.asprintf "%a" Ast.print_parsing_compil_kappa ast in
  let it = { rank = 0; content } in
  let () = Hashtbl.reset catalog.elements in
  let () = Hashtbl.add catalog.elements filename it in
  let () =
    Mods.DynArray.iteri
      (fun i _ -> Mods.DynArray.set catalog.index i None)
      catalog.index
  in
  let () = Mods.DynArray.set catalog.index 0 (Some filename) in
  catalog.ast := Current ast

let update_ast ast catalog = catalog.ast := Current ast
