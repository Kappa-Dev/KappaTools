(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type slot = { local: string option; name: string }
type active = { rank: int; cursor_pos: Locality.position; out_of_sync: bool }
type model = { current: active option; directory: slot Mods.IntMap.t }

let dummy_cursor_pos = { Locality.line = -1; Locality.chr = 0 }
let blank_state = { current = None; directory = Mods.IntMap.empty }
let model, set_directory_state = React.S.create blank_state

type refresh = { filename: string; content: string; line: int option }

let refresh_file, set_refresh_file = React.E.create ()

let current_filename =
  React.S.map
    (fun m ->
      Option_util.bind
        (fun x ->
          Option_util.map
            (fun { name; _ } -> name)
            (Mods.IntMap.find_option x.rank m.directory))
        m.current)
    model

let with_current_pos ?eq ?(on = React.S.const true) f default =
  React.S.fmap ?eq
    (fun m ->
      Option_util.bind
        (fun x ->
          Option_util.bind
            (fun { name; _ } -> f name x.cursor_pos)
            (Mods.IntMap.find_option x.rank m.directory))
        m.current)
    default
    (React.S.on on blank_state model)

let with_current_file f =
  let state = React.S.value model in
  match state.current with
  | None ->
    let error_msg : string = "Attempt to fetch file with none selected." in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some active ->
    (match Mods.IntMap.find_option active.rank state.directory with
    | None ->
      let error_msg : string =
        "Internal inconsistentcy: No file at selected rank."
      in
      Lwt.return (Api_common.result_error_msg error_msg)
    | Some x -> f state active x)

let get_file () : (string * string) Api.result Lwt.t =
  with_current_file (fun _state active -> function
    | { local = None; name } ->
      State_project.with_project ~label:"get_file" (fun manager ->
          manager#file_get name
          >>= Api_common.result_bind_lwt ~ok:(fun (text, rank') ->
                  if active.rank = rank' then
                    Lwt.return (Result_util.ok (text, name))
                  else (
                    let error_msg = "Inconsistency in rank while get_file." in
                    Lwt.return (Api_common.result_error_msg error_msg)
                  )))
    | { local = Some text; name } -> Lwt.return (Result_util.ok (text, name)))

let send_refresh (line : int option) : unit Api.result Lwt.t =
  (* only send refresh if there is a current file *)
  match (React.S.value model).current with
  | None -> Lwt.return (Result_util.ok ())
  | Some { out_of_sync; _ } ->
    if out_of_sync then
      Lwt.return
        (Api_common.result_error_msg
           "File was not in sync. Switching may lead to data lost.")
    else
      get_file ()
      >>= Api_common.result_bind_lwt ~ok:(fun (content, filename) ->
              let () = Common.debug content in
              let () = set_refresh_file { filename; content; line } in
              Lwt.return (Result_util.ok ()))

let update_directory ~reset current catalog =
  let state = React.S.value model in
  let directory =
    List.fold_left
      (fun acc { Kfiles.position; id } ->
        Mods.IntMap.add position { name = id; local = None } acc)
      (if reset then
         Mods.IntMap.empty
       else
         state.directory)
      catalog
  in
  set_directory_state { current; directory }

let create_file ~(filename : string) ~(content : string) : unit Api.result Lwt.t
    =
  State_project.with_project ~label:"create_file" (fun manager ->
      manager#file_catalog
      >>= Api_common.result_bind_lwt ~ok:(fun catalog ->
              let matching_file =
                List.filter
                  (fun file_metadata -> filename = file_metadata.Kfiles.id)
                  catalog
              in
              (match matching_file with
              | [] ->
                let max_pos =
                  List.fold_left
                    (fun acc { Kfiles.position; _ } -> max acc position)
                    0 catalog
                in
                manager#file_create (succ max_pos) filename content
                >>= Api_common.result_bind_lwt ~ok:(fun () ->
                        manager#file_catalog
                        >>= Api_common.result_bind_lwt ~ok:(fun catalog' ->
                                Lwt.return
                                  (Result_util.ok (catalog', succ max_pos))))
              | metadata :: _ ->
                manager#file_update filename content
                >>= Api_common.result_bind_lwt ~ok:(fun () ->
                        Lwt.return
                          (Result_util.ok (catalog, metadata.Kfiles.position))))
              >>= Api_common.result_bind_lwt ~ok:(fun (catalog, current) ->
                      let () =
                        update_directory ~reset:false
                          (Some
                             {
                               rank = current;
                               cursor_pos = dummy_cursor_pos;
                               out_of_sync = false;
                             })
                          catalog
                      in
                      send_refresh None)))

let rec choose_file choice = function
  | [] ->
    let error_msg : string =
      Format.sprintf "Failed to switch file %s." choice
    in
    Api_common.result_error_msg error_msg
  | { Kfiles.id; position } :: t ->
    if choice = id then
      Result_util.ok position
    else
      choose_file choice t

let select_file (filename : string) (line : int option) : unit Api.result Lwt.t
    =
  State_project.with_project ~label:"select_file" (fun manager ->
      manager#file_catalog
      >>= Api_common.result_bind_lwt ~ok:(fun catalog ->
              Api_common.result_bind_lwt
                ~ok:(fun rank ->
                  let () =
                    update_directory ~reset:false
                      (Some
                         {
                           rank;
                           cursor_pos = dummy_cursor_pos;
                           out_of_sync = false;
                         })
                      catalog
                  in
                  send_refresh line)
                (choose_file filename catalog)))

let set_content (content : string) : unit Api.result Lwt.t =
  with_current_file (fun state active -> function
    | { local = Some _; name } ->
      let directory =
        Mods.IntMap.add active.rank
          { local = Some content; name }
          state.directory
      in
      let () = set_directory_state { current = state.current; directory } in
      Lwt.return (Result_util.ok ())
    | { local = None; name } ->
      let () =
        set_directory_state
          {
            current =
              Some
                {
                  rank = active.rank;
                  cursor_pos = active.cursor_pos;
                  out_of_sync = false;
                };
            directory = state.directory;
          }
      in
      State_project.with_project ~label:"set_content" (fun manager ->
          manager#file_update name content))

let set_compile file_id (compile : bool) : unit Api.result Lwt.t =
  let state = React.S.value model in
  match
    Mods.IntMap.filter_one (fun _ { name; _ } -> name = file_id) state.directory
  with
  | None ->
    let error_msg = "Internal inconsistency: No file " ^ file_id in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some (rank, { local = Some content; name }) ->
    if compile then (
      let directory =
        Mods.IntMap.add rank { local = None; name } state.directory
      in
      let () = set_directory_state { current = state.current; directory } in
      State_project.with_project ~label:"set_compile" (fun manager ->
          manager#file_create rank name content)
    ) else
      Lwt.return (Result_util.ok ())
  | Some (rank, { local = None; name }) ->
    if compile then
      Lwt.return (Result_util.ok ())
    else
      State_project.with_project ~label:"set_compile" (fun manager ->
          manager#file_get name
          >>= Api_common.result_bind_lwt ~ok:(fun (content, rank') ->
                  if rank = rank' then (
                    let directory =
                      Mods.IntMap.add rank
                        { local = Some content; name }
                        state.directory
                    in
                    let () =
                      set_directory_state { current = state.current; directory }
                    in
                    State_project.with_project ~label:"set_compile'"
                      (fun manager -> manager#file_delete name)
                  ) else (
                    let error_msg =
                      "Inconsistency in rank while set_compile."
                    in
                    Lwt.return (Api_common.result_error_msg error_msg)
                  )))

let remove_file () : unit Api.result Lwt.t =
  with_current_file (fun state active { local; name } ->
      let directory = Mods.IntMap.remove active.rank state.directory in
      let current =
        Option_util.map
          (fun (rank, _) ->
            { rank; cursor_pos = dummy_cursor_pos; out_of_sync = false })
          (Mods.IntMap.root directory)
      in
      let () = set_directory_state { current; directory } in
      let x = send_refresh None in
      match local with
      | Some _ -> x
      | None ->
        State_project.with_project ~label:"remove_file" (fun manager ->
            manager#file_delete name >>= fun y ->
            x >>= fun x -> Lwt.return (Api_common.result_combine [ x; y ])))

let do_a_move state file_id rank =
  match
    Mods.IntMap.filter_one (fun _ { name; _ } -> name = file_id) state.directory
  with
  | None ->
    let error_msg = "Internal inconsistency: No file " ^ file_id in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some (rank', ({ local; _ } as x)) ->
    let directory =
      Mods.IntMap.add rank x (Mods.IntMap.remove rank' state.directory)
    in
    let current =
      match state.current with
      | Some { rank = pos; cursor_pos; out_of_sync } when pos = rank' ->
        Some { rank; cursor_pos; out_of_sync }
      | x -> x
    in
    if local = None then
      State_project.with_project ~label:"remove_file" (fun manager ->
          manager#file_move rank file_id
          >>= Api_common.result_bind_lwt ~ok:(fun () ->
                  Lwt.return (Result_util.ok { current; directory })))
    else
      Lwt.return (Result_util.ok { current; directory })

let rec set_position state file_id rank =
  match Mods.IntMap.find_option rank state.directory with
  | Some { name; _ } ->
    if file_id = name then
      Lwt.return (Result_util.ok state)
    else
      set_position state name (succ rank)
      >>= Api_common.result_bind_lwt ~ok:(fun state' ->
              do_a_move state' file_id rank)
  | None -> do_a_move state file_id rank

let order_files (filenames : string list) : unit Api.result Lwt.t =
  let rec _order_file filenames state index : unit Api.result Lwt.t =
    match filenames with
    | [] ->
      let () = set_directory_state state in
      Lwt.return (Result_util.ok ())
    | file_id :: tail ->
      set_position state file_id index
      >>= Api_common.result_bind_lwt ~ok:(fun state' ->
              _order_file tail state' (index + 1))
  in
  _order_file filenames (React.S.value model) 0

let cursor_activity ~line ~ch =
  let v = React.S.value model in
  match v.current with
  | None -> ()
  | Some { rank; out_of_sync; _ } ->
    set_directory_state
      {
        current =
          Some
            {
              rank;
              cursor_pos = { Locality.line = succ line; chr = ch };
              out_of_sync;
            };
        directory = v.directory;
      }

let out_of_sync out_of_sync =
  let v = React.S.value model in
  match v.current with
  | None -> ()
  | Some { rank; cursor_pos; _ } ->
    set_directory_state
      {
        current = Some { rank; cursor_pos; out_of_sync };
        directory = v.directory;
      }

let sync ?(reset = false) () : unit Api.result Lwt.t =
  State_project.with_project ~label:"select_file" (fun manager ->
      manager#file_catalog
      >>= Api_common.result_bind_lwt ~ok:(fun catalog ->
              let cand = (React.S.value model).current in
              let pos =
                if
                  reset
                  ||
                  match cand with
                  | None -> true
                  | Some x ->
                    List.exists
                      (fun { Kfiles.position; _ } -> x.rank = position)
                      catalog
                then (
                  match catalog with
                  | [] -> None
                  | { Kfiles.position; _ } :: _ ->
                    Some
                      {
                        rank = position;
                        cursor_pos = dummy_cursor_pos;
                        out_of_sync = false;
                      }
                ) else
                  cand
              in
              let () = update_directory ~reset pos catalog in
              send_refresh None))

let load_default () : unit Lwt.t =
  create_file ~filename:"model.ka" ~content:""
  >>= Result_util.fold
        ~ok:(fun () -> Lwt.return_unit)
        ~error:(fun errors ->
          let msg =
            Format.asprintf
              "State_file.load_default : creating default file error@ @[<v>%a@]"
              (Pp.list Pp.space Result_util.print_message)
              errors
          in
          let () = Common.debug (Js.string msg) in
          Lwt.return_unit)

let load_models () : unit Lwt.t =
  let models = Common_state.url_args "model" in
  let rec add_models models load_file : unit Lwt.t =
    match models with
    | [] -> Lwt.return_unit
    | model :: models ->
      (* fetch model *)
      Js_of_ocaml_lwt.XmlHttpRequest.get model
      >>= (fun content ->
            if content.Js_of_ocaml_lwt.XmlHttpRequest.code <> 200 then
              Lwt.return
                (Api_common.result_error_msg
                   (Format.sprintf "bad response code %d fetching url %s"
                      content.Js_of_ocaml_lwt.XmlHttpRequest.code model))
            else (
              match
                Url.url_of_string content.Js_of_ocaml_lwt.XmlHttpRequest.url
              with
              | None ->
                Lwt.return
                  (Api_common.result_error_msg
                     (Format.sprintf "failed to retrieve url %s" model))
              | Some u ->
                let filename =
                  List_util.last
                    (match u with
                    | Url.Http h | Url.Https h -> h.Url.hu_path
                    | Url.File f -> f.Url.fu_path)
                in
                let filecontent : string =
                  content.Js_of_ocaml_lwt.XmlHttpRequest.content
                in
                Lwt.return (Result_util.ok (filename, filecontent))
            ))
      >>= (* add content *)
      Api_common.result_bind_lwt ~ok:(fun (filename, content) ->
          create_file ~filename ~content
          >>= Api_common.result_bind_lwt ~ok:(fun () ->
                  Lwt.return (Result_util.ok filename)))
      >>= (* select model if needed *)
      Result_util.fold
        ~ok:(fun filename ->
          if load_file then
            select_file filename None
            >>= Result_util.fold
                  ~ok:(fun _ -> add_models models false)
                  ~error:(fun _ -> add_models models load_file)
          else
            add_models models load_file)
        ~error:(fun errors ->
          let msg =
            Format.asprintf "creating loading url %s error@ @[<v>%a@]" model
              (Pp.list Pp.space Result_util.print_message)
              errors
          in
          let () =
            Common.debug
              (Js.string (Format.sprintf "State_file.load_model %s" msg))
          in
          add_models models load_file)
  in
  match models with
  | [] -> load_default ()
  | _ :: _ -> add_models models true

let init () : unit Lwt.t = Lwt.return_unit >>= load_models
