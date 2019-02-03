(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type slot = { local : string option ; id : string; }

type model = { current : int option ; directory : slot Mods.IntMap.t }

let blank_state =
  { current = None ; directory = Mods.IntMap.empty }
let model, set_directory_state = React.S.create blank_state

type refresh = { filename : string ; content : string ; line : int option ; }
let refresh_file , set_refresh_file = React.E.create ()

let current_filename =
  React.S.map
    (fun m -> Option_util.bind
        (fun x -> Option_util.map
            (fun { id; _ } -> id) (Mods.IntMap.find_option x m.directory))
        m.current) model

let with_current_file f =
  let state = React.S.value model in
  match state.current with
  | None ->
    let error_msg : string =
      "Attempt to fetch file with none selected."
    in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some rank ->
    match Mods.IntMap.find_option rank state.directory with
    | None ->
      let error_msg : string =
        "Internal inconsistentcy: No file at selected rank." in
      Lwt.return (Api_common.result_error_msg error_msg)
    | Some x -> f state rank x

let get_file () : (string*string) Api.result Lwt.t =
  with_current_file (fun _state rank -> function
      | { local = None; id } ->
        State_project.with_project ~label:"get_file"
          (fun manager ->
             manager#file_get id >>=
             Api_common.result_bind_lwt
               ~ok:(fun (text,rank') ->
                   if rank = rank' then
                     Lwt.return (Result_util.ok (text,id))
                   else
                     let error_msg = "Inconsistency in rank while get_file." in
                     Lwt.return (Api_common.result_error_msg error_msg)))
      | { local = Some text; id } ->
        Lwt.return (Result_util.ok (text,id)))

let send_refresh
    (line : int option) : unit Api.result Lwt.t =
  (* only send refresh if there is a current file *)
  match (React.S.value model).current with
  | None -> Lwt.return (Result_util.ok ())
  | Some _ ->
    get_file () >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (content,filename) ->
           let () = Common.debug content in
           let () = set_refresh_file { filename; content; line } in
           Lwt.return (Result_util.ok ()))
    )

let update_directory ~reset current catalog =
  let state = React.S.value model in
  let directory =
    List.fold_left (fun acc { Kfiles.position; id } ->
        Mods.IntMap.add position { id; local = None } acc)
      (if reset then Mods.IntMap.empty else state.directory) catalog in
  set_directory_state { current; directory }

let create_file
    ~(filename:string)
    ~(content:string) :
  unit Api.result Lwt.t =
  State_project.with_project ~label:"create_file"
    (fun manager ->
       manager#file_catalog >>=
       Api_common.result_bind_lwt
         ~ok:(fun catalog ->
             let matching_file =
               List.filter
                 (fun file_metadata -> filename = file_metadata.Kfiles.id)
                 catalog in
             (match matching_file with
              | [] ->
                let max_pos =
                  List.fold_left
                    (fun acc { Kfiles.position; _ } -> max acc position)
                    0 catalog in
                manager#file_create (succ max_pos) filename content >>=
                Api_common.result_bind_lwt
                  ~ok:(fun () ->
                      manager#file_catalog >>=
                      Api_common.result_bind_lwt
                        ~ok:(fun catalog' ->
                            Lwt.return
                              (Result_util.ok (catalog',succ max_pos))))
              | metadata::_ ->
                manager#file_update filename content >>=
                Api_common.result_bind_lwt
                  ~ok:(fun () ->
                      Lwt.return
                        (Result_util.ok (catalog,metadata.Kfiles.position))))
             >>= Api_common.result_bind_lwt
                 ~ok:(fun (catalog,current) ->
                   let () =
                     update_directory ~reset:false (Some current) catalog in
                   send_refresh None)
           ))

let rec choose_file choice = function
  | [] ->
    let error_msg : string =
      Format.sprintf "Failed to switch file %s." choice in
    Api_common.result_error_msg error_msg
  | { Kfiles.id; position } :: t ->
    if choice = id then Result_util.ok position else choose_file choice t

let select_file (filename : string) (line : int option) : unit Api.result Lwt.t =
  State_project.with_project ~label:"select_file"
    (fun manager ->
       manager#file_catalog >>=
       Api_common.result_bind_lwt
         ~ok:(fun catalog ->
             Api_common.result_bind_lwt
               ~ok:(fun pos ->
                   let () =
                     update_directory ~reset:false (Some pos) catalog in
                   send_refresh line)
               (choose_file filename catalog)))

let set_content (content : string) : unit Api.result Lwt.t =
  with_current_file (fun state rank -> function
      | { local = Some _; id } ->
        let directory =
          Mods.IntMap.add rank { local = Some content; id } state.directory in
        let () = set_directory_state { current = state.current; directory } in
        Lwt.return (Result_util.ok ())
      | { local = None; id } ->
        State_project.with_project ~label:"set_content"
          (fun manager -> manager#file_update id content))

let set_compile file_id (compile : bool) : unit Api.result Lwt.t =
  let state  = React.S.value model in
  match Mods.IntMap.filter_one
          (fun _ { id; _ } -> id = file_id) state.directory with
  | None ->
    let error_msg = "Internal inconsistency: No file "^file_id in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some (rank, { local = Some content; id }) ->
    if compile then
      let directory =
        Mods.IntMap.add rank { local = None; id } state.directory in
      let () = set_directory_state { current = state.current; directory } in
      State_project.with_project ~label:"set_compile"
        (fun manager -> manager#file_create rank id content)
    else Lwt.return (Result_util.ok ())
  | Some (rank, { local = None; id }) ->
    if compile then Lwt.return (Result_util.ok ())
    else
      State_project.with_project ~label:"set_compile"
        (fun manager -> manager#file_get id >>=
          Api_common.result_bind_lwt
            ~ok:(fun (content,rank') ->
                if rank = rank' then
                  let directory = Mods.IntMap.add
                      rank { local = Some content; id } state.directory in
                  let () = set_directory_state
                      { current = state.current; directory } in
                  State_project.with_project ~label:"set_compile'"
                    (fun manager -> manager#file_delete id)
                else
                  let error_msg = "Inconsistency in rank while set_compile." in
                  Lwt.return (Api_common.result_error_msg error_msg)))

let remove_file () : unit Api.result Lwt.t =
  with_current_file (fun state rank { local; id } ->
      let directory = Mods.IntMap.remove rank state.directory in
      let current = Option_util.map fst (Mods.IntMap.root directory) in
      let () = set_directory_state {current; directory} in
      let x = send_refresh None in
      match local with
      | Some _ -> x
      | None ->
        State_project.with_project ~label:"remove_file"
          (fun manager ->
             manager#file_delete id >>= fun y -> x >>= fun x ->
             Lwt.return (Api_common.result_combine [x; y]))
    )

let do_a_move state file_id rank =
  match Mods.IntMap.filter_one
          (fun _ { id; _ } -> id = file_id) state.directory with
  | None ->
    let error_msg = "Internal inconsistency: No file "^file_id in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some (rank', ({ local; _ } as x)) ->
    let directory =
      Mods.IntMap.add rank x (Mods.IntMap.remove rank' state.directory) in
    let current =
      if state.current = Some rank' then Some rank else state.current in
    if local = None then
      State_project.with_project ~label:"remove_file"
        (fun manager -> manager#file_move rank file_id >>=
          Api_common.result_bind_lwt
            ~ok:(fun () -> Lwt.return (Result_util.ok { current; directory })))
    else Lwt.return (Result_util.ok { current; directory })

let rec set_position state file_id rank =
  match Mods.IntMap.find_option rank state.directory with
  | Some { id; _ } ->
    if file_id = id then Lwt.return (Result_util.ok state)
    else
      set_position state id (succ rank) >>=
      Api_common.result_bind_lwt
        ~ok:(fun state' -> do_a_move state' file_id rank)
  | None ->
    do_a_move state file_id rank

let order_files (filenames : string list) : unit Api.result Lwt.t =
  let rec _order_file filenames state index : unit Api.result Lwt.t =
    match filenames with
    | [] ->
      let () = set_directory_state state in
      Lwt.return (Result_util.ok ())
    | file_id::tail ->
      (set_position state file_id index) >>=
      Api_common.result_bind_lwt
        ~ok:(fun state' -> _order_file tail state' (index + 1))
  in
  _order_file filenames (React.S.value model) 0

let sync ?(reset=false) () : unit Api.result Lwt.t =
  State_project.with_project ~label:"select_file"
    (fun manager ->
       manager#file_catalog >>=
       Api_common.result_bind_lwt
         ~ok:(fun catalog ->
             let cand = (React.S.value model).current in
             let pos =
               if reset || match cand with
                 | None -> true
                 | Some x ->
                   List.exists (fun {Kfiles.position; _} -> x=position) catalog
               then
                 match catalog with
                 | [] -> None
                 | { Kfiles.position; _ } :: _ -> Some position
               else cand in
             let () = update_directory ~reset pos catalog in
             send_refresh None))

let load_default () : unit Lwt.t =
 (create_file ~filename:"model.ka" ~content:"") >>=
 (Result_util.fold
    ~ok:(fun () -> Lwt.return_unit)
    ~error:(fun errors ->
        let msg =
          Format.asprintf
            "State_file.load_default : creating default file error@ @[<v>%a@]"
            (Pp.list Pp.space Result_util.print_message) errors in
        let () = Common.debug (Js.string msg) in
        Lwt.return_unit))

let load_models () : unit Lwt.t =
  let models = Common_state.url_args "model" in
  let rec add_models models load_file : unit Lwt.t =
    match models with
    | [] -> Lwt.return_unit
    | model::models ->
      (* fetch model *)
      Js_of_ocaml_lwt.XmlHttpRequest.get model >>=
      (fun content ->
         if content.Js_of_ocaml_lwt.XmlHttpRequest.code <> 200 then
           Lwt.return
             (Api_common.result_error_msg
                (Format.sprintf "bad response code %d fetching url %s"
                   content.Js_of_ocaml_lwt.XmlHttpRequest.code model))
         else
           match Url.url_of_string
                   content.Js_of_ocaml_lwt.XmlHttpRequest.url with
           | None ->
             Lwt.return
               (Api_common.result_error_msg
                  (Format.sprintf "failed to retrieve url %s" model))
           | Some u ->
             let filename =
               List_util.last
                 (match u with
                  | (Url.Http h | Url.Https h) -> h.Url.hu_path
                  | Url.File f -> f.Url.fu_path) in
             let filecontent : string =
               content.Js_of_ocaml_lwt.XmlHttpRequest.content
             in
             Lwt.return (Result_util.ok (filename,filecontent))
      )
      >>=
      (* add content *)
      (Api_common.result_bind_lwt
         ~ok:(fun (filename,content) ->
             (create_file ~filename ~content) >>=
             (Api_common.result_bind_lwt
                ~ok:(fun () -> Lwt.return (Result_util.ok filename))
             ))) >>=
      (* select model if needed *)
      (Result_util.fold
         ~ok:(fun filename ->
             if load_file then
               (select_file filename None) >>=
               (Result_util.fold
                  ~ok:(fun _ -> add_models models false)
                  ~error:(fun _ -> add_models models load_file)
               )
             else
               add_models models load_file
           )
         ~error:(fun errors ->
             let msg =
               Format.asprintf
                 "creating loading url %s error@ @[<v>%a@]"
                 model
                 (Pp.list Pp.space Result_util.print_message) errors in
             let () = Common.debug
                 (Js.string (Format.sprintf "State_file.load_model %s" msg)) in
             add_models models load_file)
      )
  in
  match models with
  [] -> load_default ()
  |_::_ -> add_models models true

let init () : unit Lwt.t =
  Lwt.return_unit >>=
  load_models
