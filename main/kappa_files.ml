(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let outputDirName = ref ""
let marshalizedOutFile = ref ""
let cflowFileName = ref "cflow.dot"
let branch_and_cut_engine_profilingName = ref "compression_status.txt"
let tasks_profilingName = ref "profiling.html"
let fluxFileName = ref ""

let mk_dir_r d =
  let rec aux d =
    if not (Sys.file_exists d) then
      let () = aux (Filename.dirname d) in
      Unix.mkdir d 0o775 in
  Unix.handle_unix_error aux d

let overwrite_permission = ref false

let path f =
  if Filename.is_implicit f then Filename.concat !outputDirName f else f

let get_fresh_filename base_name concat_list facultative ext =
  let tmp_name = try Filename.chop_extension base_name
    with Invalid_argument _ -> base_name in
  let base_name = String.concat "_" (tmp_name::concat_list) in
  Tools.find_available_name
    ~already_there:(fun x -> Sys.file_exists (path x))
    base_name ~facultative ~ext

let open_out f =
  let x = path f in
  let () = mk_dir_r (Filename.dirname x) in
  open_out x

let open_out_fresh base_name facultative ext =
  let x =
    if !overwrite_permission then
      let base = try Filename.chop_extension base_name
        with Invalid_argument _ -> base_name in
      base^ext
    else get_fresh_filename base_name [] facultative ext in
  let () = mk_dir_r (Filename.dirname x) in
  open_out x

let set name ext_opt =
  if !name <> "" then
    let fname =
      match ext_opt with
      | None -> !name
      | Some ext ->
        if (Filename.check_suffix !name ext) then !name
        else
          (!name^"."^ext) in
    name:=fname

let setOutputName () =
  set fluxFileName (Some "dot") ;
  set marshalizedOutFile None

let check_not_exists = function
  | "" -> ()
  | file ->
    let file = path file in
    if Sys.file_exists file then
      let () =
        Format.eprintf
          "File '%s' already exists do you want to erase (y/N)?@." file in
      let answer = Tools.read_input () in
      if answer<>"y" && answer<>"Y" && answer<>"yes" &&
         answer<>"YES" && answer<>"Yes" then exit 1
      else overwrite_permission := true

let setCheckFileExists ~batchmode outputFile =
  let () = setOutputName () in
  if batchmode then overwrite_permission := true
  else
    let () = check_not_exists !fluxFileName in
    let () = check_not_exists !marshalizedOutFile in
    check_not_exists outputFile

let with_channel str f =
  if str <> ""  then
    let desc = open_out str in
    let () = f desc in
    close_out desc

let wrap_formatter f desc =
  let fr = Format.formatter_of_out_channel desc in
  let () = f fr in
  Format.pp_print_flush fr ()

let with_formatter str f =
  with_channel str (wrap_formatter f)

let set_dir s =
  let () = try
      if not (Sys.is_directory s)
      then (Format.eprintf "'%s' is not a directory@." s ; exit 1)
    with Sys_error _ -> mk_dir_r s in
  outputDirName := s

let get_dir () = !outputDirName

let set_marshalized f = marshalizedOutFile := f
let with_marshalized f =
  match !marshalizedOutFile with
  | "" -> ()
  | file ->
    let x = path file in
    let () = mk_dir_r (Filename.dirname x) in
    let d = open_out_bin x in
    let () = f d in
    close_out d

let set_cflow s = cflowFileName := s
let get_cflow l e = get_fresh_filename !cflowFileName l "" e
let with_cflow_file l e f =
  with_formatter (get_fresh_filename !cflowFileName l "" e) f

let open_tasks_profiling () = open_out !tasks_profilingName
let open_branch_and_cut_engine_profiling () = open_out !branch_and_cut_engine_profilingName
let set_flux nme event =
  let () =
    match nme with
    | "" -> fluxFileName := "flux"^"_"^(string_of_int event)
    | _ -> fluxFileName := nme
  in
  set fluxFileName (Some "dot")

let with_flux str f =
  with_channel (match str with "" -> !fluxFileName | _ -> str) f

let with_snapshot str ext event f =
  let desc = open_out_fresh str (string_of_int event) ext in
  let () = f desc in
  close_out desc
