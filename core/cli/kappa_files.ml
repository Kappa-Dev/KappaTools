(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
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
    if not (Sys.file_exists d) then (
      let () = aux (Filename.dirname d) in
      Unix.mkdir d 0o775
    )
  in
  Unix.handle_unix_error aux d

let overwrite_permission = ref false

let path f =
  if Filename.is_implicit f then
    Filename.concat !outputDirName f
  else
    f

let open_out f =
  let x = path f in
  let () = mk_dir_r (Filename.dirname x) in
  open_out x

let rec aux_open_out_fresh base v ext =
  try
    open_out_gen
      [ Open_wronly; Open_creat; Open_excl; Open_text ]
      0o666
      (base ^ "~" ^ string_of_int v ^ ext)
  with Sys_error _ -> aux_open_out_fresh base (succ v) ext

let open_out_fresh name concat_list facultative ext =
  let name = path name in
  let () = mk_dir_r (Filename.dirname name) in
  let tmp_name = Tools.chop_suffix_or_extension name ext in
  let base = String.concat "_" (tmp_name :: concat_list) in
  let over_flag =
    if !overwrite_permission then
      Open_trunc
    else
      Open_excl
  in
  let flags = [ Open_wronly; Open_creat; over_flag; Open_text ] in
  try open_out_gen flags 0o666 (base ^ ext)
  with Sys_error _ ->
    let base' =
      if facultative <> "" then
        base ^ "_" ^ facultative
      else
        base
    in
    (try open_out_gen flags 0o666 (base' ^ ext)
     with Sys_error _ -> aux_open_out_fresh base' 0 ext)

let set name ext_opt =
  if !name <> "" then (
    let fname =
      match ext_opt with
      | None -> !name
      | Some ext ->
        if Filename.check_suffix !name ext then
          !name
        else
          !name ^ "." ^ ext
    in
    name := fname
  )

let setOutputName () =
  set fluxFileName (Some "dot");
  set marshalizedOutFile None

let check_not_exists = function
  | "" -> ()
  | file ->
    let file = path file in
    if Sys.file_exists file then (
      let () =
        Format.eprintf "File '%s' already exists do you want to erase (y/N)?@."
          file
      in
      let answer = Tools.read_input () in
      if
        answer <> "y" && answer <> "Y" && answer <> "yes" && answer <> "YES"
        && answer <> "Yes"
      then
        exit 1
      else
        overwrite_permission := true
    )

let setCheckFileExists ~batchmode outputFile =
  let () = setOutputName () in
  if batchmode then
    overwrite_permission := true
  else (
    let () = check_not_exists !fluxFileName in
    let () = check_not_exists !marshalizedOutFile in
    check_not_exists outputFile
  )

let with_channel str f =
  if str <> "" then (
    let desc = open_out str in
    let () = f desc in
    close_out desc
  )

let wrap_formatter f desc =
  let fr = Format.formatter_of_out_channel desc in
  let () = f fr in
  Format.pp_print_flush fr ()

let set_dir s =
  let () =
    try
      if not (Sys.is_directory s) then (
        Format.eprintf "'%s' is not a directory@." s;
        exit 1
      )
    with Sys_error _ -> mk_dir_r s
  in
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

let with_cflow_file l e f =
  let desc = open_out_fresh !cflowFileName l "" e in
  let () = wrap_formatter f desc in
  close_out desc

let open_tasks_profiling () = open_out !tasks_profilingName

let open_branch_and_cut_engine_profiling () =
  open_out !branch_and_cut_engine_profilingName

let set_flux nme event =
  let () =
    match nme with
    | "" -> fluxFileName := "flux" ^ "_" ^ string_of_int event
    | _ -> fluxFileName := nme
  in
  set fluxFileName (Some "dot")

let with_flux str f =
  with_channel
    (match str with
    | "" -> !fluxFileName
    | _ -> str)
    f

let with_snapshot str ext event f =
  let desc = open_out_fresh str [] (string_of_int event) ext in
  let () = f desc in
  close_out desc
