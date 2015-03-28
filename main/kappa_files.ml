let outputDirName = ref ""
let marshalizedOutFile = ref ""
let snapshotFileName = ref "snap"
let dumpFileName = ref "dump.ka"
let cflowFileName = ref "cflow.dot"
let profilingName = ref "profiling.txt"
let influenceFileName = ref ""
let fluxFileName = ref ""
let outputDataName = ref "data.out"

let path f =
  if Filename.is_relative f && Filename.dirname f = Filename.current_dir_name
  then Filename.concat !outputDirName f
  else f

let open_out f =
  open_out (path f)

let find_available_name name facultative ext =
  let base = try Filename.chop_extension name
	     with Invalid_argument _ -> name in
  if Sys.file_exists (base^"."^ext) then
    let base' = if facultative <> "" then base^"_"^facultative else base in
    if Sys.file_exists (base'^"."^ext) then
      let v = ref 0 in
      let () =
	while Sys.file_exists (base'^"~"^(string_of_int !v)^"."^ext)
	do incr v; done
      in base'^"~"^(string_of_int !v)^"."^ext
    else base'^"."^ext
  else base^"."^ext

let open_out_fresh_filename base_name concat_list facultative ext =
  let tmp_name =
    path (try Filename.chop_extension base_name
		with Invalid_argument _ -> base_name) in
  let base_name = String.concat "_" (tmp_name::concat_list) in
  open_out (find_available_name base_name facultative ext)

let mk_dir_r d =
  let rec aux d =
    let par = Filename.dirname d in
    let () = if not (Sys.file_exists par) then aux par in
    Unix.mkdir d 0o775 in
  Unix.handle_unix_error aux d

let set name ext_opt =
  if !name <> "" then
    let fname =
      match ext_opt with
      | None -> !name
      | Some ext ->
	 if (Filename.check_suffix !name ext) then !name
	 else
	   (!name^"."^ext)
    in
    name:=fname

let setOutputName () =
  set snapshotFileName (Some "dot");
  set dumpFileName (Some "ka");
  set influenceFileName (Some "dot") ;
  set fluxFileName (Some "dot") ;
  set marshalizedOutFile None;
  set outputDataName None

let setCheckFileExists () =
  let check file =
    match file with
    | "" -> ()
    | file ->
       let file = path file in
       if not !Parameter.batchmode && Sys.file_exists file then
	 let () =
	   Format.eprintf
	     "File '%s' already exists do you want to erase (y/N)?@." file in
	 let answer = Tools.read_input () in
	 if answer<>"y" then exit 1
  in
  let () = setOutputName () in
  check !influenceFileName ;
  check !fluxFileName ;
  check !marshalizedOutFile ;
  check !outputDataName

let (openOutDescriptors:out_channel list ref) = ref []

let add_out_desc d = openOutDescriptors := d::!openOutDescriptors

let set_dir s =
  let () = try
      if not (Sys.is_directory s)
      then (Format.eprintf "'%s' is not a directory@." s ; exit 1)
    with Sys_error _ -> mk_dir_r s in
  outputDirName := s

let set_data f = outputDataName := f
let get_data () = !outputDataName

let set_marshalized f = marshalizedOutFile := f
let with_marshalized f =
  match !marshalizedOutFile with
  | "" -> ()
  | file ->
     let d = open_out_bin (path file) in
     let () = f d in
     close_out d

let set_cflow s = cflowFileName := s
let fresh_cflow_filename l e = open_out_fresh_filename !cflowFileName l "" e

let open_profiling () = open_out !profilingName

let set_flux nme event =
  let () =
    match nme with
    | "" -> fluxFileName := "flux"^"_"^(string_of_int event)
    | _ -> fluxFileName := nme
  in
  set fluxFileName (Some "dot")
let with_flux str f =
  let desc = open_out (match str with "" -> !fluxFileName | _ -> str) in
  let fr = Format.formatter_of_out_channel desc in
  let () = add_out_desc desc in
  let () = f fr in
  close_out desc

let open_snapshot str event ext =
  let str = if str="" then !snapshotFileName else str in
  let desc =
    open_out_fresh_filename
      str [] (string_of_int event) ext in
  let () = add_out_desc desc in
  desc

let set_influence s = influenceFileName := s
let set_up_influence () =
  set_influence
    (if !influenceFileName = "" then "im.dot" else !influenceFileName)
let with_influence f =
  if !influenceFileName <> ""  then
    let desc = open_out !influenceFileName in
    let fr = Format.formatter_of_out_channel desc in
    let () = f fr in
    close_out desc

let with_dump f =
  let desc = open_out !dumpFileName in
  let () = f desc in
  let () = close_out desc in
  Format.eprintf "Final state dumped (%s)@." !dumpFileName

let close_out_desc desc =
  let () = openOutDescriptors :=
	     List.filter (fun x -> x != desc) !openOutDescriptors in
  close_out desc

let close_all_out_desc () =
  List.iter (fun d -> close_out d) !openOutDescriptors
