let file = ref ""
let dotCflows = ref Causal.Dot
let none_compression = ref false
let weak_compression = ref false
let strong_compression = ref false

let options = [
  ("--version",
   Arg.Unit (fun () -> Format.print_string Version.version_msg;
              Format.print_newline () ; exit 0),
   "display KaStor version");
  ("-o", Arg.String Kappa_files.set_cflow,
   "file name skeleton for outputs") ;
  ("-d",
   Arg.String Kappa_files.set_dir,
   "Specifies directory name where output file(s) should be stored");
  ("--none", Arg.Set none_compression, "Outputs uncompressed stories");
  ("--weak", Arg.Set weak_compression, "Outputs weakly compressed stories");
  ("--strong",
   Arg.Set strong_compression,
   "Outputs strongly compressed stories");
  ("-format", Arg.String
                (function
                   "true" | "yes" | "dot" -> dotCflows := Causal.Dot
                   |"false" | "no" | "html"-> dotCflows := Causal.Html
                   |"json" -> dotCflows := Causal.Json
                   | _ as error  ->
                      raise
                        (ExceptionDefn.Malformed_Decl
                           (Locality.dummy_annot
                              ("Value "^error^
                              " should be either \"html, dot\" or \"json\"")))),
                      "Print stories in html format");
   ("--time-independent",
    Arg.Set Parameter.time_independent,
    "Disable the use of time is story heuritics (for test suite)")
  ]

let server_mode () =
  Stream.iter (function
      | `Json (`Assoc ["command", `String "Quit"]) -> ()
      | `Json json ->
        begin
          try
            let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
            let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
            let none = match Yojson.Basic.Util.to_bool_option
                               (Yojson.Basic.Util.member "none" json)
              with None -> false | Some b -> b in
            let weak = match Yojson.Basic.Util.to_bool_option
                               (Yojson.Basic.Util.member "weak" json)
              with None -> false | Some b -> b in
            let strong = match Yojson.Basic.Util.to_bool_option
                                 (Yojson.Basic.Util.member "strong" json)
              with None -> false | Some b -> b in
            let parameter =
              Compression_main.build_parameter
                ~called_from:Remanent_parameters_sig.Server
                ~none ~weak ~strong in
            Compression_main.compress_and_print
              parameter ~dotFormat:Causal.Html
              env (Compression_main.init_secret_log_info ())
              steps
          with Yojson.Basic.Util.Type_error (e,x) ->
            Format.eprintf "%s:@ %s@." e (Yojson.Basic.pretty_to_string x)
        end
      | `Exn (Yojson.Json_error e) -> Format.eprintf "%s@." e
      | `Exn e -> Format.eprintf "%s@." (Printexc.to_string e))
    (Yojson.Basic.linestream_from_channel stdin)


let get_simulation fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_lcurl lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "uuid") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let _ = Yojson.Basic.read_string lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_comma lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "env") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let env = Model.of_yojson
      (Yojson.Basic.read_json lex_st lex_buf) in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_comma lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "trace") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let steps = Yojson.Basic.read_list
      (fun x y -> Trace.step_of_yojson (Yojson.Basic.read_json x y))
      lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = try Yojson.Basic.read_object_end lex_buf
    with Yojson.End_of_object -> () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = close_in desc in
  (env,steps)

let main () =
  let () =
    Arg.parse
      options
      (fun f -> if !file = "" then file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      (Sys.argv.(0) ^
       " trace\n computes stories from 'trace' file generated by KaSim") in
  if!file = "" then
    server_mode ()
  else
    let (none,weak,strong) =
      (!none_compression, !weak_compression, !strong_compression) in
    let parameter =
      Compression_main.build_parameter
        ~called_from:Remanent_parameters_sig.KaSim ~none ~weak ~strong in
    let () =
      Loggers.fprintf (Compression_main.get_logger parameter)
        "+ Loading trace@." in
    let dotFormat = !dotCflows in
    let env,steps = get_simulation !file in
    Compression_main.compress_and_print
      parameter ~dotFormat env (Compression_main.init_secret_log_info ()) steps

let () = Sys.catch_break true  
let () = main ()
