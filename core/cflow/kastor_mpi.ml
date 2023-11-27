(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let on_message ~none ~weak ~strong ~send_message =
  let parameter =
    ref
      (Compression_main.build_parameter
         ~called_from:Remanent_parameters_sig.Server ~send_message ~none ~weak
         ~strong ())
  in
  fun text ->
    try
      JsonUtil.read_variant Yojson.Basic.read_string
        (fun st b -> function
          | "CONFIG" ->
            let conf = JsonUtil.read_next_item Yojson.Basic.read_json st b in
            let none =
              match
                Yojson.Basic.Util.to_bool_option
                  (Yojson.Basic.Util.member "none" conf)
              with
              | None -> none
              | Some b -> b
            in
            let weak =
              match
                Yojson.Basic.Util.to_bool_option
                  (Yojson.Basic.Util.member "weak" conf)
              with
              | None -> weak
              | Some b -> b
            in
            let strong =
              match
                Yojson.Basic.Util.to_bool_option
                  (Yojson.Basic.Util.member "strong" conf)
              with
              | None -> strong
              | Some b -> b
            in
            let () =
              parameter :=
                Compression_main.build_parameter
                  ~called_from:Remanent_parameters_sig.Server ~send_message
                  ~none ~weak ~strong ()
            in
            ()
          | "RUN" ->
            let env, steps =
              JsonUtil.read_next_item
                (Trace.fold_trace
                   (fun _env steps step -> step :: steps)
                   (fun _ -> []))
                st b
            in
            let () =
              Compression_main.compress_and_print !parameter
                ~dotFormat:Causal.Html env
                (Compression_main.init_secret_log_info ())
                (List.rev steps)
            in
            ()
          | x -> raise (Yojson.json_error ("Invalid KaStor message: " ^ x)))
        (Yojson.Safe.init_lexer ())
        (Lexing.from_string text)
    with e ->
      let () = Format.eprintf "%s@." (Printexc.to_string e) in
      () (*TODO*)
