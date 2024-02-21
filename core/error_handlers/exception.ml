(**
 * exception.ml
 * openkappa
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: 08/03/2010
 * Last modification: Time-stamp: <Aug 15 2016>
 * *
 * This library declares exceptions
 *
 * Copyright 2010 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 *  under the terms of the GNU Library General Public License *)

type method_handler = Exception_without_parameter.method_handler

let empty_error_handler = Exception_without_parameter.empty_error_handler
let is_empty_error_handler = Exception_without_parameter.is_empty_error_handler

let safe_warn parameters _error_handler file_name message exn _default =
  let uncaught =
    Exception_without_parameter.build_uncaught_exception ?file_name ?message exn
  in
  let stringlist =
    Exception_without_parameter.stringlist_of_uncaught uncaught
      [ Remanent_parameters.get_prefix parameters ]
  in
  let _ =
    List.iter
      (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s")
      stringlist
  in
  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  raise (Exception_without_parameter.Uncaught_exception uncaught)

let unsafe_warn _parameters error_handler ?to_ui file_name message exn default =
  let uncaught =
    Exception_without_parameter.build_uncaught_exception ?file_name ?message exn
  in
  ( Exception_without_parameter.add_uncaught_error uncaught ?to_ui error_handler,
    default () )

let warn_aux parameters error_handler ?to_ui file message exn default =
  let error, dft =
    if Remanent_parameters.get_unsafe parameters then
      unsafe_warn parameters error_handler ?to_ui file message exn default
    else
      safe_warn parameters error_handler file message exn default
  in
  let () = Remanent_parameters.save_error_list parameters error in
  error, dft

let warn_with_exn parameters error_handler ?to_ui (file, line, _, _)
    ?(message = "") ?(pos = None) exn default =
  let liaison =
    if message = "" && pos = None then
      ""
    else
      ": "
  in
  let pos =
    match pos with
    | None -> ""
    | Some s -> ", " ^ Loc.to_string s
  in
  warn_aux parameters error_handler ?to_ui (Some file)
    (Some ("line " ^ string_of_int line ^ pos ^ liaison ^ message))
    exn default

let warn parameters error_handler ?to_ui file_line ?(message = "") ?pos exn
    default =
  warn_with_exn parameters error_handler ?to_ui file_line ~message ~pos exn
    (fun () -> default)

let print_for_KaSim parameters handlers =
  let parameters = Remanent_parameters.update_prefix parameters "error: " in
  let _ =
    List.iter
      (fun caught ->
        let stringlist =
          Remanent_parameters.get_prefix parameters
          :: Exception_without_parameter.stringlist_of_caught caught []
        in
        let _ =
          List.iter
            (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s")
            stringlist
        in
        let _ =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        ())
      (List.rev
         (Exception_without_parameter.get_caught_exception_list handlers))
  in
  let _ =
    List.iter
      (fun uncaught ->
        let stringlist =
          Remanent_parameters.get_prefix parameters
          :: Exception_without_parameter.stringlist_of_uncaught uncaught []
        in
        let _ =
          List.iter
            (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s")
            stringlist
        in
        let _ =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        ())
      (List.rev
         (Exception_without_parameter.get_uncaught_exception_list handlers))
  in
  ()

let print parameters handlers =
  if
    Exception_without_parameter.get_caught_exception_list handlers = []
    && Exception_without_parameter.get_uncaught_exception_list handlers = []
  then (
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sexecution finished without any exception"
        (Remanent_parameters.get_prefix parameters)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    ()
  ) else (
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sSome exceptions have been raised"
        (Remanent_parameters.get_prefix parameters)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    print_for_KaSim parameters handlers
  )

let print_errors_light_for_kasim parameters handlers =
  if
    Exception_without_parameter.get_caught_exception_list handlers = []
    && Exception_without_parameter.get_uncaught_exception_list handlers = []
  then
    ()
  else (
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sSome exceptions have been raised during the static analysis, please \
         analyse your file with KaSa"
        (Remanent_parameters.get_prefix parameters)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    ()
  )

let wrap parameters error string string_opt exn =
  fst (warn_aux parameters error (Some string) string_opt exn (fun () -> ()))

let _lift_error_logs_for_KaSa f =
  f (fun parameters error string string_opt exn ->
      fst
        (warn_aux parameters error (Some string) string_opt exn (fun () -> ())))

let check_point
    (warn :
      Remanent_parameters_sig.parameters ->
      method_handler ->
      ?to_ui:bool ->
      'a ->
      ?message:string ->
      ?pos:Loc.t ->
      exn ->
      unit ->
      method_handler * unit) parameter error error' s ?to_ui ?message ?pos exn =
  if error == error' then
    error
  else (
    let error, () = warn parameter error' ?to_ui s ?message ?pos exn () in
    error
  )
