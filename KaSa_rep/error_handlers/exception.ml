    (**
    * exception.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 08/03/2010
    * Last modification: 05/02/2015
    * *
    * This library declares exceptions
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)


type method_handler = Exception_without_parameter.method_handler
let empty_error_handler = Exception_without_parameter.empty_error_handler

let safe_warn parameters error_handler file message exn default =
  let uncaught = Exception_without_parameter.build_uncaught_exception file message exn in
  let stringlist = Exception_without_parameter.stringlist_of_uncaught uncaught [Remanent_parameters.get_prefix parameters] in
  let _ =
    List.iter
      (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s")
      stringlist
  in
  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  raise (Exception_without_parameter.Uncaught_exception uncaught)

let unsafe_warn parameters error_handler file message exn default =
  let uncaught = Exception_without_parameter.build_uncaught_exception file message exn in
  Exception_without_parameter.add_uncaught_error uncaught error_handler, default ()

let warn parameters =
  if Remanent_parameters.get_unsafe parameters
  then unsafe_warn parameters
  else safe_warn parameters

let print_for_KaSim parameters handlers =
  let parameters = Remanent_parameters.update_prefix parameters "error: " in
  let _ =
    List.iter
      (fun caught ->
       let stringlist = (Remanent_parameters.get_prefix parameters)::(Exception_without_parameter.stringlist_of_caught caught []) in
       let _ = List.iter
	 (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s") stringlist in
       let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
       ())
      (List.rev (Exception_without_parameter.get_caught_exception_list handlers))
      in
  let _ =
    List.iter
      (fun uncaught ->
       let stringlist =  (Remanent_parameters.get_prefix parameters)::(Exception_without_parameter.stringlist_of_uncaught uncaught []) in
       let _ = List.iter (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s") stringlist in
       let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
       ())
      (List.rev (Exception_without_parameter.get_uncaught_exception_list handlers))
      in
  ()

let print parameters handlers =
  if
    Exception_without_parameter.get_caught_exception_list handlers = []
    &&
    Exception_without_parameter.get_uncaught_exception_list handlers = []
  then
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%sexecution finished without any exception" (Remanent_parameters.get_prefix parameters) in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
    ()
  else
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%sSome exceptions have been raised" (Remanent_parameters.get_prefix parameters) in
    print_for_KaSim parameters handlers

let print_errors_light_for_kasim parameters handlers =
  if Exception_without_parameter.get_caught_exception_list handlers = []
     &&
     Exception_without_parameter.get_uncaught_exception_list handlers = []
  then
    ()
  else
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%sSome exceptions have been raised during the static analysis, please analyse your file with KaSa" (Remanent_parameters.get_prefix parameters) in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
    ()

let wrap = (fun parameters error string string_opt exn -> fst (warn parameters error (Some string) string_opt exn (fun  () -> ())))

let lift_error_logs_for_KaSa f = f (fun parameters error string string_opt exn -> fst (warn parameters error (Some string) string_opt exn (fun  () -> ())))

let check warn parameter error error' s exn =
  if error==error'
  then error
  else
    let error,() = warn parameter error' s exn () in
    error
