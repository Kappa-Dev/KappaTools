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


type uncaught_exception =
    {file_name:string option;
     message:string option;
     alarm: exn}

exception Uncaught_exception of uncaught_exception

type caught_exception =
  {uncaught_exception: uncaught_exception;
   calling_stack: string list}

exception Caught_exception of caught_exception

let raise_exception file_name key message exn =
  raise
    (Uncaught_exception
      {file_name=file_name;
        message=message;
        alarm=exn})

let rec stringlist_of_exception x stack =
  match x with
      Exit -> "Exit"::stack
    | Not_found -> "Not_found"::stack
    | Arg.Bad x -> "Arg.Bad("::x::")"::stack
    | Sys.Break -> "Sys.Break"::stack
    | Stack.Empty -> "Stack.Empty"::stack
    | Queue.Empty -> "Queue.Empty"::stack
    | Stream.Error x -> "Stream.Error"::x::stack
    | Stream.Failure -> "Stream.Failure"::stack
    | Arg.Help x -> "Arg.Help("::x::")"::stack
    | Parsing.Parse_error -> "Parsing.Parse_error"::stack
    | Scanf.Scan_failure x -> "Scanf.Scan.failure("::x::")"::stack
    | Lazy.Undefined -> "Lazy.Undefined"::stack
    | UnixLabels.Unix_error _ -> "UnixLabels.Unix_error"::stack
    | Unix.Unix_error _ -> "Unix.Unix.error"::stack
    | Failure x -> "Failure("::x::")"::stack
    | Stack_overflow -> "Stack_overflow"::stack
    | Caught_exception x  -> "Caught_exception("::(stringlist_of_caught x (")"::stack))
    | Uncaught_exception x  -> "Uncaught_exception("::(stringlist_of_uncaught x (")"::stack))
    | _ -> "Unknown"::stack
and stringlist_of_uncaught x stack =
    (match x.file_name with
        None -> ""
    | Some file_name -> "file_name: "^file_name^"; ")
    ::(match x.message with
        None -> ""
    | Some message -> "message: "^message^"; ")
    ::"exception:"
    ::(stringlist_of_exception x.alarm stack)
and stringlist_of_caught x stack =
  "calling_stack: "
  ::(List.fold_left
      (fun sol string -> string::", "::sol)
      ("; "::(stringlist_of_uncaught x.uncaught_exception ("; "::stack))))
      x.calling_stack

and stringlist_of_uncaught_light x stack =
    (match x.file_name with
        None -> ""
    | Some file_name -> "file_name: "^file_name^"; ")
    ::(match x.message with
        None -> ""
    | Some message -> "message: "^message^"; ")
    ::"exception:"
    ::(stringlist_of_exception x.alarm stack)
and stringlist_of_caught_light x stack =
  "calling_stack: "
  ::(List.fold_left
      (fun sol string -> string::", "::sol)
      ("; "::(stringlist_of_uncaught x.uncaught_exception ("; "::stack))))
      x.calling_stack

type method_handler =
  {mh_caught_error_list:caught_exception list;
   mh_uncaught_error_list:uncaught_exception list;
   mh_calling_stack: string list}

let empty_error_handler =
  {mh_caught_error_list=[];
   mh_uncaught_error_list=[];
   mh_calling_stack=[]}

let handle_errors method_handler f x default =
  try
    f x,method_handler
  with
     Uncaught_exception exn ->
      let error =
        {calling_stack = method_handler.mh_calling_stack;
         uncaught_exception = exn} in
    default,
    {method_handler with  mh_caught_error_list = error::method_handler.mh_caught_error_list}


let safe_warn parameters error_handler file message exn default =
  let uncaught = {file_name = file;
                            message = message;
                            alarm = exn}
  in
  let stringlist = stringlist_of_uncaught uncaught [Remanent_parameters.get_prefix parameters] in
  let _ =
    List.iter
      (Loggers.fprintf
	 (Remanent_parameters.get_logger parameters) "%s")
      stringlist
  in
  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  raise (Uncaught_exception
	   {
	     file_name = file;
             message = message;
             alarm = exn
	   })

let unsafe_warn parameters error_handler file message exn default =
  {error_handler
  with
    mh_uncaught_error_list =
      {file_name = file;
       message = message;
       alarm = exn}::error_handler.mh_uncaught_error_list},default ()

let warn parameters =
  if Remanent_parameters.get_unsafe parameters
  then unsafe_warn parameters
  else safe_warn parameters

let print_for_KaSim parameters handlers =
  let parameters = Remanent_parameters.update_prefix parameters "error: " in
  let _ =
    List.iter
      (fun caught ->
       let stringlist = (Remanent_parameters.get_prefix parameters)::(stringlist_of_caught caught []) in
       let _ = List.iter
	 (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s") stringlist in
       let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
       ())
      (List.rev (handlers.mh_caught_error_list))
  in
  let _ =
    List.iter
      (fun uncaught ->
       let stringlist =  (Remanent_parameters.get_prefix parameters)::(stringlist_of_uncaught uncaught []) in
       let _ = List.iter (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s") stringlist in
       let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
       ())
      (List.rev (handlers.mh_uncaught_error_list))
  in
  ()

let print parameters handlers =
  if handlers.mh_caught_error_list = [] && handlers.mh_uncaught_error_list = []
  then
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%sexecution finished without any exception" (Remanent_parameters.get_prefix parameters) in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
    ()
  else
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%sSome exceptions have been raised" (Remanent_parameters.get_prefix parameters) in
    print_for_KaSim parameters handlers

let print_errors_light_for_kasim parameters handlers =
  if handlers.mh_caught_error_list = [] && handlers.mh_uncaught_error_list = []
  then ()
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
