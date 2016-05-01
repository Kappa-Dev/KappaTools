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

let build_uncaught_exception file_name message exn =
  {
    file_name = file_name;
    message = message ;
    alarm = exn ;
  }

let build_caught_exception file_name message exn stack =
  {
    uncaught_exception = build_uncaught_exception file_name message exn ;
    calling_stack = stack ;
  }

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
let add_uncaught_error uncaught error = {error with mh_uncaught_error_list = uncaught::error.mh_uncaught_error_list}
let get_caught_exception_list error = error.mh_caught_error_list
let get_uncaught_exception_list error = error.mh_uncaught_error_list
