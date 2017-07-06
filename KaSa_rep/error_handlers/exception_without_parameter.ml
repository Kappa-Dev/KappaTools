    (**
    * exception.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 08/03/2010
 * Last modification: Time-stamp: <Nov 28 2016>
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
type caught_exception =
  {uncaught_exception: uncaught_exception;
   calling_stack: string list}

exception Uncaught_exception of uncaught_exception
exception Caught_exception of caught_exception

let rec exn_to_json =
  function
  | Exit -> `Assoc ["Exit", `Null]
  | Not_found -> `Assoc ["Not_found", `Null]
  | Arg.Bad x -> `Assoc ["Arg.Bad", JsonUtil.of_string x]
  | Sys.Break -> `Assoc ["Sys.Break", `Null]
  | Stack.Empty -> `Assoc ["Stack.Empty", `Null]
  | Queue.Empty -> `Assoc ["Queue.Empty", `Null]
  | Stream.Error x -> `Assoc ["Stream.Error", JsonUtil.of_string x]
  | Stream.Failure -> `Assoc ["Stream.Failure", `Null]
  | Arg.Help x -> `Assoc ["Arg.Help", JsonUtil.of_string x]
  | Parsing.Parse_error -> `Assoc ["Parsing.Parse_error", `Null]
  | Scanf.Scan_failure x -> `Assoc ["Scan_failure", JsonUtil.of_string x]
  | Lazy.Undefined -> `Assoc ["Lazy.Undefined", `Null]
  | UnixLabels.Unix_error (a,b,c) ->
    `Assoc
      ["UnixLabels.Unix_error",
       `Assoc
         ["fst",JsonUtil.of_unix_label a;
          "snd",JsonUtil.of_string b;
          "trd",JsonUtil.of_string c]]
  | Unix.Unix_error (a,b,c) ->
    `Assoc
      [
        "Unix.Unix.error",
        `Assoc
          ["fst",JsonUtil.of_unix_error a;
           "snd",JsonUtil.of_string b;
           "trd",JsonUtil.of_string c]
      ]
  | Failure x ->  `Assoc ["Failure", JsonUtil.of_string x]
  | Stack_overflow -> `Assoc ["Stack_overflow", `Null]
  | Caught_exception x  -> `Assoc ["Caught", caught_exception_to_json x]
  | Uncaught_exception x  -> `Assoc ["Uncaught", uncaught_exception_to_json x]
  | _ -> `Assoc ["Unknown", `Null]

and uncaught_exception_to_json uncaught =
  JsonUtil.of_pair
    ~lab1:"file_name" ~lab2:"message"
    (JsonUtil.of_option JsonUtil.of_string)
    (JsonUtil.of_option JsonUtil.of_string)
    (uncaught.file_name, uncaught.message)

and caught_exception_to_json caught =
  JsonUtil.of_pair
    ~lab1:"uncaught_exception" ~lab2:"calling_stack"
    uncaught_exception_to_json
    (JsonUtil.of_list JsonUtil.of_string)
    (caught.uncaught_exception, caught.calling_stack)


let rec exn_of_json (json : Yojson.Basic.json) =
  match
    json
  with
  | `Assoc ["Exit", `Null] -> Exit
  | `Assoc ["Not_found", `Null] -> Not_found
  | `Assoc ["Arg.Bad", x] -> Arg.Bad (JsonUtil.to_string x)
  | `Assoc ["Sys.Break", `Null] -> Sys.Break
  | `Assoc ["Stack.Empty", `Null] -> Stack.Empty
  | `Assoc ["Queue.Empty", `Null] -> Queue.Empty
  | `Assoc ["Stream.Error", x] -> Stream.Error (JsonUtil.to_string x)
  | `Assoc ["Stream.Failure", `Null] -> Stream.Failure
  | `Assoc ["Arg.Help", x] -> Arg.Help (JsonUtil.to_string x)
  | `Assoc ["Parsing.Parse_error", `Null] -> Parsing.Parse_error
  | `Assoc ["Scan_failure", x] -> Scanf.Scan_failure (JsonUtil.to_string x)
  | `Assoc ["Lazy.Undefined", `Null] -> Lazy.Undefined
  | `Assoc
      ["UnixLabels.Unix_error", `Assoc l] when List.length l = 3 ->
    begin
      try
        UnixLabels.Unix_error
          (JsonUtil.to_unix_label (List.assoc "fst" l),
           JsonUtil.to_string (List.assoc "snd" l),
           JsonUtil.to_string (List.assoc "trd" l))
      with
      | _ ->
        raise (Yojson.Basic.Util.Type_error
                 (JsonUtil.build_msg "unix labels error",json))
    end
    | `Assoc
        ["Unix.Unix_error", `Assoc l] when List.length l = 3 ->
      begin
        try
          Unix.Unix_error
            (JsonUtil.to_unix_label (List.assoc "fst" l),
             JsonUtil.to_string (List.assoc "snd" l),
             JsonUtil.to_string (List.assoc "trd" l))
        with
        | _ ->
          raise (Yojson.Basic.Util.Type_error
                   (JsonUtil.build_msg "unix error",json))
      end
    | `Assoc ["Failure", x] -> Failure (JsonUtil.to_string x)
  | `Assoc ["Stack_overflow", `Null] -> Stack_overflow
  | `Assoc ["Caught", x] ->  Caught_exception (caught_exception_of_json x)
  | `Assoc ["Uncaught", x] -> Uncaught_exception (uncaught_exception_of_json x)
  | `Assoc ["Unknown", `Null] -> Failure "Unknown"
  | _ ->
    raise (Yojson.Basic.Util.Type_error
                   (JsonUtil.build_msg "exception",json))

and uncaught_exception_of_json json =
  let a,b =
    JsonUtil.to_pair
    ~lab1:"file_name" ~lab2:"message"
    (JsonUtil.to_option (JsonUtil.to_string ~error_msg:"file_name"))
    (JsonUtil.to_option (JsonUtil.to_string ~error_msg:"message"))
    json
  in
  {
    file_name = a ;
    message = b ;
    alarm = Exit
  }


and caught_exception_of_json json =
  let a,b =
    JsonUtil.to_pair
    ~lab1:"uncaught_exception" ~lab2:"calling_stack"
    uncaught_exception_of_json
    (JsonUtil.to_list ~error_msg:"calling stack" (JsonUtil.to_string ~error_msg:"stack elt"))
    json
  in
  {
    uncaught_exception = a;
    calling_stack = b
  }




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

let to_json method_handler =
  `Assoc
    [
      "caught",
      JsonUtil.of_list
        caught_exception_to_json method_handler.mh_caught_error_list;
      "uncaught",
      JsonUtil.of_list
        uncaught_exception_to_json method_handler.mh_uncaught_error_list;
      "calling_stack",
      JsonUtil.of_list
        JsonUtil.of_string
        method_handler.mh_calling_stack
    ]

let of_json =
  function
  | `Assoc l as x when List.length l = 3->
    begin
      try
        let caught =
          (JsonUtil.to_list caught_exception_of_json)
            (List.assoc "caught" l)
        in
        let uncaught =
          (JsonUtil.to_list uncaught_exception_of_json)
            (List.assoc "uncaught" l)
        in
        let stack =
          (JsonUtil.to_list (JsonUtil.to_string ~error_msg:"calling stack"))
            (List.assoc "calling_stack" l)
        in
        {
          mh_caught_error_list = caught ;
          mh_uncaught_error_list = uncaught ;
          mh_calling_stack = stack
        }
      with
      | _ ->
        raise
          (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "error handler",x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "error handler",x))

let empty_error_handler =
  {
    mh_caught_error_list=[];
    mh_uncaught_error_list=[];
    mh_calling_stack=[]
  }

let add_uncaught_error uncaught error = {error with mh_uncaught_error_list = uncaught::error.mh_uncaught_error_list}
let get_caught_exception_list error = error.mh_caught_error_list
let get_uncaught_exception_list error = error.mh_uncaught_error_list

let is_empty_error_handler x =
  x.mh_caught_error_list=[] && x.mh_uncaught_error_list=[]
