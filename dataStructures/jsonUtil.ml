(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let read_between_spaces f lex_st lex_buf =
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let x = f lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  x

let build_msg s = "Not a correct "^s
let of_string (s:string) = `String s

let to_string ?error_msg:(error_msg=build_msg "string") =
  function
  | `String (s:string) -> s
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_int (s:int) = `Int s

let to_int ?error_msg:(error_msg=build_msg "int") =
  function
  | `Int (s:int) -> s
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_unit () = `Null

let to_unit ?error_msg:(error_msg=build_msg "unit") =
  function
  | `Null -> ()
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_option to_json = function
  | None -> `Null
  | Some x -> match to_json x with
    | `Null -> failwith "ambiguous JsonUtil.of_option"
    | x -> x

let to_option = Yojson.Basic.Util.to_option

let of_list to_json l =
  `List (List.rev_map to_json (List.rev l))

let to_list ?error_msg:(error_msg=build_msg "list") of_json = function
  | `List l as x ->
    begin
      try List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | `Null -> []
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_array to_json a =
  `List (Array.fold_right (fun x acc -> to_json x::acc) a [])

let to_array ?error_msg:(error_msg=build_msg "array") of_json = function
  | `List l -> Tools.array_map_of_list of_json l
  | `Null -> [||]
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let smart_assoc l =
  `Assoc (List.rev (List.fold_left (fun acc -> function
      | _,(`Null | `Assoc [] | `List []) -> acc
      | x -> x::acc) [] l))

let of_assoc to_json l =
  `Assoc (List.rev_map to_json (List.rev l))

let to_assoc
    ?error_msg:(error_msg=build_msg "association")
    of_json json =
  match json
  with
  | `Assoc l as x ->
    begin
      try
        List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | `Null -> []
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))


let of_pair ?(lab1="first") ?(lab2="second") to_json1 to_json2 (a,b) =
  `Assoc [ lab1, to_json1 a; lab2, to_json2 b ]

let to_triple
    ?lab1:(lab1="first") ?lab2:(lab2="second") ?lab3:(lab3="third")
    ?error_msg:(error_msg=build_msg "triple")
    of_json1 of_json2 of_json3 =
  function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        of_json1 (List.assoc lab1 l),
        of_json2 (List.assoc lab2 l),
        of_json3 (List.assoc lab3 l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_triple ?(lab1="first") ?(lab2="second") ?(lab3="third")
      to_json1 to_json2 to_json3 (a,b,c) =
    `Assoc [ lab1, to_json1 a; lab2, to_json2 b ; lab3, to_json3 c]

let to_pair ?lab1:(lab1="first") ?lab2:(lab2="second")
    ?error_msg:(error_msg=build_msg "pair") of_json1 of_json2 =
  function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        of_json1 (List.assoc lab1 l),
        of_json2 (List.assoc lab2 l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (error_msg,x))


let of_map
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    ~fold key_to_json value_to_json map =
  `List
    (List.rev
       (fold
          (fun (key:'key) (value:'value) (list:Yojson.Basic.json list) ->
             (`Assoc [
                 lab_key,key_to_json key;
                 lab_value,value_to_json value
               ])::list
          )
          map
          [])
    )

let to_map
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    ?error_msg:(error_msg=build_msg "map")
    ~add ~empty json_to_key json_to_value =
  function
  | `List l ->
    List.fold_left
      (fun map x ->
         match x
         with `Assoc  l as x when List.length l = 2 ->
           begin
             try
               add
                 (json_to_key (List.assoc lab_key l))
                 (json_to_value (List.assoc lab_value l))
                 map
             with Not_found ->
               raise
                 (Yojson.Basic.Util.Type_error (error_msg,x))
           end
            | x ->
              raise
                (Yojson.Basic.Util.Type_error (error_msg,x)))
      empty
      l
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_unix_label =
  function
  | UnixLabels.E2BIG -> `Assoc ["E2BIG",`Null]
  | UnixLabels.EACCES -> `Assoc ["EACCES",`Null]
  | UnixLabels.EAGAIN -> `Assoc ["EAGAIN",`Null]
  | UnixLabels.EBADF -> `Assoc ["EBADF",`Null]
  | UnixLabels.EBUSY -> `Assoc ["EBUSY",`Null]
  | UnixLabels.ECHILD -> `Assoc ["ECHILD",`Null]
  | UnixLabels.EDEADLK -> `Assoc ["EDEADLK",`Null]
  | UnixLabels.EDOM -> `Assoc ["EDOM",`Null]
  | UnixLabels.EEXIST -> `Assoc ["EEXIST",`Null]
  | UnixLabels.EFAULT -> `Assoc ["EFAULT",`Null]
  | UnixLabels.EFBIG -> `Assoc ["EFBIG",`Null]
  | UnixLabels.EINTR -> `Assoc ["EINTR",`Null]
  | UnixLabels.EINVAL -> `Assoc ["EINVAL",`Null]
  | UnixLabels.EIO -> `Assoc ["EIO",`Null]
  | UnixLabels.EISDIR -> `Assoc ["EISDIR",`Null]
  | UnixLabels.EMFILE -> `Assoc ["EMFILE",`Null]
  | UnixLabels.EMLINK -> `Assoc ["EMLINK",`Null]
  | UnixLabels.ENAMETOOLONG -> `Assoc ["ENAMETOOLONG",`Null]
  | UnixLabels.ENFILE -> `Assoc ["ENFILE",`Null]
  | UnixLabels.ENODEV -> `Assoc ["ENODEV",`Null]
  | UnixLabels.ENOENT -> `Assoc ["ENOENT",`Null]
  | UnixLabels.ENOEXEC -> `Assoc ["ENOEXEC",`Null]
  | UnixLabels.ENOLCK -> `Assoc ["ENOLCK",`Null]
  | UnixLabels.ENOMEM -> `Assoc ["ENOMEM",`Null]
  | UnixLabels.ENOSPC -> `Assoc ["ENOSPC",`Null]
  | UnixLabels.ENOSYS -> `Assoc ["ENOSYS",`Null]
  | UnixLabels.ENOTDIR -> `Assoc ["ENOTDIR",`Null]
  | UnixLabels.ENOTEMPTY -> `Assoc ["ENOTEMPTY",`Null]
  | UnixLabels.ENOTTY -> `Assoc ["ENOTTY",`Null]
  | UnixLabels.ENXIO -> `Assoc ["ENXIO",`Null]
  | UnixLabels.EPERM -> `Assoc ["EPERM",`Null]
  | UnixLabels.EPIPE -> `Assoc ["EPIPE",`Null]
  | UnixLabels.ERANGE -> `Assoc ["ERANGE",`Null]
  | UnixLabels.EROFS -> `Assoc ["EROFS",`Null]
  | UnixLabels.ESPIPE -> `Assoc ["ESPIPE",`Null]
  | UnixLabels.ESRCH -> `Assoc ["ESRCH",`Null]
  | UnixLabels.EXDEV -> `Assoc ["EXDEV",`Null]
  | UnixLabels.EWOULDBLOCK -> `Assoc ["EWOULDBLOCK",`Null]
  | UnixLabels.EINPROGRESS -> `Assoc ["EINPROGRESS",`Null]
  | UnixLabels.EALREADY -> `Assoc ["EALREADY",`Null]
  | UnixLabels.ENOTSOCK -> `Assoc ["ENOTSOCK",`Null]
  | UnixLabels.EDESTADDRREQ -> `Assoc ["EDESTADDRREQ",`Null]
  | UnixLabels.EMSGSIZE -> `Assoc ["EMSGSIZE",`Null]
  | UnixLabels.EPROTOTYPE -> `Assoc ["EPROTOTYPE",`Null]
  | UnixLabels.ENOPROTOOPT -> `Assoc ["ENOPROTOOPT",`Null]
  | UnixLabels.EPROTONOSUPPORT -> `Assoc ["EPROTONOSUPPORT",`Null]
  | UnixLabels.ESOCKTNOSUPPORT -> `Assoc ["ESOCKTNOSUPPORT",`Null]
  | UnixLabels.EOPNOTSUPP -> `Assoc ["EOPNOTSUPP",`Null]
  | UnixLabels.EPFNOSUPPORT -> `Assoc ["EPFNOSUPPORT",`Null]
  | UnixLabels.EAFNOSUPPORT -> `Assoc ["EAFNOSUPPORT",`Null]
  | UnixLabels.EADDRINUSE -> `Assoc ["EADDRINUSE",`Null]
  | UnixLabels.EADDRNOTAVAIL -> `Assoc ["EADDRNOTAVAIL",`Null]
  | UnixLabels.ENETDOWN -> `Assoc ["ENETDOWN",`Null]
  | UnixLabels.ENETUNREACH -> `Assoc ["ENETUNREACH",`Null]
  | UnixLabels.ENETRESET -> `Assoc ["ENETRESET",`Null]
  | UnixLabels.ECONNABORTED -> `Assoc ["ECONNABORTED",`Null]
  | UnixLabels.ECONNRESET -> `Assoc ["ECONNRESET",`Null]
  | UnixLabels.ENOBUFS -> `Assoc ["ENOBUFS",`Null]
  | UnixLabels.EISCONN -> `Assoc ["EISCONN",`Null]
  | UnixLabels.ENOTCONN -> `Assoc ["ENOTCONN",`Null]
  | UnixLabels.ESHUTDOWN -> `Assoc ["ESHUTDOWN",`Null]
  | UnixLabels.ETOOMANYREFS -> `Assoc ["ETOOMANYREFS",`Null]
  | UnixLabels.ETIMEDOUT -> `Assoc ["ETIMEDOUT",`Null]
  | UnixLabels.ECONNREFUSED -> `Assoc ["ECONNREFUSED",`Null]
  | UnixLabels.EHOSTDOWN -> `Assoc ["EHOSTDOWN",`Null]
  | UnixLabels.EHOSTUNREACH -> `Assoc ["EHOSTUNREACH",`Null]
  | UnixLabels.ELOOP -> `Assoc ["ELOOP",`Null]
  | UnixLabels.EOVERFLOW -> `Assoc ["EOVERFLOW",`Null]
  | UnixLabels.EUNKNOWNERR int -> `Assoc ["EUNKNOWNERR", of_int int]


let (to_unix_label : Yojson.Basic.json -> UnixLabels.error) =
  function
  | `Assoc ["E2BIG",`Null] -> UnixLabels.E2BIG
  | `Assoc ["EACCES",`Null] -> UnixLabels.EACCES
  | `Assoc ["EAGAIN",`Null] -> UnixLabels.EAGAIN
  | `Assoc ["EBADF",`Null] -> UnixLabels.EBADF
  | `Assoc ["EBUSY",`Null] -> UnixLabels.EBUSY
  | `Assoc ["ECHILD",`Null] -> UnixLabels.ECHILD
  | `Assoc ["EDEADLK",`Null] -> UnixLabels.EDEADLK
  | `Assoc ["EDOM",`Null] -> UnixLabels.EDOM
  | `Assoc ["EEXIST",`Null] -> UnixLabels.EEXIST
  | `Assoc ["EFAULT",`Null] -> UnixLabels.EFAULT
  | `Assoc ["EFBIG",`Null] -> UnixLabels.EFBIG
  | `Assoc ["EINTR",`Null] -> UnixLabels.EINTR
  | `Assoc ["EINVAL",`Null] -> UnixLabels.EINVAL
  | `Assoc ["EIO",`Null] -> UnixLabels.EIO
  | `Assoc ["EISDIR",`Null] -> UnixLabels.EISDIR
  | `Assoc ["EMFILE",`Null] -> UnixLabels.EMFILE
  | `Assoc ["EMLINK",`Null] -> UnixLabels.EMLINK
  | `Assoc ["ENAMETOOLONG",`Null] -> UnixLabels.ENAMETOOLONG
  | `Assoc ["ENFILE",`Null] -> UnixLabels.ENFILE
  | `Assoc ["ENODEV",`Null] -> UnixLabels.ENODEV
  | `Assoc ["ENOENT",`Null] -> UnixLabels.ENOENT
  | `Assoc ["ENOEXEC",`Null] -> UnixLabels.ENOEXEC
  | `Assoc ["ENOLCK",`Null] -> UnixLabels.ENOLCK
  | `Assoc ["ENOMEM",`Null] -> UnixLabels.ENOMEM
  | `Assoc ["ENOSPC",`Null] -> UnixLabels.ENOSPC
  | `Assoc ["ENOSYS",`Null] -> UnixLabels.ENOSYS
  | `Assoc ["ENOTDIR",`Null] -> UnixLabels.ENOTDIR
  | `Assoc ["ENOTEMPTY",`Null] -> UnixLabels.ENOTEMPTY
  | `Assoc ["ENOTTY",`Null] -> UnixLabels.ENOTTY
  | `Assoc ["ENXIO",`Null] -> UnixLabels.ENXIO
  | `Assoc ["EPERM",`Null] -> UnixLabels.EPERM
  | `Assoc ["EPIPE",`Null] -> UnixLabels.EPIPE
  | `Assoc ["ERANGE",`Null] -> UnixLabels.ERANGE
  | `Assoc ["EROFS",`Null] -> UnixLabels.EROFS
  | `Assoc ["ESPIPE",`Null] -> UnixLabels.ESPIPE
  | `Assoc ["ESRCH",`Null] -> UnixLabels.ESRCH
  | `Assoc ["EXDEV",`Null] -> UnixLabels.EXDEV
  | `Assoc ["EWOULDBLOCK",`Null] -> UnixLabels.EWOULDBLOCK
  | `Assoc ["EINPROGRESS",`Null] -> UnixLabels.EINPROGRESS
  | `Assoc ["EALREADY",`Null] -> UnixLabels.EALREADY
  | `Assoc ["ENOTSOCK",`Null] -> UnixLabels.ENOTSOCK
  | `Assoc ["EDESTADDRREQ",`Null] -> UnixLabels.EDESTADDRREQ
  | `Assoc ["EMSGSIZE",`Null] -> UnixLabels.EMSGSIZE
  | `Assoc ["EPROTOTYPE",`Null] -> UnixLabels.EPROTOTYPE
  | `Assoc ["ENOPROTOOPT",`Null] -> UnixLabels.ENOPROTOOPT
  | `Assoc ["EPROTONOSUPPORT",`Null] -> UnixLabels.EPROTONOSUPPORT
  | `Assoc ["ESOCKTNOSUPPORT",`Null] -> UnixLabels.ESOCKTNOSUPPORT
  | `Assoc ["EOPNOTSUPP",`Null] -> UnixLabels.EOPNOTSUPP
  | `Assoc ["EPFNOSUPPORT",`Null] -> UnixLabels.EPFNOSUPPORT
  | `Assoc ["EAFNOSUPPORT",`Null] -> UnixLabels.EAFNOSUPPORT
  | `Assoc ["EADDRINUSE",`Null] -> UnixLabels.EADDRINUSE
  | `Assoc ["EADDRNOTAVAIL",`Null] -> UnixLabels.EADDRNOTAVAIL
  | `Assoc ["ENETDOWN",`Null] -> UnixLabels.ENETDOWN
  | `Assoc ["ENETUNREACH",`Null] -> UnixLabels.ENETUNREACH
  | `Assoc ["ENETRESET",`Null] -> UnixLabels.ENETRESET
  | `Assoc ["ECONNABORTED",`Null] -> UnixLabels.ECONNABORTED
  | `Assoc ["ECONNRESET",`Null] -> UnixLabels.ECONNRESET
  | `Assoc ["ENOBUFS",`Null] -> UnixLabels.ENOBUFS
  | `Assoc ["EISCONN",`Null] -> UnixLabels.EISCONN
  | `Assoc ["ENOTCONN",`Null] -> UnixLabels.ENOTCONN
  | `Assoc ["ESHUTDOWN",`Null] -> UnixLabels.ESHUTDOWN
  | `Assoc ["ETOOMANYREFS",`Null] -> UnixLabels.ETOOMANYREFS
  | `Assoc ["ETIMEDOUT",`Null] -> UnixLabels.ETIMEDOUT
  | `Assoc ["ECONNREFUSED",`Null] -> UnixLabels.ECONNREFUSED
  | `Assoc ["EHOSTDOWN",`Null] -> UnixLabels.EHOSTDOWN
  | `Assoc ["EHOSTUNREACH",`Null] -> UnixLabels.EHOSTUNREACH
  | `Assoc ["ELOOP",`Null] -> UnixLabels.ELOOP
  | `Assoc ["EOVERFLOW",`Null] -> UnixLabels.EOVERFLOW
  | `Assoc ["EUNKNOWNERR",int] -> UnixLabels.EUNKNOWNERR (to_int int)
  | x ->
    raise (Yojson.Basic.Util.Type_error (build_msg "unix labels error",x))

let of_unix_error =
    function
    | Unix.E2BIG -> `Assoc ["E2BIG",`Null]
    | Unix.EACCES -> `Assoc ["EACCES",`Null]
    | Unix.EAGAIN -> `Assoc ["EAGAIN",`Null]
    | Unix.EBADF -> `Assoc ["EBADF",`Null]
    | Unix.EBUSY -> `Assoc ["EBUSY",`Null]
    | Unix.ECHILD -> `Assoc ["ECHILD",`Null]
    | Unix.EDEADLK -> `Assoc ["EDEADLK",`Null]
    | Unix.EDOM -> `Assoc ["EDOM",`Null]
    | Unix.EEXIST -> `Assoc ["EEXIST",`Null]
    | Unix.EFAULT -> `Assoc ["EFAULT",`Null]
    | Unix.EFBIG -> `Assoc ["EFBIG",`Null]
    | Unix.EINTR -> `Assoc ["EINTR",`Null]
    | Unix.EINVAL -> `Assoc ["EINVAL",`Null]
    | Unix.EIO -> `Assoc ["EIO",`Null]
    | Unix.EISDIR -> `Assoc ["EISDIR",`Null]
    | Unix.EMFILE -> `Assoc ["EMFILE",`Null]
    | Unix.EMLINK -> `Assoc ["EMLINK",`Null]
    | Unix.ENAMETOOLONG -> `Assoc ["ENAMETOOLONG",`Null]
    | Unix.ENFILE -> `Assoc ["ENFILE",`Null]
    | Unix.ENODEV -> `Assoc ["ENODEV",`Null]
    | Unix.ENOENT -> `Assoc ["ENOENT",`Null]
    | Unix.ENOEXEC -> `Assoc ["ENOEXEC",`Null]
    | Unix.ENOLCK -> `Assoc ["ENOLCK",`Null]
    | Unix.ENOMEM -> `Assoc ["ENOMEM",`Null]
    | Unix.ENOSPC -> `Assoc ["ENOSPC",`Null]
    | Unix.ENOSYS -> `Assoc ["ENOSYS",`Null]
    | Unix.ENOTDIR -> `Assoc ["ENOTDIR",`Null]
    | Unix.ENOTEMPTY -> `Assoc ["ENOTEMPTY",`Null]
    | Unix.ENOTTY -> `Assoc ["ENOTTY",`Null]
    | Unix.ENXIO -> `Assoc ["ENXIO",`Null]
    | Unix.EPERM -> `Assoc ["EPERM",`Null]
    | Unix.EPIPE -> `Assoc ["EPIPE",`Null]
    | Unix.ERANGE -> `Assoc ["ERANGE",`Null]
    | Unix.EROFS -> `Assoc ["EROFS",`Null]
    | Unix.ESPIPE -> `Assoc ["ESPIPE",`Null]
    | Unix.ESRCH -> `Assoc ["ESRCH",`Null]
    | Unix.EXDEV -> `Assoc ["EXDEV",`Null]
    | Unix.EWOULDBLOCK -> `Assoc ["EWOULDBLOCK",`Null]
    | Unix.EINPROGRESS -> `Assoc ["EINPROGRESS",`Null]
    | Unix.EALREADY -> `Assoc ["EALREADY",`Null]
    | Unix.ENOTSOCK -> `Assoc ["ENOTSOCK",`Null]
    | Unix.EDESTADDRREQ -> `Assoc ["EDESTADDRREQ",`Null]
    | Unix.EMSGSIZE -> `Assoc ["EMSGSIZE",`Null]
    | Unix.EPROTOTYPE -> `Assoc ["EPROTOTYPE",`Null]
    | Unix.ENOPROTOOPT -> `Assoc ["ENOPROTOOPT",`Null]
    | Unix.EPROTONOSUPPORT -> `Assoc ["EPROTONOSUPPORT",`Null]
    | Unix.ESOCKTNOSUPPORT -> `Assoc ["ESOCKTNOSUPPORT",`Null]
    | Unix.EOPNOTSUPP -> `Assoc ["EOPNOTSUPP",`Null]
    | Unix.EPFNOSUPPORT -> `Assoc ["EPFNOSUPPORT",`Null]
    | Unix.EAFNOSUPPORT -> `Assoc ["EAFNOSUPPORT",`Null]
    | Unix.EADDRINUSE -> `Assoc ["EADDRINUSE",`Null]
    | Unix.EADDRNOTAVAIL -> `Assoc ["EADDRNOTAVAIL",`Null]
    | Unix.ENETDOWN -> `Assoc ["ENETDOWN",`Null]
    | Unix.ENETUNREACH -> `Assoc ["ENETUNREACH",`Null]
    | Unix.ENETRESET -> `Assoc ["ENETRESET",`Null]
    | Unix.ECONNABORTED -> `Assoc ["ECONNABORTED",`Null]
    | Unix.ECONNRESET -> `Assoc ["ECONNRESET",`Null]
    | Unix.ENOBUFS -> `Assoc ["ENOBUFS",`Null]
    | Unix.EISCONN -> `Assoc ["EISCONN",`Null]
    | Unix.ENOTCONN -> `Assoc ["ENOTCONN",`Null]
    | Unix.ESHUTDOWN -> `Assoc ["ESHUTDOWN",`Null]
    | Unix.ETOOMANYREFS -> `Assoc ["ETOOMANYREFS",`Null]
    | Unix.ETIMEDOUT -> `Assoc ["ETIMEDOUT",`Null]
    | Unix.ECONNREFUSED -> `Assoc ["ECONNREFUSED",`Null]
    | Unix.EHOSTDOWN -> `Assoc ["EHOSTDOWN",`Null]
    | Unix.EHOSTUNREACH -> `Assoc ["EHOSTUNREACH",`Null]
    | Unix.ELOOP -> `Assoc ["ELOOP",`Null]
    | Unix.EOVERFLOW -> `Assoc ["EOVERFLOW",`Null]
    | Unix.EUNKNOWNERR int -> `Assoc ["EUNKNOWNERR", of_int int]


let (to_unix_error : Yojson.Basic.json -> Unix.error) =
  function
  | `Assoc ["E2BIG",`Null] -> Unix.E2BIG
  | `Assoc ["EACCES",`Null] -> Unix.EACCES
  | `Assoc ["EAGAIN",`Null] -> Unix.EAGAIN
  | `Assoc ["EBADF",`Null] -> Unix.EBADF
  | `Assoc ["EBUSY",`Null] -> Unix.EBUSY
  | `Assoc ["ECHILD",`Null] -> Unix.ECHILD
  | `Assoc ["EDEADLK",`Null] -> Unix.EDEADLK
  | `Assoc ["EDOM",`Null] -> Unix.EDOM
  | `Assoc ["EEXIST",`Null] -> Unix.EEXIST
  | `Assoc ["EFAULT",`Null] -> Unix.EFAULT
  | `Assoc ["EFBIG",`Null] -> Unix.EFBIG
  | `Assoc ["EINTR",`Null] -> Unix.EINTR
  | `Assoc ["EINVAL",`Null] -> Unix.EINVAL
  | `Assoc ["EIO",`Null] -> Unix.EIO
  | `Assoc ["EISDIR",`Null] -> Unix.EISDIR
  | `Assoc ["EMFILE",`Null] -> Unix.EMFILE
  | `Assoc ["EMLINK",`Null] -> Unix.EMLINK
  | `Assoc ["ENAMETOOLONG",`Null] -> Unix.ENAMETOOLONG
  | `Assoc ["ENFILE",`Null] -> Unix.ENFILE
  | `Assoc ["ENODEV",`Null] -> Unix.ENODEV
  | `Assoc ["ENOENT",`Null] -> Unix.ENOENT
  | `Assoc ["ENOEXEC",`Null] -> Unix.ENOEXEC
  | `Assoc ["ENOLCK",`Null] -> Unix.ENOLCK
  | `Assoc ["ENOMEM",`Null] -> Unix.ENOMEM
  | `Assoc ["ENOSPC",`Null] -> Unix.ENOSPC
  | `Assoc ["ENOSYS",`Null] -> Unix.ENOSYS
  | `Assoc ["ENOTDIR",`Null] -> Unix.ENOTDIR
  | `Assoc ["ENOTEMPTY",`Null] -> Unix.ENOTEMPTY
  | `Assoc ["ENOTTY",`Null] -> Unix.ENOTTY
  | `Assoc ["ENXIO",`Null] -> Unix.ENXIO
  | `Assoc ["EPERM",`Null] -> Unix.EPERM
  | `Assoc ["EPIPE",`Null] -> Unix.EPIPE
  | `Assoc ["ERANGE",`Null] -> Unix.ERANGE
  | `Assoc ["EROFS",`Null] -> Unix.EROFS
  | `Assoc ["ESPIPE",`Null] -> Unix.ESPIPE
  | `Assoc ["ESRCH",`Null] -> Unix.ESRCH
  | `Assoc ["EXDEV",`Null] -> Unix.EXDEV
  | `Assoc ["EWOULDBLOCK",`Null] -> Unix.EWOULDBLOCK
  | `Assoc ["EINPROGRESS",`Null] -> Unix.EINPROGRESS
  | `Assoc ["EALREADY",`Null] -> Unix.EALREADY
  | `Assoc ["ENOTSOCK",`Null] -> Unix.ENOTSOCK
  | `Assoc ["EDESTADDRREQ",`Null] -> Unix.EDESTADDRREQ
  | `Assoc ["EMSGSIZE",`Null] -> Unix.EMSGSIZE
  | `Assoc ["EPROTOTYPE",`Null] -> Unix.EPROTOTYPE
  | `Assoc ["ENOPROTOOPT",`Null] -> Unix.ENOPROTOOPT
  | `Assoc ["EPROTONOSUPPORT",`Null] -> Unix.EPROTONOSUPPORT
  | `Assoc ["ESOCKTNOSUPPORT",`Null] -> Unix.ESOCKTNOSUPPORT
  | `Assoc ["EOPNOTSUPP",`Null] -> Unix.EOPNOTSUPP
  | `Assoc ["EPFNOSUPPORT",`Null] -> Unix.EPFNOSUPPORT
  | `Assoc ["EAFNOSUPPORT",`Null] -> Unix.EAFNOSUPPORT
  | `Assoc ["EADDRINUSE",`Null] -> Unix.EADDRINUSE
  | `Assoc ["EADDRNOTAVAIL",`Null] -> Unix.EADDRNOTAVAIL
  | `Assoc ["ENETDOWN",`Null] -> Unix.ENETDOWN
  | `Assoc ["ENETUNREACH",`Null] -> Unix.ENETUNREACH
  | `Assoc ["ENETRESET",`Null] -> Unix.ENETRESET
  | `Assoc ["ECONNABORTED",`Null] -> Unix.ECONNABORTED
  | `Assoc ["ECONNRESET",`Null] -> Unix.ECONNRESET
  | `Assoc ["ENOBUFS",`Null] -> Unix.ENOBUFS
  | `Assoc ["EISCONN",`Null] -> Unix.EISCONN
  | `Assoc ["ENOTCONN",`Null] -> Unix.ENOTCONN
  | `Assoc ["ESHUTDOWN",`Null] -> Unix.ESHUTDOWN
  | `Assoc ["ETOOMANYREFS",`Null] -> Unix.ETOOMANYREFS
  | `Assoc ["ETIMEDOUT",`Null] -> Unix.ETIMEDOUT
  | `Assoc ["ECONNREFUSED",`Null] -> Unix.ECONNREFUSED
  | `Assoc ["EHOSTDOWN",`Null] -> Unix.EHOSTDOWN
  | `Assoc ["EHOSTUNREACH",`Null] -> Unix.EHOSTUNREACH
  | `Assoc ["ELOOP",`Null] -> Unix.ELOOP
  | `Assoc ["EOVERFLOW",`Null] -> Unix.EOVERFLOW
  | `Assoc ["EUNKNOWNERR",int] -> Unix.EUNKNOWNERR (to_int int)
  | x ->
    raise (Yojson.Basic.Util.Type_error (build_msg "unix error",x))
