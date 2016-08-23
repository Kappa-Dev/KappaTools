let build_msg s = "Not a correct "^s
let string_to_json (s:string) = `String s

let string_of_json ?error_msg:(error_msg=build_msg "string") =
  function
  | `String (s:string) -> s
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (error_msg,x))

let int_to_json (s:int) = `Int s

let int_of_json ?error_msg:(error_msg=build_msg "int") =
  function
  | `Int (s:int) -> s
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (error_msg,x))

let list_to_json to_json l =
  `List
    (List.rev_map to_json (List.rev l))

let list_of_json
    ?error_msg:(error_msg=build_msg "list") of_json =
  function
  | `List l as x ->
    begin
      try
        List.rev_map of_json (List.rev l)
      with Not_found ->
        raise
          (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let pair_to_json ?lab1:(lab1="first") ?lab2:(lab2="second") to_json1 to_json2 (a,b) =
  `Assoc
    [
      lab1, to_json1 a;
      lab2, to_json2 b
    ]

let pair_of_json ?lab1:(lab1="first") ?lab2:(lab2="second") ?error_msg:(error_msg=build_msg "pair") of_json1 of_json2 =
  function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        of_json1 (List.assoc lab1 l),
        of_json2 (List.assoc lab2 l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))




let map_to_json
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    fold key_to_json value_to_json map =
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


let map_of_json
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    ?error_msg:(error_msg=build_msg "map")
    add empty json_to_key json_to_value =
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
