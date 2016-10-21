(** Jsonify simple types *)

val build_msg: string -> string

val of_string: string -> Yojson.Basic.json
val to_string: ?error_msg:string -> Yojson.Basic.json  -> string

val of_int: int -> Yojson.Basic.json
val to_int: ?error_msg:string -> Yojson.Basic.json  -> int

val of_option: ('a -> Yojson.Basic.json) -> 'a option -> Yojson.Basic.json

val to_option: (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a option
(** Beware: `Null is reserved for None *)

val of_list: ('a -> Yojson.Basic.json) -> 'a list -> Yojson.Basic.json

val to_list:
  ?error_msg:string -> (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a list

val of_assoc:
  ('a  -> string * Yojson.Basic.json) -> 'a list -> Yojson.Basic.json

val to_assoc:
  ?error_msg:string -> (string * Yojson.Basic.json -> 'a) ->
  Yojson.Basic.json -> 'a list

val of_pair:
  ?lab1:string -> ?lab2:string ->
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a * 'b) -> Yojson.Basic.json

val to_pair:
  ?lab1:string -> ?lab2:string -> ?error_msg:string ->
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> 'a * 'b

val of_map:
  ?lab_key:string -> ?lab_value:string ->
  fold:(('key -> 'value -> Yojson.Basic.json list -> Yojson.Basic.json list) ->
   'map -> Yojson.Basic.json list -> Yojson.Basic.json list) ->
  ('key -> Yojson.Basic.json) -> ('value -> Yojson.Basic.json) ->
  'map -> Yojson.Basic.json

val to_map:
  ?lab_key:string -> ?lab_value:string -> ?error_msg:string ->
  add:('key -> 'value -> 'map -> 'map) ->
  empty:'map ->
  (Yojson.Basic.json -> 'key) ->
  (Yojson.Basic.json -> 'value) ->
  Yojson.Basic.json -> 'map
