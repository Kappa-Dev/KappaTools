val build_msg: string -> string

val string_to_json:
  string -> Yojson.Basic.json

val string_of_json:
  ?error_msg:string -> Yojson.Basic.json  -> string

val int_to_json: int -> Yojson.Basic.json

val int_of_json:
  ?error_msg:string -> Yojson.Basic.json  -> int

val list_to_json:
  ('a -> Yojson.Basic.json) ->
  'a list -> Yojson.Basic.json
val list_of_json:
  ?error_msg:string ->
  (Yojson.Basic.json -> 'a) ->
  Yojson.Basic.json -> 'a list

val assoc_to_json:
  ('a  -> string * Yojson.Basic.json) ->
  'a list -> Yojson.Basic.json

val assoc_of_json:
    ?error_msg:string ->
    (string * Yojson.Basic.json -> 'a)
    ->
     Yojson.Basic.json -> 'a list

val pair_to_json:
  ?lab1:string -> ?lab2:string ->
  ('a -> Yojson.Basic.json) ->
  ('b -> Yojson.Basic.json) ->
  ('a * 'b) -> Yojson.Basic.json

val pair_of_json:
  ?lab1:string -> ?lab2:string -> ?error_msg:string ->
  (Yojson.Basic.json -> 'a) ->
  (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> 'a * 'b

val map_of_json:
  ?lab_key:string -> ?lab_value:string -> ?error_msg:string ->
  ('key -> 'value -> 'map -> 'map) ->
  'map ->
  (Yojson.Basic.json -> 'key) ->
  (Yojson.Basic.json -> 'value) ->
  Yojson.Basic.json -> 'map

val map_to_json:
  ?lab_key:string -> ?lab_value:string ->
  (('key -> 'value -> Yojson.Basic.json list -> Yojson.Basic.json list) -> 'map -> Yojson.Basic.json list -> Yojson.Basic.json list) ->
  ('key -> Yojson.Basic.json) ->
  ('value -> Yojson.Basic.json) ->
  'map -> Yojson.Basic.json
