type position = int

type level =
  | Normal (* always shown & accepted *)
  | Expert (* shown in expert or developper mode, always accepted *)
  | Developper (* only shown & accepted in developper mode *)
  | Hidden (* never shown *)

type category = string * position * level option
type key = string
type msg = string

type spec =
  | Void (* To skip a line *)
  | Bool of bool ref (* Sets a boolean value *)
  | Int of int ref (* Sets an integer value *)
  | Int_opt of int option ref (* Sets an optional integer value *)
  | String of string ref (* Sets a string value *)
  | String_opt of string option ref (* Sets an optional string value *)
  | String_list of string list ref (* Sets a list of strings *)
  | StringNbr_list of
      (string * string) list ref (* Sets a list of pairs of strings *)
  | Float of float ref (* Sets a float value *)
  | Float_opt of float option ref (* Sets an optional  float value *)
  (* one or several options among a list *)
  | Choice of (key * msg) list * key list * key ref
  | Choice_list of (key * msg) list * key list ref
  (* meta-options to set several options at once:
     - 1st option list is appended as-is when the option is used
     - when 2-st list is not empty, the meta option takes an argument that
     is passed to each option in the list
     - the keys in both lists must appear as regular options as well!
  *)
  | Multi of key list * key list
    (* meta-options, take a list of (k,ext):key*string and give to the option k the parameter s^ext *)
  | MultiExt of (key * string) list

type t = (key * spec * msg * (category * position) list * level) list

val accept_level_display : level -> bool
val check : t -> unit
val parse_list : with_tk:bool -> ?title:msg -> t -> msg list -> msg list

module StringMap : SetMap.Map with type elt = string
module StringIntSet : SetMap.Set with type elt = string * int * level option
module StringIntMap : SetMap.Map with type elt = string * int * level option

val max_level_opt : level -> level option -> level
val show_level : level -> bool
val expert_mode : bool ref
val nokey : msg -> msg

val order :
  t ->
  ((msg * spec * msg * (StringIntSet.elt * position) list * level) list * level)
  StringIntMap.t

val cut_list : msg -> msg list
