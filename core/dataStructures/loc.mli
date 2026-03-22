(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Module for type Loc.t annotating structured data with the line range
 * in a file which was used to define it *)

type position = { chr: int; line: int }
type t = { file: string; from_position: position; to_position: position }
type 'a annoted = 'a * t

val v : 'a annoted -> 'a
(** Extract value from Loc.annoted *)

val get_annot : 'a annoted -> t
(** Extract annotation from Loc.annoted *)

val copy_annot : 'b annoted -> 'a -> 'a annoted
(** Create annoted variable with same annotation as existing variable *)

val map_annot : ('a -> 'b) -> 'a annoted -> 'b annoted
(** Apply operation on variable and keep annotation *)

val of_pos : Lexing.position -> Lexing.position -> t
val dummy : t
val annot_with_dummy : 'a -> 'a annoted
val is_annoted_with_dummy : 'a annoted -> bool

val merge : t -> t -> t
(** [merge b e] creates the range from beginning of [b] to the end of [e]
 (filename must match) *)

val is_included_in : string -> position -> t -> bool

(** {2 I/O} *)

val to_string : t -> string
val print : Format.formatter -> t -> unit

val print_annoted :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a annoted -> unit

val annoted_of_yojson :
  ?filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  Yojson.Basic.t ->
  'a annoted

val yojson_of_annoted :
  ?filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  'a annoted ->
  Yojson.Basic.t

val write_position : Buffer.t -> position -> unit
val read_position : Yojson.Safe.lexer_state -> Lexing.lexbuf -> position

val write_range : Buffer.t -> t -> unit
(** Output a JSON value of type {!t}. *)

val string_of_range : ?len:int -> t -> string
(** Serialize a value of type {!t} into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_range : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
(** Input JSON data of type {!t}. *)

val range_of_string : string -> t
(** Deserialize JSON data of type {!t}. *)

(** Annoted yojson helpers *)

val string_annoted_to_json :
  filenames:int Mods.StringMap.t -> string annoted -> Yojson.Basic.t

val string_annoted_of_json :
  filenames:string array -> Yojson.Basic.t -> string annoted

val string_option_annoted_to_json :
  filenames:int Mods.StringMap.t -> string option annoted -> Yojson.Basic.t

val string_option_annoted_of_json :
  filenames:string array -> Yojson.Basic.t -> string option annoted

type 'a rename_pos = (t -> t option)-> 'a -> 'a 

val rename_loc: t rename_pos 
val rename_pos: 'a rename_pos -> 'a annoted rename_pos 
val rename_pos_flat: 'a annoted rename_pos 
val rename_pos_opt: 'a rename_pos -> 'a option rename_pos 
val rename_pos_pair: 'a rename_pos -> 'b rename_pos -> ('a*'b) rename_pos 
val rename_pos_list: ((t -> t option) -> 'a -> 'a) -> (t -> t option) ->  'a list -> 'a list 


type ('parameters,'errors,'a) rename_pos_with_errors = 'parameters -> 'errors -> (t -> t option)-> 'a -> 'errors * 'a 


val rename_pos_with_errors: 
   ('parameters,'errors,'a) rename_pos_with_errors -> ('parameters,'errors,'a annoted) rename_pos_with_errors 
val rename_pos_opt_with_errors:  ('parameters,'errors,'a) rename_pos_with_errors -> ('parameters,'errors,'a option) rename_pos_with_errors 
val rename_pos_list_with_errors: ('parameters,'errors,'a) rename_pos_with_errors -> ('parameters,'errors,'a list) rename_pos_with_errors 
val rename_pos_flat_with_errors: ('parameters,'errors,'a annoted) rename_pos_with_errors
val rename_pos_pair_with_errors: ('parameters,'errors,'a) rename_pos_with_errors ->('parameters,'errors,'b) rename_pos_with_errors -> ('parameters,'errors,'a*'b) rename_pos_with_errors 

type 'a diff_pos = ('a -> 'a -> (t*t) list -> (t*t) list)
val diff_pos: t diff_pos 
val diff_pos_annoted: 'a diff_pos -> 'a annoted diff_pos 
val diff_pos_flat: 'a diff_pos 
val diff_pos_opt: 'a diff_pos -> 'a option diff_pos 
val diff_pos_list: 'a diff_pos -> 'a list diff_pos 
val diff_pos_pair: 'a diff_pos -> 'b diff_pos -> ('a*'b) diff_pos  
val diff_pos_empty: (t*t) list 

val fun_of_list: (t*t) list -> (t -> t option)