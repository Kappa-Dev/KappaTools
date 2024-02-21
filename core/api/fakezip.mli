(***********************************************************************)
(*                                                                     *)
(*                     (From) The CamlZip library                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Write naively but in pure OCaml uncompressed ZIP archives

    The ZIP file format used in this module is identical to that
    implemented by the popular [pkzip] archiver under Windows,
    and by the Info-ZIP [zip] and [unzip] commands under Unix and Windows.
    This format is also identical to the JAR file format used by Java. *)

(** {6 Information on ZIP entries} *)

(** Indicate whether the data in the entry is compressed or not. *)
type compression_method = Stored  (** data is stored without compression *)

(** {6 Writing to ZIP files} *)

type out_file
(** Abstract type representing a handle opened for writing to
              a ZIP file. *)

val open_out : ?comment:string -> string -> out_file
(** Create the structure representing a ZIP file. The argument
   (morally the filename) is useless.  The optional argument [comment]
   is a comment string that is attached to the ZIP file as a whole (as
   opposed to the comments that can be attached to individual ZIP
   entries). *)

val add_entry :
  string ->
  out_file ->
  ?extra:string ->
  ?comment:string ->
  ?level:int ->
  ?mtime:float ->
  string ->
  unit
(** [Zip.add_entry data zf name] adds a new entry to the 
              ZIP file [zf].  The data (file contents) associated with
              the entry is taken from the string [data].  It is compressed
              and written to the ZIP file [zf].  [name] is the file name
              stored along with this entry.  Several optional arguments
              can be provided to control the format and attached information 
              of the entry:
              @param extra  extra data attached to the entry (a string).
                Default: empty.
              @param comment  attached to the entry (a string).
                Default: empty.
              @param level  compression level for the entry. This is must be 0,
                with 0 meaning no compression (store as is).
                Default: 0.
              @param mtime  last modification time (in seconds since the
                epoch).
                Default: the current time. *)
(*
val copy_channel_to_entry:
  in_channel -> out_file -> 
    ?extra: string -> ?comment: string -> ?level: int ->
    ?mtime: float -> string -> unit
          (** Same as [Zip.add_entry], but the data associated with the
              entry is read from the input channel given as first argument.
              The channel is read up to end of file. *)
val copy_file_to_entry:
  string -> out_file -> 
    ?extra: string -> ?comment: string -> ?level: int ->
    ?mtime: float -> string -> unit
          (** Same as [Zip.add_entry], but the data associated with the
              entry is read from the file whose name is given as first
              argument.  Also, the default value for the [mtime]
              optional parameter is the time of last modification of the
              file. *)
*)

val add_entry_generator :
  out_file ->
  ?extra:string ->
  ?comment:string ->
  ?level:int ->
  ?mtime:float ->
  string ->
  (bytes -> int -> int -> unit) * (unit -> unit)
(** [Zip.add_entry_generator zf name] returns a pair of functions
              [(add, finish)].  It adds a new entry to the 
              ZIP file [zf].  The file name stored along with this entry
              is [name].  Initially, no data is stored in this entry.
              To store data in this entry, the program must repeatedly call
              the [add] function returned by [Zip.add_entry_generator].
              An invocation [add s ofs len] stores [len] characters of
              byte sequence [s] starting at offset [ofs] in the ZIP entry.
              When all the data forming the entry has been sent, the
              program must call the [finish] function returned by
              [Zip.add_entry_generator].  [finish] must be called exactly once.
              The optional arguments to [Zip.add_entry_generator]
              are as described in {!Zip.add_entry}. *)

val close_out : out_file -> Bigbuffer.bigstring
(** Finish writing the ZIP archive by adding the table of
              contents, and return its content. *)

(** {6 Error reporting} *)

exception Error of string * string * string
(** Exception raised when an ill-formed ZIP archive is encountered,
              or illegal parameters are given to the functions in this
              module.  The exception is of the form
              [Error(ZIP_name, entry_name, message)] where [ZIP_name]
              is the name of the ZIP file, [entry_name] the name of
              the offending entry, and [message] an explanation of the
              error. *)
