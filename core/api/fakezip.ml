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

(* Write unefficiently but in pure OCaml uncompressed ZIP archives *)

exception Error of string * string * string

let write1 oc n = Bigbuffer.add_char oc (Char.unsafe_chr n)

let write2 oc n =
  write1 oc n;
  write1 oc (n lsr 8)

let write4 oc n =
  write2 oc (Int32.to_int n);
  write2 oc (Int32.to_int (Int32.shift_right_logical n 16))

let write4_int oc n =
  write2 oc n;
  write2 oc (n lsr 16)

let writestring oc s = Bigbuffer.add_string oc s

type compression_method = Stored

type entry = {
  filename: string;
  extra: string;
  comment: string;
  methd: compression_method;
  mtime: float;
  crc: int32;
  uncompressed_size: int;
  compressed_size: int;
  (*is_directory: bool;*)
  file_offset: int32;
}

type out_file = {
  of_filename: string;
  of_channel: Bigbuffer.t;
  mutable of_entries: entry list;
  of_comment: string;
}

(*let filename_is_directory name =
  String.length name > 0 && name.[String.length name - 1] = '/'*)

(* Convert between Unix dates and DOS dates *)

let dostime_of_unixtime t =
  let tm = Unix.localtime t in
  ( (tm.Unix.tm_sec lsr 1) + (tm.Unix.tm_min lsl 5) + (tm.Unix.tm_hour lsl 11),
    tm.Unix.tm_mday
    + ((tm.Unix.tm_mon + 1) lsl 5)
    + ((tm.Unix.tm_year - 80) lsl 9) )

(* Open a ZIP file for writing *)

let open_out ?(comment = "") filename =
  if String.length comment >= 0x10000 then
    raise (Error (filename, "", "comment too long"));
  {
    of_filename = filename;
    of_channel = Bigbuffer.create 8192;
    of_entries = [];
    of_comment = comment;
  }

(* Close a ZIP file for writing.  Add central directory. *)

let write_directory_entry oc e =
  write4 oc (Int32.of_int 0x02014b50);
  (* signature *)
  let version =
    match e.methd with
    | Stored -> 10
  in
  write2 oc version;
  (* version made by *)
  write2 oc version;
  (* version needed to extract *)
  write2 oc 8;
  (* flags *)
  write2 oc
    (match e.methd with
    | Stored -> 0);
  (* method *)
  let time, date = dostime_of_unixtime e.mtime in
  write2 oc time;
  (* last mod time *)
  write2 oc date;
  (* last mod date *)
  write4 oc e.crc;
  (* CRC32 *)
  write4_int oc e.compressed_size;
  (* compressed size *)
  write4_int oc e.uncompressed_size;
  (* uncompressed size *)
  write2 oc (String.length e.filename);
  (* filename length *)
  write2 oc (String.length e.extra);
  (* extra length *)
  write2 oc (String.length e.comment);
  (* comment length *)
  write2 oc 0;
  (* disk number start *)
  write2 oc 0;
  (* internal attributes *)
  write4_int oc 0;
  (* external attributes *)
  write4 oc e.file_offset;
  (* offset of local header *)
  writestring oc e.filename;
  (* filename *)
  writestring oc e.extra;
  (* extra info *)
  writestring oc e.comment (* file comment *)

let close_out ofile =
  let oc = ofile.of_channel in
  let start_cd = Bigbuffer.length oc in
  List.iter (write_directory_entry oc) (List.rev ofile.of_entries);
  let cd_size = Bigbuffer.length oc - start_cd in
  let num_entries = List.length ofile.of_entries in
  if num_entries >= 0x10000 then
    raise (Error (ofile.of_filename, "", "too many entries"));
  write4 oc (Int32.of_int 0x06054b50);
  (* signature *)
  write2 oc 0;
  (* disk number *)
  write2 oc 0;
  (* number of disk with central dir *)
  write2 oc num_entries;
  (* # entries in this disk *)
  write2 oc num_entries;
  (* # entries in central dir *)
  write4_int oc cd_size;
  (* size of central dir *)
  write4_int oc start_cd;
  (* offset of central dir *)
  write2 oc (String.length ofile.of_comment);
  (* length of comment *)
  writestring oc ofile.of_comment;
  (* comment *)
  Bigbuffer.contents oc

(* Write a local file header and return the corresponding entry *)

let add_entry_header ofile extra comment level mtime filename =
  if level <> 0 then
    raise (Error (ofile.of_filename, filename, "fake_zip cannot compress"));
  if String.length filename >= 0x10000 then
    raise (Error (ofile.of_filename, filename, "filename too long"));
  if String.length extra >= 0x10000 then
    raise (Error (ofile.of_filename, filename, "extra data too long"));
  if String.length comment >= 0x10000 then
    raise (Error (ofile.of_filename, filename, "comment too long"));
  let oc = ofile.of_channel in
  let pos = Bigbuffer.length oc in
  write4 oc (Int32.of_int 0x04034b50);
  (* signature *)
  let version =
    if level = 0 then
      10
    else
      20
  in
  write2 oc version;
  (* version needed to extract *)
  write2 oc 8;
  (* flags *)
  write2 oc
    (if level = 0 then
       0
     else
       8);
  (* method *)
  let time, date = dostime_of_unixtime mtime in
  write2 oc time;
  (* last mod time *)
  write2 oc date;
  (* last mod date *)
  write4 oc Int32.zero;
  (* CRC32 - to be filled later *)
  write4_int oc 0;
  (* compressed size - later *)
  write4_int oc 0;
  (* uncompressed size - later *)
  write2 oc (String.length filename);
  (* filename length *)
  write2 oc (String.length extra);
  (* extra length *)
  writestring oc filename;
  (* filename *)
  writestring oc extra;
  (* extra info *)
  {
    filename;
    extra;
    comment;
    methd = Stored;
    mtime;
    crc = Int32.zero;
    uncompressed_size = 0;
    compressed_size = 0;
    (*is_directory = filename_is_directory filename;*)
    file_offset = Int32.of_int pos;
  }

(* Write a data descriptor and update the entry *)

let add_data_descriptor ofile crc compr_size uncompr_size entry =
  let oc = ofile.of_channel in
  write4 oc (Int32.of_int 0x08074b50);
  (* signature *)
  write4 oc crc;
  (* CRC *)
  write4_int oc compr_size;
  (* compressed size *)
  write4_int oc uncompr_size;
  (* uncompressed size *)
  {
    entry with
    crc;
    uncompressed_size = uncompr_size;
    compressed_size = compr_size;
  }

let update_crc crc buf start len = Crc32.string ~crc buf start len

(* Add an entry with the contents of a string *)

let add_entry data ofile ?(extra = "") ?(comment = "") ?(level = 0)
    ?(mtime = Unix.time ()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = update_crc Int32.zero data 0 (String.length data) in
  let compr_size =
    match level with
    | 0 ->
      Bigbuffer.add_substring ofile.of_channel data 0 (String.length data);
      String.length data
    | _ -> raise (Error (ofile.of_filename, name, "compression error"))
  in
  let e' = add_data_descriptor ofile crc compr_size (String.length data) e in
  ofile.of_entries <- e' :: ofile.of_entries

(*
(* Add an entry with the contents of an in channel *)

let copy_channel_to_entry ic ofile ?(extra = "") ?(comment = "")
                                   ?(level = 0) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = ref Int32.zero in
  let (compr_size, uncompr_size) =
    match level with
      0 ->
        let buf = Bytes.create 4096 in
        let rec copy sz =
          let r = input ic buf 0 (Bytes.length buf) in
          if r = 0 then sz else begin
            crc := update_crc !crc buf 0 r;
            BitBuffer.add_substring ofile.of_channel buf 0 r;
            copy (sz + r)
          end in
        let size = copy 0 in
        (size, size)
    | _ -> raise (Error(ofile.of_filename, name, "compression error")) in
  let e' = add_data_descriptor ofile !crc compr_size uncompr_size e in
  ofile.of_entries <- e' :: ofile.of_entries

(* Add an entry with the contents of a file *)

let copy_file_to_entry infilename ofile ?(extra = "") ?(comment = "")
                                        ?(level = 0) ?mtime name =
  let ic = open_in_bin infilename in
  let mtime' =
    match mtime with
      Some t -> mtime
    | None ->
        try Some((Unix.stat infilename).Unix.st_mtime)
        with Unix.Unix_error(_,_,_) -> None in
  try
    copy_channel_to_entry ic ofile ~extra ~comment ~level ?mtime:mtime' name;
    Pervasives.close_in ic
  with x ->
    Pervasives.close_in ic; raise x
*)

(* Add an entry whose content will be produced by the caller *)

let add_entry_generator ofile ?(extra = "") ?(comment = "") ?(level = 0)
    ?(mtime = Unix.time ()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = ref Int32.zero in
  let compr_size = ref 0 in
  let uncompr_size = ref 0 in
  let finished = ref false in
  let check () =
    if !finished then
      raise (Error (ofile.of_filename, name, "entry already finished"))
  in
  let finish () =
    finished := true;
    let e' = add_data_descriptor ofile !crc !compr_size !uncompr_size e in
    ofile.of_entries <- e' :: ofile.of_entries
  in
  match level with
  | 0 ->
    ( (fun buf pos len ->
        check ();
        Bigbuffer.add_subbytes ofile.of_channel buf pos len;
        compr_size := !compr_size + len;
        uncompr_size := !uncompr_size + len),
      fun () ->
        check ();
        finish () )
  | _ -> raise (Error (ofile.of_filename, name, "compression error"))
