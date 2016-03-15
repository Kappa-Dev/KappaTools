#!/usr/bin/env ocaml
#use "topfind";;
#require "str";;
open Printf;;
open Str;;

(* http://www.codecodex.com/wiki/Remove_non-letters_from_a_string#OCaml *)
let strip_nonalpha = Str.global_replace (Str.regexp "[^a-zA-Z]+") "_";;
(* https://www.rosettacode.org/wiki/Read_entire_file#OCaml *)
let read_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = String.create n in
        really_input ic s 0 n;
        close_in ic;
        (s)

let () =
for i = 1 to Array.length Sys.argv - 1 do
    let file_name : string = Sys.argv.(i) in
    let file_content : string = String.escaped (read_file file_name) in
    let variable_name : string = (strip_nonalpha (Filename.basename file_name)) in
    printf "let %s = \"%s\";;\n" variable_name file_content
done;;
