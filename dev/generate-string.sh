#!/usr/bin/env ocaml
open Printf
let alpha = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'l';'j';'k';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
             'A';'B';'C';'D';'E';'F';'G';'H';'I';'L';'J';'K';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';
             '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']

let strip_nonalpha =
  String.map (fun c -> if List.mem c alpha then (Char.lowercase_ascii c) else '_')

(* https://www.rosettacode.org/wiki/Read_entire_file#OCaml *)
let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  let () = close_in ic in
  s

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let file_name : string = Sys.argv.(i) in
    let file_content = String.escaped (read_file file_name) in
    let variable_name = (strip_nonalpha (Filename.basename file_name)) in
    printf "let %s = \"%s\";;\n" variable_name file_content
  done
