#!/usr/bin/env ocaml

#load "unix.cma"

let version =
  let chan = Unix.open_process_in "git describe --always --dirty" in
  try
    let out = input_line chan in
    if Unix.close_process_in chan = Unix.WEXITED 0 then
      out
    else
      "unkown"
  with End_of_file -> "unkown"

let () = Printf.printf "let t = \"%s\"\n" version
