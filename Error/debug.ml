let tag s = print_string (s^"\n") ; flush stdout

let tag_if_debug s =
  if !Parameter.debugModeOn
  then Printf.kfprintf (fun out -> Printf.fprintf out "\n%!") stdout s
  else Printf.ifprintf stdout s
