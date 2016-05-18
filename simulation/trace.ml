type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (* the agents *)
  | PERT of string (* the rule *)

let print_event_kind ?env f x =
  match env with
  | None ->
     (match x with
      | OBS i -> Format.fprintf f "OBS(%s)" i
      | RULE i -> Format.fprintf f "RULE(%i)" i
      | INIT l ->
	 Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
      | PERT s -> Format.fprintf f "PERT(%s)" s)
  | Some env ->
     match x with
     | OBS name -> Format.pp_print_string f name
     | PERT s -> Format.pp_print_string f s
     | RULE r_id -> Environment.print_ast_rule ~env f r_id
     | INIT s ->
	Format.fprintf
	  f "Intro @[<h>%a@]"
	  (Pp.list Pp.comma (Environment.print_agent ~env)) s

let print_event_kind_dot_annot env f = function
  | RULE r_id  ->
     Format.fprintf
       f "[label=\"%a\", shape=%s, style=%s, fillcolor = %s]"
       (Environment.print_ast_rule ~env) r_id "invhouse" "filled" "lightblue"
  | OBS name  ->
     Format.fprintf
       f "[label=\"%s\", style=filled, fillcolor=red]" name
  | INIT s ->
     Format.fprintf
       f "[label=\"Intro @[<h>%a@]\", shape=%s, style=%s, fillcolor=green]"
       (Pp.list Pp.comma (Environment.print_agent ~env)) s "house" "filled"
  | PERT s ->
     Format.fprintf
       f "[label=\"%s\", shape=%s, style=%s, fillcolor = %s]"
       s "invhouse" "filled" "green"
