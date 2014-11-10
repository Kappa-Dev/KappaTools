type t = char list

let empty:t = []

let is_empty ls = match ls with [] -> true | _ -> false

let concat ?sep str ls =
  let pls = ref ls in
  let () =
    match sep with
      None -> ()
    | Some c -> if not (is_empty ls) then pls:=(c::!pls)
  in
  let () = String.iter (fun c -> pls:=(c::!pls)) str in
  !pls

let print ls = List.fold_left (fun _ c -> print_char c) () (List.rev ls)

let printf ?(no_reverse=false) d ls =
  Pp.list Pp.empty Format.pp_print_char
	  d (if no_reverse then List.rev ls else ls)

let to_string ls = (*costly but just to check!*)
    List.fold_left (fun str c -> Format.sprintf "%c%s" c str) "" ls
