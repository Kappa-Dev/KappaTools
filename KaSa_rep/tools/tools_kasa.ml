let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Tools") message exn (fun () -> default) 

let fst_option x = 
  match x 
  with 
  | Some (x,_) -> Some x 
  | _ -> None 

let snd_option x = 
  match x 
  with 
  | Some (_,x) -> Some x 
  | _ -> None 
   

(** dot output*)
let escape_label_in_dot s =
  Str.global_substitute (Str.regexp "[\"\\]") (function
  | "\"" -> "\\\""
  | "\\" -> "\\\\"
  | x -> x) s
 
let make_id_compatible_with_dot_format parameters error string =
  let tab = 
    Remanent_parameters.get_make_labels_compatible_with_dot parameters 
  in 
  let rec aux pos l = 
    if pos<0 
    then l 
    else
      let char = String.get string pos in 
      try 
	let liste_char = 
	  Remanent_parameters_sig.CharMap.find char tab 
	in 
	aux (pos-1) 
	  begin 
	    List.fold_left 
	      (fun list char -> char::list)
	      l 
	      (List.rev liste_char)
	  end 
      with 
      | Not_found -> 
	aux (pos-1) (char::l) 
  in 
  let l = aux (String.length string -1) [] in 
  error,
  String.concat "" (List.rev_map (String.make 1) (List.rev l)) (*I fear that this is very costly, but if I am not allowed to use OCaml 4.02*)

