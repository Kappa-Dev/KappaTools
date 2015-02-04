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
    
let make_id_compatible_with_dot_format parameters error string = 
  let size_init = String.length string in 
  let list,size = 
    let rec aux k list size = 
      if k=(-1) then list,size 
      else 
	let char = String.get string k in 
	let list,size = 
	  try 
	    let list' = 
	      Remanent_parameters_sig.CharMap.find char parameters.Remanent_parameters_sig.make_id_compatible_with_dot
	    in 
	    let size' = size + (List.length list') -1 in 
	    List.fold_left 
	      (fun l a -> a::l)
	      list 
	      (List.rev list'),size'
	  with 
	  | Not_found -> 
	    char::list,size
	in
	aux (k-1) list size 
    in 
    aux (size_init - 1) [] size_init 
  in 
  let liste_ref = ref list in 
  let error_ref = ref error in 
  let string' = 
    String.init size 
      (fun _ -> 
	let head,tail = 
	  match 
	    !liste_ref 
	  with 
	  | [] -> 
	    let error,(head,tail) = warn parameters (!error_ref) (Some "line 46") Exit ('0',[])
	    in 
	    let _ = error_ref:=error in
	    head,tail
	  | head::tail -> head,tail
	in 
	let _ = liste_ref:=tail in 
	head)
  in 
  let error,() = 
    match 
      !liste_ref 
    with 
    | []  -> error,()
    | t::q -> warn parameters error (Some "line 60") Exit ()
  in 
  error,string' 
