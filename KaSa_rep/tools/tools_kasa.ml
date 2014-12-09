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
    
  
