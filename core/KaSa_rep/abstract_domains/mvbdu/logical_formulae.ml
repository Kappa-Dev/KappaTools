type 'a formula = 
  | P of 'a 
  | NOT of 'a formula 
  | IMPLY of 'a formula * 'a formula 
  | AND of 'a formula * 'a formula 
  | OR of 'a formula * 'a formula 
  | False 
  | True 

let op x y = 
  x = NOT y || y = NOT x 

let rec simplify f = 
  match f with
  | NOT x -> 
    let x = simplify x in 
    begin 
      match x with 
        | NOT x -> x 
        | True -> False 
        | False -> True 
        | IMPLY _ | OR _ | AND _ | P _ -> NOT x 
    end
  | IMPLY (x, y) -> simplify (OR(NOT x,y))
  | OR (x,y) -> 
    let x = simplify x in 
    let y = simplify y in 
    begin
      match x,y with 
      | True, _ | _, True -> True 
      | False, x| x, False -> x 
      | x, AND(y,z)  when op x y -> OR(x,z)
      | AND(y,z), x  when op x y -> OR(x,z)
      | x, AND(z,y)  when op x y -> OR(x,z)
      | AND(z,y), x when op x y -> OR(x,z)
    
      | (OR _ | AND _ | NOT _ | IMPLY _ | P _),
      (OR _ | AND _ | NOT _ | IMPLY _ | P _) -> OR (x,y) 
    end 
    | AND (x,y) -> 
      let x = simplify x in 
      let y = simplify y in 
      begin
        match x,y with 
        | False, _ | _, False -> False
        | True, x| x, True -> x 
        | x, OR(y,z)  when op x y -> AND(x,z)
        | OR(y,z), x  when op x y -> AND(x,z)
        | x, OR(z,y)  when op x y -> AND(x,z)
        | OR(z,y), x when op x y -> AND(x,z)
        | (OR _ | AND _ | NOT _ | IMPLY _ | P _),
        (OR _ | AND _ | NOT _ | IMPLY _ | P _) -> AND (x,y) 
      end 
    | True | False | P _ -> f



let rec print parameter error string_of formula = 
  match formula with 
  | P x -> 
    let error, s = string_of error x in 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" s 
    in error    
  | NOT x -> 
   let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "~" in 
   print_arg parameter error string_of x 
  | OR (x1,x2) -> 
    let error = print_arg parameter error string_of x1 in 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ".or." in 
    print_arg parameter error string_of x2  
  | AND (x1,x2) -> 
    let error = print_arg parameter error string_of x1 in 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ".and." in 
    print_arg parameter error string_of x2  
  | IMPLY (x1,x2) -> 
    let error = print_arg parameter error string_of x1 in 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "=>" in 
    print_arg parameter error string_of x2  
  | True -> 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ".T." in error 
  | False -> let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ".F." in error 
and print_arg parameter error string_of formula  = 
  match formula with 
  | P _ | True | False  -> print parameter error string_of formula 
  | NOT _ | OR (_,_) | AND (_,_) | IMPLY (_,_) -> 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "(" in 
    let error = print parameter error string_of formula in 
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ")" in error 

    let print p e s f = print p e s (simplify f)