type 'a formula = 
  | P of 'a 
  | NOT of 'a formula 
  | IMPLY of 'a formula * 'a formula 
  | AND of 'a formula * 'a formula 
  | OR of 'a formula * 'a formula 
  | False 
  | True 

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

