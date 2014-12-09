 (**
    * counting_test.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 06/10/2010
    * Last modification: 18/01/2011
    * * 
    * Test suite for the counting engine 
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

module C = Counting_engine.Count(Counting_algebrae.Explicit_enumeration)
module D = Counting_engine.Count(Counting_algebrae.Counting)


let f parameters dual dual_and_self interface_of_brick init = 
  let _ = 
    C.count  
      parameters 
      Exception.empty_error_handler 
      {Counting_engine.print_hole=(fun log -> Printf.fprintf log "%d");
       Counting_engine.dual = dual ; 
       Counting_engine.dual_and_self = dual_and_self ;
       Counting_engine.interface_of_brick = interface_of_brick}
       C.print_handler 
      init
  in 
  let _ = 
    D.count  
      parameters 
      Exception.empty_error_handler 
      {Counting_engine.print_hole=(fun log -> Printf.fprintf log "%d");
       Counting_engine.dual = dual ; 
       Counting_engine.dual_and_self = dual_and_self ;
       Counting_engine.interface_of_brick = interface_of_brick}
      D.print_handler
      init
  in ()

let test_counting_procedure parameters = 
  let dual error x = 
    error,
    match x with 
     | 1 -> [2;3]
     | 2 -> [1]
     | 3 -> [1]
     | 4 -> [5]
     | 5 -> [4]
     | _ -> []
  in 
  let dual_and_self error x = 
    error,
    (match x with 
     | 1 -> [2;3]
     | 2 -> [1]
     | 3 -> [1]
     | 4 -> [5]
     | 5 -> [4]
     | _ -> []),
    match x with 
     | 6 -> true
     | _ -> false 
  in    
  let interface_of_brick error x = 
    error,
    match x with 
     | 0 -> [1]
     | 1 -> [2;3]
     | 2 -> []
     | 3 -> [4]
     | 4 -> [5;6]
     | _ -> []
  in    
  let init = [2,0;4,1;5,2;2,3;2,4] in 
  let _ = Printf.fprintf stdout "\n\nFirst test\n\n" in 
  let _ = f parameters dual dual_and_self interface_of_brick init in    
  
  let dual error x = 
    error,if x =1 then [2] else if x =2 then [1] else []
  in 
  let dual_and_self error x = 
    let error,y = dual error x in 
    error,y,false
  in 
  let interface_of_brick error x =
    error,if x = 1 then [2] else if x = 2 then [1;2] else []
  in  
  let init = [1,1;1,2] in 
  
  let _ = Printf.fprintf stdout "\n\nSecond test\n\n" in 
  let _ = f parameters dual dual_and_self interface_of_brick init in 
  ()
  
