 (**
  * influence_labels.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: March, the 7th 2011
  * Last modification: March, the 23rd 2011
  * * 
  * Labels to be associated with influence relations
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Influence_labels") message exn (fun () -> default) 

module type Labels = 
sig
  type label 
  
  val label_of_int: Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> Exception.method_handler * label 
  val to_string:Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> Exception.method_handler  * string
  val dump:   Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> Exception.method_handler
    
end

module Int_labels = 
(struct 
  type label = int  
   
  let label_of_int _ error i = error,i  
                  
  let to_string parameter error i = 
     error,string_of_int i
      
  let dump h error i = 
    let error,s = to_string h error i  in 
    let _ = Printf.fprintf h.Remanent_parameters_sig.log "%s" s in 
      error
    
end:Labels)

module type Label_handler = 
sig
  type label 
  type label_set 
  type label_set_couple

  val label_of_int: Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> Exception.method_handler * label  
  val empty: label_set
  val empty_couple: label_set_couple   
  val add_set:     Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> label_set -> Exception.method_handler * label_set 
  val add_couple:  Remanent_parameters_sig.parameters -> Exception.method_handler -> label_set -> label_set -> label_set_couple -> Exception.method_handler * label_set_couple
  val dump:        Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler -> label_set  -> Exception.method_handler 
  val dump_couple: Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler ->label_set_couple -> Exception.method_handler 
  val to_string :  Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler ->label_set -> Exception.method_handler * string list 
  val to_string_couple : Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler ->label_set_couple  -> Exception.method_handler * string list 
end
    
module Empty =  
(struct 
  type label = unit 
  type label_set = unit
  type label_set_couple = unit 
    
  let label_of_int handler error _ = error,() 
  let empty = () 
  let empty_couple = ()
  let add_set _ error _ _ = error,() 
  let add_couple _ error _ _ _ = error,() 
  let dump _ error _ _ = error 
  let to_string _ error _ _ = error,[]
  let dump_couple _ error _ _ = error 
  let to_string_couple _ error _ _ = error,[]
end:Label_handler with type label=unit)

module Extensive = 
  (functor (L:Labels) -> 
    (struct 
      type label = L.label 
      module Set = Set_and_map.Make (struct type t=label let compare = compare end)
      type label_set = Set.set
      module Pair_Set = Set_and_map.Make (struct type t=label*label let compare = compare end)
      type label_set_couple =  Pair_Set.set

      let label_of_int = L.label_of_int
      let empty = Set.empty_set   
      let empty_couple = Pair_Set.empty_set  
      let add_set = Set.add_set
      let add_couple remanent error a b sol = 
        Set.fold_set 
          (fun a (error,sol) -> 
            Set.fold_set
              (fun b (error,sol) -> 
                Pair_Set.add_set remanent error (a,b) sol
              )  
              b 
              (error,sol)
          ) 
          a
          (error,sol)
    
        
      let dump parameter error handler  a =
        let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "[" in
        let _,error  = 
          Set.fold_set 
            (fun a (bool,error) -> 
              let error,a' = L.to_string parameter error a in 
              let _ = 
                if bool 
                then 
                   Printf.fprintf parameter.Remanent_parameters_sig.log ";%s" a'  
                else 
                   Printf.fprintf parameter.Remanent_parameters_sig.log "%s" a' 
              in 
                true,error
            )
            a 
            (false,error)
        in 
        let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "]" in 
          error
  
        let to_string parameter error handler a = 
          let sol = ["["] in  
          let _,sol,error = 
            Set.fold_set 
              (fun a (bool,sol,error) -> 
                 let error,a' = L.to_string parameter error a in 
                 let _ = 
                    if bool 
                    then 
                      (";"^a')::sol  
                    else 
                      a'::sol               
                 in true,sol,error)
              a 
              (false,sol,error)
          in 
          let sol = List.rev ("]"::sol) in 
            error,sol   
          
      let dump_couple parameter error handler a =
        let _,error  = 
          Pair_Set.fold_set 
            (fun (a,b) (bool,error) -> 
              let error,a' = L.to_string parameter error a in 
              let error,b' = L.to_string parameter error b in 
              let _ = 
                if bool 
                then 
                   Printf.fprintf parameter.Remanent_parameters_sig.log ";[%s->%s]" a' b' 
                else 
                   Printf.fprintf parameter.Remanent_parameters_sig.log "[%s->%s]" a' b'
              in 
                true,error
            )
            a 
            (false,error)
        in 
          error
  
        let to_string_couple parameter error handler a = 
          let sol = [] in  
          let _,sol,error = 
            Pair_Set.fold_set 
              (fun (a,b) (bool,sol,error) -> 
                 let error,a' = L.to_string parameter error a in 
                 let error,b' = L.to_string parameter error b in 
                 let _ = 
                    if bool 
                    then 
                      (";["^a'^"->"^b'^"]")::sol  
                    else 
                      ("["^a'^"->"^b'^"]")::sol               
                 in true,sol,error)
              a 
              (false,sol,error)
          in 
          let sol = List.rev sol in 
            error,sol     
end:Label_handler))

module Implicit = 
  (functor (L:Labels) -> 
    (struct 
      type label = L.label 
      module Set = Set_and_map.Make (struct type t=label let compare = compare end)
      type label_set = Set.set       
      type label_set_couple =  (label_set * label_set) list 
    
      let label_of_int = L.label_of_int
      let empty = Set.empty_set 
      let empty_couple  = []
      let add_set = Set.add_set
      let add_couple remanent error a b sol = error,(a,b)::sol 
        
      let dump parameter error handler a =
        let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "[" in
        let _ = 
          Set.fold_set 
                (fun x bool ->
                     let error,x' = L.to_string parameter error x in 
                     let _ = 
                         if bool 
                         then 
                            Printf.fprintf parameter.Remanent_parameters_sig.log ";%s" x'  
                         else 
                            Printf.fprintf parameter.Remanent_parameters_sig.log "%s" x' 
                     in true)
                       a 
                false
        in 
        let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "]" in 
          error
  
    let to_string parameter error handler a = 
        let sol = [] in  
        let _,sol = 
          Set.fold_set 
              (fun x (bool,sol) -> 
                   let error,x' = L.to_string parameter error x in 
                   let sol = 
                     if bool 
                     then 
                        (";"^x')::sol  
                     else 
                        x'::sol               
                   in true,sol)
              a 
              (false,sol)
        in 
        let sol = List.rev (sol) in 
          error,sol    
        
    let dump_couple parameter error handler a =
        let _ = 
          List.fold_left 
            (fun bool (a,b) -> 
              Set.fold_set 
                (fun x bool ->
                   let error,x' = L.to_string parameter error x in 
                      Set.fold_set 
                      (fun y bool -> 
                         let error,y' = L.to_string parameter error y in 
                          let _ = 
                            if bool 
                            then 
                              Printf.fprintf parameter.Remanent_parameters_sig.log ";[%s->%s]" x' y' 
                            else 
                              Printf.fprintf parameter.Remanent_parameters_sig.log "[%s->%s]" x' y'
                          in true)
                      b 
                      bool)
                a 
                bool)
            false 
            a
        in 
          error
  
      let to_string_couple parameter error handler a = 
        let sol = ["["] in  
        let _,sol = 
          List.fold_left
            (fun (bool,sol) (a,b) -> 
              Set.fold_set 
                (fun x (bool,sol) -> 
                   let error,x' = L.to_string parameter error x in 
                  Set.fold_set
                    (fun y (bool,sol) -> 
                      let error,y' = L.to_string parameter error y in 
                      let sol = 
                          if bool 
                          then 
                            (";["^x'^"->"^y'^"]")::sol  
                          else 
                            ("["^x'^"->"^y'^"]")::sol               
                      in true,sol)
                    a 
                    (bool,sol))
               b
               (bool,sol))
            (false,sol)
            a
        in 
        let sol = List.rev ("]"::sol) in 
          error,sol     

end:Label_handler))