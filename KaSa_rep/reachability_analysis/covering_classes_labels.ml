 (**
  * covering_classes_labels.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 22th February
  * Last modification: 
  * * 
  * Labels to be associated with covering classes relations
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering_classes_labels") message exn (fun () -> default)

module type Labels =
  sig
    type label
    val label_of_int : Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> Exception.method_handler * label
    val to_string : Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> Exception.method_handler * string
    val dump : Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> Exception.method_handler
  end

module Int_labels =
  (struct
      type label = int
                     
      let label_of_int _ error i = error, i
                                            
      let to_string parameter error i =
        error, string_of_int i
                             
      let dump h error i =
        let error, s = to_string h error i in
        let _ = Printf.fprintf (Remanent_parameters.get_log h) "%s" s in
        error

    end:Labels)

module type Label_handler =
  sig
    type label
    type label_set
           
    (*TODO if needed*)
    val label_of_int : Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> Exception.method_handler * label
    val empty : label_set
    val add_set : Remanent_parameters_sig.parameters -> Exception.method_handler -> label -> label_set -> Exception.method_handler * label_set
    val dump : Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler -> label_set -> Exception.method_handler
    val to_string : Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.kappa_handler -> label_set -> Exception.method_handler * string list
  end

module Empty =
  (struct
      type label = unit
      type label_set = unit

      let lable_of_int handler error _ = error, ()
      let empty = ()
      let add_set _ error _ _ = error, ()
      let dump _ error _ _ = error
      let to_string _ error _ _ = error, []
   end:Label_handler with type label = unit)
    
module Extensive =
  (functor (L:Lables) ->
   (struct
       type label = L.label
       module Set = Set_and_map.Make
                      (struct
                          type t = label
                          let compare = compare
                        end)
       type label_set = Set.set
                          
       let label_of_int = L.label_of_int
       let empty = Set.empty_set
       let add_set = Set.add_set

       let dump parameter error handler a =
         let _ = Printf.fprintf (Remanent_parameters.get_log parameter) "[" in
         let _, error =
           Set.fold_set
             (fun a (bool, error) ->
              let error, a' = L.to_string parameter error a in
              let _ =
                if bool
                then Printf.fprintf (Remanent_parameters.get_log parameter) ";%s" a'
                else Printf.fprintf (Remanent_parameters.get_log parameter) "%s" a'
              in
              true, error
             )
             a (false, error)
         in
         let _ = Printf.fprintf (Remanent_parameters.get_log parameter) "]" in
         error

       let to_string parameter error handler a =
         let solution = [ "[" ] in
         let _, solution, error =
           Set.fold_set
             (fun a (bool, solution, error) ->
              let error, a' = L.to_string parameter error a in
              let _ =
                if bool
                then (";" ^ a') :: solution
                else
                  a' :: solution
              in true, solution, error)
             a (false, solution, error)
         in
         let solution = List.rev ("]" :: solution) in
         error, solution
                             
   end:Label_handler)
  )
    
