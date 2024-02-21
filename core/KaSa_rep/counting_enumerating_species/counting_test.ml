(**
    * counting_test.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 06/10/2010
    * Last modification: Time-stamp: <Aug 05 2016>
    * *
    * Test suite for the counting engine
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

module C = Counting_engine.Count (Counting_algebrae.Explicit_enumeration)
module D = Counting_engine.Count (Counting_algebrae.Counting)

let f parameters dual dual_and_self interface_of_brick init =
  let _error_handler, kappa_handler =
    List_tokens.empty_handler parameters Exception.empty_error_handler
  in
  let i =
    C.count parameters Exception.empty_error_handler kappa_handler
      {
        Counting_engine.print_hole = (fun log -> Printf.fprintf log "%d");
        Counting_engine.dual;
        Counting_engine.dual_and_self;
        Counting_engine.interface_of_brick;
      }
      C.print_handler init
  in
  let j =
    D.count parameters Exception.empty_error_handler kappa_handler
      {
        Counting_engine.print_hole = (fun log -> Printf.fprintf log "%d");
        Counting_engine.dual;
        Counting_engine.dual_and_self;
        Counting_engine.interface_of_brick;
      }
      D.print_handler init
  in
  ( Counting_algebrae.Explicit_enumeration.size_of_abstract_species_set i,
    Counting_algebrae.Counting.size_of_abstract_species_set j )

let test_counting_procedure parameters =
  let dual error x =
    ( error,
      match x with
      | 1 -> [ 2; 3 ]
      | 2 -> [ 1 ]
      | 3 -> [ 1 ]
      | 4 -> [ 5 ]
      | 5 -> [ 4 ]
      | _ -> [] )
  in
  let dual_and_self error x =
    ( error,
      (match x with
      | 1 -> [ 2; 3 ]
      | 2 -> [ 1 ]
      | 3 -> [ 1 ]
      | 4 -> [ 5 ]
      | 5 -> [ 4 ]
      | _ -> []),
      match x with
      | 6 -> true
      | _ -> false )
  in
  let interface_of_brick error x =
    ( error,
      match x with
      | 0 -> [ 1 ]
      | 1 -> [ 2; 3 ]
      | 2 -> []
      | 3 -> [ 4 ]
      | 4 -> [ 5; 6 ]
      | _ -> [] )
  in
  let init = [ 2, 0; 4, 1; 5, 2; 2, 3; 2, 4 ] in
  let _ = Printf.fprintf stdout "\n\nFirst test\n\n" in
  let n = f parameters dual dual_and_self interface_of_brick init in
  let dual error x =
    ( error,
      if x = 1 then
        [ 2 ]
      else if x = 2 then
        [ 1 ]
      else
        [] )
  in
  let dual_and_self error x =
    let error, y = dual error x in
    error, y, false
  in
  let interface_of_brick error x =
    ( error,
      if x = 1 then
        [ 2 ]
      else if x = 2 then
        [ 1; 2 ]
      else
        [] )
  in
  let init = [ 1, 1; 1, 2 ] in

  let _ = Printf.fprintf stdout "\n\nSecond test\n\n" in
  let m = f parameters dual dual_and_self interface_of_brick init in
  [
    ("Counting0", fun x -> x, Int_inf.equal (fst n) (snd n), None);
    ( "Counting1",
      fun x ->
        x, Int_inf.equal (fst n) (Int_inf.Int (Big_int.big_int_of_int 41)), None
    );
    ("Counting2", fun x -> x, Int_inf.equal (fst m) (snd m), None);
    ("Counting3", fun x -> x, Int_inf.equal (fst m) Int_inf.Infinity, None);
  ]
