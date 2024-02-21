(**
   * counting_algebrae.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 06/10/2010
   * Last modification: Time-stamp: <Jul 12 2016>
   * *
   * Choices of algebrae for counting or enumerating the shapes that we can do with puzzle pieces
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

module type Enumeration = sig
  type brick
  type puzzle_hole
  type abstract_species_set

  val size_of_abstract_species_set : abstract_species_set -> Int_inf.bi_intinf

  val promote :
    brick Linear_combination.linear_combination -> abstract_species_set

  val combine :
    abstract_species_set ->
    puzzle_hole ->
    puzzle_hole ->
    abstract_species_set ->
    abstract_species_set

  val sum : abstract_species_set -> abstract_species_set -> abstract_species_set
  val square : abstract_species_set -> abstract_species_set

  val print :
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    out_channel ->
    (out_channel -> puzzle_hole -> unit) ->
    abstract_species_set ->
    unit

  val print_short :
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    out_channel ->
    (out_channel -> puzzle_hole -> unit) ->
    abstract_species_set ->
    unit

  val nil : abstract_species_set
  val infinity : abstract_species_set
end

module Explicit_enumeration :
  Enumeration with type puzzle_hole = int and type brick = int = struct
  type brick = int
  type puzzle_hole = (*Kappa.puzzle_hole *) int

  type regular_formula =
    | Nil
    | Brick of brick
    | Sum of (regular_formula * regular_formula)
    | Product of (regular_formula * puzzle_hole * puzzle_hole * regular_formula)
    | Square of regular_formula
    | Infinity

  type abstract_species_set = {
    regular_formula: regular_formula;
    cardinal: Int_inf.bi_intinf;
  }

  let nil = { regular_formula = Nil; cardinal = Int_inf.bi_zero }
  let infinity = { regular_formula = Infinity; cardinal = Int_inf.infty }
  let size_of_abstract_species_set set = set.cardinal
  let print_brick _kappa stdout brick = Printf.fprintf stdout "%i" brick

  let print_regular_formula _error kappa stdout print_hole set =
    let rec aux regular_formula =
      match regular_formula with
      | Nil -> Printf.fprintf stdout "0"
      | Brick b -> print_brick kappa stdout b
      | Sum (a, b) ->
        let _ = aux_sum a in
        let _ = Printf.fprintf stdout "+" in
        let _ = aux_sum b in
        ()
      | Product (a, b, c, d) ->
        let _ = aux_in_paren a in
        let _ = Printf.fprintf stdout ":" in
        let _ = print_hole stdout b in
        let _ = Printf.fprintf stdout "-" in
        let _ = print_hole stdout c in
        let _ = Printf.fprintf stdout ":" in
        let _ = aux_in_paren d in
        ()
      | Square a ->
        let _ = aux_in_paren a in
        let _ = Printf.fprintf stdout "^2" in
        ()
      | Infinity ->
        let _ = Printf.fprintf stdout "+oo" in
        ()
    and aux_in_paren regular_formula =
      match regular_formula with
      | Brick _ | Nil -> aux regular_formula
      | Sum _ | Infinity | Product _ | Square _ ->
        let _ = Printf.fprintf stdout "(" in
        let _ = aux regular_formula in
        let _ = Printf.fprintf stdout ")" in
        ()
    and aux_itemize p regular_formula =
      if p regular_formula then
        aux regular_formula
      else
        aux_in_paren regular_formula
    and aux_sum x =
      aux_itemize
        (fun regular_formula ->
          match regular_formula with
          | Nil | Brick _ | Sum _ -> true
          | Product _ | Square _ | Infinity -> false)
        x
    in
    let _ = aux set.regular_formula in
    ()

  let print error kappa stdout (*hole_array*) print_hole set =
    let _ = print_regular_formula error kappa stdout print_hole set in
    let _ =
      Printf.fprintf stdout "\n\n There are %s chemical species.\n"
        (Int_inf.bi_string_of set.cardinal)
    in
    ()

  let print_short error kappa stdout (*hole_array*) print_hole set =
    let _ = print_regular_formula error kappa stdout print_hole set in
    let _ =
      Printf.fprintf stdout "(%s)\n" (Int_inf.bi_string_of set.cardinal)
    in
    ()

  let combine a b c d =
    {
      regular_formula = Product (a.regular_formula, b, c, d.regular_formula);
      cardinal = Int_inf.bi_mult a.cardinal d.cardinal;
    }

  let promote linear_combination =
    let regular_expression, cardinal =
      match linear_combination with
      | [] -> Nil, Int_inf.bi_zero
      | (n, t) :: q ->
        List.fold_left
          (fun (regular_expression, cardinal) (n, brick) ->
            ( Sum (Brick brick, regular_expression),
              Int_inf.bi_add (Int_inf.bi_of_int n) cardinal ))
          (Brick t, Int_inf.bi_of_int n)
          q
    in
    { regular_formula = regular_expression; cardinal }

  let sum a b =
    if a == nil then
      b
    else if b == nil then
      a
    else
      {
        regular_formula = Sum (a.regular_formula, b.regular_formula);
        cardinal = Int_inf.bi_add a.cardinal b.cardinal;
      }

  let square a =
    {
      regular_formula = Square a.regular_formula;
      cardinal = Int_inf.bi_n_n_plus_1_divided_by_2 a.cardinal;
    }
end

module Counting : Enumeration with type puzzle_hole = int and type brick = int =
struct
  type brick = int
  type puzzle_hole = (*Kappa.puzzle_hole *) int
  type abstract_species_set = Int_inf.bi_intinf

  let nil = Int_inf.bi_zero
  let infinity = Int_inf.infty
  let size_of_abstract_species_set set = set

  (*let dual _ kappa_handler = kappa_handler.Kappa.dual_of_puzzle_hole *)

  let print _error _kappa stdout _ (*_*) set =
    let _ =
      Printf.fprintf stdout "\n\n There are %s chemical species.\n"
        (Int_inf.bi_string_of set)
    in
    ()

  let print_short = print

  let promote linear_combination =
    List.fold_left
      (fun cardinal (n, _brick) ->
        Int_inf.bi_add (Int_inf.bi_of_int n) cardinal)
      Int_inf.bi_zero linear_combination

  let combine a _ _ = Int_inf.bi_mult a
  let sum = Int_inf.bi_add
  let square a = Int_inf.bi_n_n_plus_1_divided_by_2 a
end
