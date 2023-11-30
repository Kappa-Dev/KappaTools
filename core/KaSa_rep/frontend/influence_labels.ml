(**
 * influence_labels.ml
 * openkappa
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: March, the 7th 2011
 * Last modification: Time-stamp: <Nov 08 2017>
 * *
 * Labels to be associated with influence relations
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

module type Labels = sig
  type label

  val label_of_int :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    int ->
    Exception.method_handler * label

  val to_string :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    label ->
    Exception.method_handler * string

  val dump :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    label ->
    Exception.method_handler

  val print : Format.formatter -> label -> unit
  val int_of_label : label -> int
end

module Int_labels : Labels with type label = int = struct
  type label = int

  let label_of_int _ error i = error, i

  let to_string _parameter error i =
    if i < 0 then
      error, string_of_int (-i - 1) ^ "*"
    else
      error, string_of_int i

  let print f i =
    if i < 0 then
      Format.fprintf f "%i*" (-i - 1)
    else
      Format.pp_print_int f i

  let dump h error i =
    let error, s = to_string h error i in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger h) "%s" s in
    error

  let int_of_label i = i
end

module type Label_handler = sig
  type label
  type label_set
  type label_set_couple

  val label_of_int :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    int ->
    Exception.method_handler * label

  val member : label -> label_set -> bool
  val empty : label_set
  val empty_couple : label_set_couple
  val is_empty_couple : label_set_couple -> bool
  val add_set : label -> label_set -> label_set
  val remove_set : label -> label_set -> label_set

  val add_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    bool ->
    label_set ->
    label_set ->
    label_set_couple ->
    Exception.method_handler * label_set_couple

  val dump :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set ->
    Exception.method_handler

  val dump_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set_couple ->
    Exception.method_handler

  val filter_couple :
    Remanent_parameters_sig.parameters ->
    'a ->
    Cckappa_sig.kappa_handler ->
    ('a -> label -> label -> 'a * bool) ->
    label_set_couple ->
    'a * label_set_couple

  val to_string :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set ->
    Exception.method_handler * string list

  val to_string_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set_couple ->
    Exception.method_handler * string list

  val convert_label_set_couple : label_set_couple -> (int * int) list
end

module Empty : Label_handler with type label = unit = struct
  type label = unit
  type label_set = unit
  type label_set_couple = unit

  let label_of_int _handler error _ = error, ()
  let empty = ()
  let empty_couple = ()
  let member _label _labelset = false
  let is_empty_couple _ = false
  let add_set _ _ = ()
  let remove_set _ _ = ()
  let add_couple _ error _ _ _ _ = error, ()
  let dump _ error _ _ = error
  let to_string _ error _ _ = error, []
  let dump_couple _ error _ _ = error
  let filter_couple _ error _ _ a = error, a
  let to_string_couple _ error _ _ = error, []
  let convert_label_set_couple _ = [ 0, 0 ]
end

module Extensive =
functor
  (L : Labels)
  ->
  (
    struct
      type label = L.label

      module LSetMap = SetMap.Make (struct
        type t = label

        let compare = compare
        let print = L.print
      end)

      module Set = LSetMap.Set

      type label_set = Set.t

      module Pair_Set = SetMap.Make (struct
        type t = label * label

        let compare = compare
        let print f (a, b) = Format.fprintf f "(%a, %a)" L.print a L.print b
      end)

      type label_set_couple = Pair_Set.Set.t

      let label_of_int = L.label_of_int
      let empty = Set.empty
      let member = Set.mem
      let empty_couple = Pair_Set.Set.empty
      let is_empty_couple = Pair_Set.Set.is_empty
      let add_set = Set.add
      let remove_set = Set.remove

      let add_couple _remanent error bool a b sol =
        Set.fold
          (fun a (error, sol) ->
            Set.fold
              (fun b (error, sol) ->
                if (not bool) && a = b then
                  error, sol
                else
                  error, Pair_Set.Set.add (a, b) sol)
              b (error, sol))
          a (error, sol)

      let dump parameter error _handler a =
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "["
        in
        let _, error =
          Set.fold
            (fun a (bool, error) ->
              let error, a' = L.to_string parameter error a in
              let _ =
                if bool then
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    ";%s" a'
                else
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    "%s" a'
              in
              true, error)
            a (false, error)
        in
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "]"
        in
        error

      let to_string parameter error _handler a =
        let sol = [ "[" ] in
        let _, sol, error =
          Set.fold
            (fun a (bool, sol, error) ->
              let error, a' = L.to_string parameter error a in
              let _ =
                if bool then
                  (";" ^ a') :: sol
                else
                  a' :: sol
              in
              true, sol, error)
            a (false, sol, error)
        in
        let sol = List.rev ("]" :: sol) in
        error, sol

      let dump_couple parameter error _handler a =
        let _, error =
          Pair_Set.Set.fold
            (fun (a, b) (bool, error) ->
              let error, a' = L.to_string parameter error a in
              let error, b' = L.to_string parameter error b in
              let _ =
                if bool then
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    ";[%s->%s]" a' b'
                else
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    "[%s->%s]" a' b'
              in
              true, error)
            a (false, error)
        in
        error

      let filter_couple _parameter error _handler f a =
        Pair_Set.Set.fold
          (fun (a, b) (error, set') ->
            let error, bool = f error a b in
            ( error,
              if bool then
                Pair_Set.Set.add (a, b) set'
              else
                set' ))
          a
          (error, Pair_Set.Set.empty)

      let to_string_couple parameter error _handler a =
        let sol = [] in
        let _, sol, error =
          Pair_Set.Set.fold
            (fun (a, b) (bool, sol, error) ->
              let error, a' = L.to_string parameter error a in
              let error, b' = L.to_string parameter error b in
              let _ =
                if bool then
                  (";[" ^ a' ^ "->" ^ b' ^ "]") :: sol
                else
                  ("[" ^ a' ^ "->" ^ b' ^ "]") :: sol
              in
              true, sol, error)
            a (false, sol, error)
        in
        let sol = List.rev sol in
        error, sol

      let convert_label_set_couple set =
        Pair_Set.Set.fold
          (fun (a, b) list -> (L.int_of_label a, L.int_of_label b) :: list)
          set []
    end :
      Label_handler with type label = L.label)

module Implicit =
functor
  (L : Labels)
  ->
  (
    struct
      type label = L.label

      module LSetMap = SetMap.Make (struct
        type t = label

        let compare = compare
        let print = L.print
      end)

      module Set = LSetMap.Set

      type label_set = Set.t
      type label_set_couple = (label_set * label_set) list

      let member = Set.mem
      let label_of_int = L.label_of_int
      let empty = Set.empty
      let empty_couple = []
      let is_empty_couple x = x = []
      let add_set = Set.add
      let remove_set = Set.remove
      let add_couple _remanent error _bool a b sol = error, (a, b) :: sol

      let dump parameter error _handler a =
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "["
        in
        let error, _ =
          Set.fold
            (fun x (error, bool) ->
              let error, x' = L.to_string parameter error x in
              let _ =
                if bool then
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    ";%s" x'
                else
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameter)
                    "%s" x'
              in
              error, true)
            a (error, false)
        in
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "]"
        in
        error

      let to_string parameter error _handler a =
        let sol = [] in
        let _, error, sol =
          Set.fold
            (fun x (bool, error, sol) ->
              let error, x' = L.to_string parameter error x in
              let sol =
                if bool then
                  (";" ^ x') :: sol
                else
                  x' :: sol
              in
              true, error, sol)
            a (false, error, sol)
        in
        let sol = List.rev sol in
        error, sol

      let dump_couple parameter error _handler a =
        let error, _ =
          List.fold_left
            (fun (error, bool) (a, b) ->
              Set.fold
                (fun x (error, bool) ->
                  let error, x' = L.to_string parameter error x in
                  Set.fold
                    (fun y (error, bool) ->
                      let error, y' = L.to_string parameter error y in
                      let _ =
                        if bool then
                          Loggers.fprintf
                            (Remanent_parameters.get_logger parameter)
                            ";[%s->%s]" x' y'
                        else
                          Loggers.fprintf
                            (Remanent_parameters.get_logger parameter)
                            "[%s->%s]" x' y'
                      in
                      error, true)
                    b (error, bool))
                a (error, bool))
            (error, false) a
        in
        error

      let filter_couple _parameter error _handler _f list =
        error, list (* to do *)

      let to_string_couple parameter error _handler a =
        let sol = [ "[" ] in
        let _, error, sol =
          List.fold_left
            (fun (bool, error, sol) (a, b) ->
              Set.fold
                (fun x (bool, error, sol) ->
                  let error, x' = L.to_string parameter error x in
                  Set.fold
                    (fun y (bool, error, sol) ->
                      let error, y' = L.to_string parameter error y in
                      let sol =
                        if bool then
                          (";[" ^ x' ^ "->" ^ y' ^ "]") :: sol
                        else
                          ("[" ^ x' ^ "->" ^ y' ^ "]") :: sol
                      in
                      true, error, sol)
                    a (bool, error, sol))
                b (bool, error, sol))
            (false, error, sol) a
        in
        let sol = List.rev ("]" :: sol) in
        error, sol

      let convert_label_set_couple set =
        List.fold_left
          (fun list (seta, setb) ->
            Set.fold
              (fun a list ->
                Set.fold
                  (fun b list -> (L.int_of_label a, L.int_of_label b) :: list)
                  setb list)
              seta list)
          [] set
    end :
      Label_handler with type label = L.label)
