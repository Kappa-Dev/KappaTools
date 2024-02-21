open Fraction

type intervalle = { inf: ffraction; sup: ffraction }

exception Intervalle_vide

let wide_max = ref { num = 2; den = 1 }
let get_wide_max () = !wide_max
let set_wide_max f = wide_max := f
let sub_convexe a b = not (ffinf a.inf b.inf || ffinf b.sup a.sup)

let trans_convexe i t =
  let inff =
    match i.inf with
    | Frac a -> ffmax (Frac { num = 0; den = 1 }) (Frac (fplus a t))
    | Unknown | Infinity -> raise Exit
    | Minfinity as a -> a
  in
  let supf =
    match i.sup with
    | Frac a -> Frac (fplus a t)
    | Unknown | Minfinity -> raise Exit
    | Infinity as a -> a
  in
  { inf = ffmax inff (Frac { num = 0; den = 1 }); sup = supf }

let zero = { inf = Frac Fraction.zero; sup = Frac Fraction.zero }

let union_convexe t1 t2 =
  let n = Array.length t1 in
  let ts =
    Array.make n
      { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }
  in
  for i = 0 to n - 1 do
    ts.(i) <-
      { inf = ffmin t1.(i).inf t2.(i).inf; sup = ffmax t1.(i).sup t2.(i).sup }
  done;
  ts

let union i1 i2 = { inf = ffmin i1.inf i2.inf; sup = ffmax i1.sup i2.sup }

let wide_union_convexe t1 t2 =
  let n = Array.length t1 in
  let ts =
    Array.make n
      { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }
  in
  for i = 0 to n - 1 do
    ts.(i) <-
      {
        inf =
          (if ffinf t2.(i).inf t1.(i).inf then
             if ffinf t2.(i).inf (Fraction.ffneg (Frac !wide_max)) then
               Minfinity
             else
               t2.(i).inf
           else
             t1.(i).inf);
        sup =
          (if ffinf t1.(i).sup t2.(i).sup then
             if ffinf (Frac !wide_max) t2.(i).sup then
               Infinity
             else
               t2.(i).sup
           else
             t1.(i).sup);
      }
  done;
  ts

let hashnumber = 1

let wide_en_place t1 t2 =
  let n = Array.length t1 in
  let l = Working_list_imperative.make hashnumber in
  let changed = ref false in
  for i = 0 to n - 1 do
    changed := false;
    let rep =
      {
        inf =
          (if ffinf t2.(i).inf t1.(i).inf then
             if ffinf t2.(i).inf (Fraction.ffneg (Frac !wide_max)) then
               if t1.(i).inf = Minfinity then
                 Minfinity
               else (
                 changed := true;
                 Minfinity
               )
             else (
               changed := true;
               t2.(i).inf
             )
           else
             t1.(i).inf);
        sup =
          (if ffinf t1.(i).sup t2.(i).sup then
             if ffinf (Frac !wide_max) t2.(i).sup then
               if t1.(i).sup = Infinity then
                 Infinity
               else (
                 changed := true;
                 Infinity
               )
             else (
               changed := true;
               t2.(i).sup
             )
           else
             t1.(i).sup);
      }
    in
    if !changed then (
      Working_list_imperative.push i l;
      t1.(i) <- rep
    ) else
      ()
  done;
  Working_list_imperative.list l

let cap_inter i1 i2 =
  let is = { inf = ffmax i1.inf i2.inf; sup = ffmin i1.sup i2.sup } in
  if ffinf is.sup is.inf then
    raise Intervalle_vide
  else
    is

let inter_convexe t1 t2 =
  let n = Array.length t1 in
  let ts =
    Array.make n
      { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }
  in
  for i = 0 to n - 1 do
    ts.(i) <- cap_inter t1.(i) t2.(i)
  done;
  ts

let iiplus i1 alpha i2 =
  if alpha.num = 0 then
    i1
  else if alpha.num < 0 then
    { inf = ffplus i1.inf alpha i2.sup; sup = ffplus i1.sup alpha i2.inf }
  else
    { inf = ffplus i1.inf alpha i2.inf; sup = ffplus i1.sup alpha i2.sup }

let combinaison_lineaire_convexe ((l, resultat), s) =
  let resultat = { num = -1 * resultat.num; den = resultat.den } in
  let rec aux l sol =
    match l with
    | (i, k) :: q when k.num < 0 ->
      (match ffplus sol k s.(i).sup with
      | Unknown -> Minfinity
      | (Infinity | Minfinity | Frac _) as k -> aux q k)
    | (i, k) :: q when k.num > 0 ->
      (match ffplus sol k s.(i).inf with
      | Unknown -> Unknown
      | (Infinity | Minfinity | Frac _) as k -> aux q k)
    | _ :: q -> aux q sol
    | [] -> sol
  in

  let inff = aux l (Frac resultat) in
  let rec aux l sol =
    match l with
    | (i, k) :: q when k.num < 0 ->
      (match ffplus sol k s.(i).inf with
      | Unknown -> Infinity
      | (Infinity | Minfinity | Frac _) as k -> aux q k)
    | (i, k) :: q (*when (k.num>0)*) ->
      (match ffplus sol k s.(i).sup with
      | Unknown -> Unknown
      | (Infinity | Minfinity | Frac _) as k -> aux q k)
    | [] -> sol
  in
  let supf = aux l (Frac resultat) in
  if ffinf supf inff then
    raise Intervalle_vide
  else
    { inf = inff; sup = supf }

let contient_zero i =
  let aux1 () =
    match i.inf with
    | Infinity -> false
    | Frac a when a.num > 0 -> false
    | Unknown -> raise Exit
    | Minfinity | Frac _ -> true
  in
  match i.sup with
  | Minfinity -> false
  | Frac a when a.num < 0 -> false
  | Unknown -> raise Exit
  | Infinity | Frac _ -> aux1 ()

let string_of_intervalle parameters error i =
  let error, inf_string =
    match i.inf with
    | Infinity | Unknown -> Exception.warn parameters error __POS__ Exit "BUG"
    | Frac _ ->
      ( error,
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters
        ^ string_of_int (cell_int i.inf) )
    | Minfinity ->
      ( error,
        Remanent_parameters.get_open_int_interval_infinity_symbol parameters
        ^ Remanent_parameters.get_minus_infinity_symbol parameters )
  in
  let error, sup_string =
    match i.sup with
    | Minfinity | Unknown -> Exception.warn parameters error __POS__ Exit "BUG"
    | Frac _ ->
      ( error,
        string_of_int (floor_int i.sup)
        ^ Remanent_parameters.get_close_int_interval_inclusive_symbol parameters
      )
    | Infinity ->
      ( error,
        Remanent_parameters.get_plus_infinity_symbol parameters
        ^ Remanent_parameters.get_close_int_interval_infinity_symbol parameters
      )
  in
  ( error,
    inf_string
    ^ Remanent_parameters.get_int_interval_separator_symbol parameters
    ^ sup_string )
