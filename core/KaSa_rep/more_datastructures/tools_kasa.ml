let fst_option x =
  match x with
  | Some (x, _) -> Some x
  | _ -> None

let snd_option x =
  match x with
  | Some (_, x) -> Some x
  | _ -> None

(* OCaml manual: In particular, if you want a regular expression that
   matches a single backslash character, you need to quote it in the
   argument to regexp (according to the last item of the list above) by
   adding a second backslash. Then you need to quote both backslashes
   (according to the syntax of string constants in OCaml) by doubling
   them again, so you need to write four backslash characters: Str.regexp
   "\\\\". *)

(** dot output*)
let escape_label_in_dot s =
  Str.global_substitute (Str.regexp "[\\\"\\\\]")
    (fun x ->
      match Str.matched_string x with
      | "\"" -> "\\\""
      | "\\" -> "\\\\"
      | _ -> assert false)
    s

let make_id_compatible_with_dot_format parameters error string =
  let tab =
    Remanent_parameters.get_make_labels_compatible_with_dot parameters
  in
  let rec aux pos l =
    if pos < 0 then
      l
    else (
      let char = String.get string pos in
      match Remanent_parameters_sig.CharMap.find_option char tab with
      | Some liste_char ->
        aux (pos - 1)
          (List.fold_left
             (fun list char -> char :: list)
             l (List.rev liste_char))
      | None -> aux (pos - 1) (char :: l)
    )
  in
  let l = aux (String.length string - 1) [] in
  error, String.concat "" (List.rev_map (String.make 1) (List.rev l))

let sorted_parts_of_list n list =
  let list = List.sort (fun a b -> compare b a) list in
  let rec aux k list suffix output =
    if k = 0 then
      suffix :: output
    else (
      match list with
      | h :: t -> aux k t suffix (aux (k - 1) t (h :: suffix) output)
      | [] -> output
    )
  in
  aux n list [] []

let sort_list key parameter error list =
  let error, refined_list =
    List.fold_left
      (fun (error, refined_list) a ->
        let error, key = key parameter error a in
        error, (key, a) :: refined_list)
      (error, []) list
  in
  let refined_sorted_list =
    List.sort (fun (a, _) (b, _) -> compare b a) refined_list
  in
  error, List.rev_map snd refined_sorted_list
