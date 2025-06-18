type 'a formula =
  | P of 'a
  | NOT of 'a formula
  | IMPLY of 'a formula * 'a formula
  | AND of 'a formula * 'a formula
  | OR of 'a formula * 'a formula
  | False
  | True

let op x y = x = NOT y || y = NOT x

let rec simplify f =
  match f with
  | NOT x ->
    let x = simplify x in
    (match x with
    | NOT x -> x
    | True -> False
    | False -> True
    | IMPLY _ | OR _ | AND _ | P _ -> NOT x)
  | IMPLY (x, y) -> simplify (OR (NOT x, y))
  | OR (x, y) ->
    let x = simplify x in
    let y = simplify y in
    (match x, y with
    | True, _ | _, True -> True
    | False, x | x, False -> x
    | x, AND (y, z) when op x y -> OR (x, z)
    | AND (y, z), x when op x y -> OR (x, z)
    | x, AND (z, y) when op x y -> OR (x, z)
    | AND (z, y), x when op x y -> OR (x, z)
    | ( (OR _ | AND _ | NOT _ | IMPLY _ | P _),
        (OR _ | AND _ | NOT _ | IMPLY _ | P _) ) ->
      OR (x, y))
  | AND (x, y) ->
    let x = simplify x in
    let y = simplify y in
    (match x, y with
    | False, _ | _, False -> False
    | True, x | x, True -> x
    | x, OR (y, z) when op x y -> AND (x, z)
    | OR (y, z), x when op x y -> AND (x, z)
    | x, OR (z, y) when op x y -> AND (x, z)
    | OR (z, y), x when op x y -> AND (x, z)
    (* | OR (AND(a,b),c), OR(d,e) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (AND(b,a),c), OR(d,e) when op c d && b = e-> simplify (AND(b,OR(c,a)))
       | OR (c,AND(a,b)), OR(d,e) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (c,AND(b,a)), OR(d,e) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(d,e), OR (AND(a,b),c) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(d,e),OR (AND(b,a),c) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(d,e),OR (c,AND(a,b)) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(d,e),OR (c,AND(b,a)) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (AND(a,b),c), OR(e,d) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (AND(b,a),c), OR(e,d) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (c,AND(a,b)), OR(e,d) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR (c,AND(b,a)), OR(e,d) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(e,d), OR (AND(a,b),c) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(e,d),OR (AND(b,a),c) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(e,d),OR (c,AND(a,b)) when op c d && b = e -> simplify (AND(b,OR(c,a)))
       | OR(e,d),OR (c,AND(b,a)) when op c d && b = e -> simplify (AND(b,OR(c,a)))*)
    | ( (OR _ | AND _ | NOT _ | IMPLY _ | P _),
        (OR _ | AND _ | NOT _ | IMPLY _ | P _) ) ->
      AND (x, y))
  | True | False | P _ -> f

let rec print_formula f_print_string error string_of formula =
  match formula with
  | P x ->
    let error, s = string_of error x in
    let () = f_print_string s in
    error
  | NOT x ->
    let () = f_print_string "~" in
    print_arg f_print_string error string_of False x
  | OR (x1, x2) ->
    let error = print_arg f_print_string error string_of (OR (P (), P ())) x1 in
    let () = f_print_string ".or." in
    print_arg f_print_string error string_of (OR (P (), P ())) x2
  | AND (x1, x2) ->
    let error =
      print_arg f_print_string error string_of (AND (P (), P ())) x1
    in
    let () = f_print_string ".and." in
    print_arg f_print_string error string_of (AND (P (), P ())) x2
  | IMPLY (x1, x2) ->
    let error = print_arg f_print_string error string_of False x1 in
    let () = f_print_string "=>" in
    print_arg f_print_string error string_of False x2
  | True ->
    let () = f_print_string ".T." in
    error
  | False ->
    let () = f_print_string ".F." in
    error

and print_arg f_print_string error string_of scheme formula =
  match formula, scheme with
  | (P _ | True | False), _ ->
    print_formula f_print_string error string_of formula
  | OR (_, _), OR (_, _) -> print_formula f_print_string error string_of formula
  | AND (_, _), AND (_, _) ->
    print_formula f_print_string error string_of formula
  | (NOT _ | OR (_, _) | AND (_, _) | IMPLY (_, _)), _ ->
    let () = f_print_string "(" in
    let error = print_formula f_print_string error string_of formula in
    let () = f_print_string ")" in
    error

let rec formula_to_json value_to_json g =
  match g with
  | True -> `Assoc [ "type", `String "True" ]
  | False -> `Assoc [ "type", `String "False" ]
  | P value -> `Assoc [ "type", `String "Param"; "value", value_to_json value ]
  | NOT g1 ->
    `Assoc [ "type", `String "Not"; "guard", formula_to_json value_to_json g1 ]
  | AND (g1, g2) ->
    `Assoc
      [
        "type", `String "And";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]
  | OR (g1, g2) ->
    `Assoc
      [
        "type", `String "Or";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]
  | IMPLY (g1, g2) ->
    `Assoc
      [
        "type", `String "Imply";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]

let rec formula_of_json value_of_json json =
  match json with
  | `Assoc fields ->
    (match List.assoc "type" fields with
    | `String "True" -> True
    | `String "False" -> False
    | `String "Param" ->
      let value_json = List.assoc "value" fields in
      P (value_of_json value_json)
    | `String "Not" ->
      let guard_json = List.assoc "guard" fields in
      NOT (formula_of_json value_of_json guard_json)
    | `String "And" ->
      let left_json = List.assoc "left" fields in
      let right_json = List.assoc "right" fields in
      AND
        ( formula_of_json value_of_json left_json,
          formula_of_json value_of_json right_json )
    | `String "Or" ->
      let left_json = List.assoc "left" fields in
      let right_json = List.assoc "right" fields in
      OR
        ( formula_of_json value_of_json left_json,
          formula_of_json value_of_json right_json )
    | `String "Imply" ->
      let left_json = List.assoc "left" fields in
      let right_json = List.assoc "right" fields in
      IMPLY
        ( formula_of_json value_of_json left_json,
          formula_of_json value_of_json right_json )
    | `String unknown -> failwith ("Unknown guard type: " ^ unknown)
    | _ -> raise (Yojson.Basic.Util.Type_error ("Incorrect guard", json)))
  | _ -> raise (Yojson.Basic.Util.Type_error ("Incorrect guard", json))

let rec convert_p convert error guard_params g =
  match g with
  | True -> error, True
  | False -> error, False
  | P p ->
    let error, conv_p = convert p error guard_params in
    error, P conv_p
  | NOT g1 ->
    let error, conv_g1 = convert_p convert error guard_params g1 in
    error, NOT conv_g1
  | AND (g1, g2) ->
    let error, conv_g1 = convert_p convert error guard_params g1 in
    let error, conv_g2 = convert_p convert error guard_params g2 in
    error, AND (conv_g1, conv_g2)
  | OR (g1, g2) ->
    let error, conv_g1 = convert_p convert error guard_params g1 in
    let error, conv_g2 = convert_p convert error guard_params g2 in
    error, OR (conv_g1, conv_g2)
  | IMPLY (g1, g2) ->
    let error, conv_g1 = convert_p convert error guard_params g1 in
    let error, conv_g2 = convert_p convert error guard_params g2 in
    error, IMPLY (conv_g1, conv_g2)

let merge_guards g1 g2 =
  let guards = List.merge String.compare g1 g2 in
  List.sort_uniq String.compare guards

let rec get_list_of_predicates = function
  | True | False -> []
  | P (id, _) -> [ id ]
  | NOT guard -> get_list_of_predicates guard
  | AND (g1, g2) | OR (g1, g2) | IMPLY (g1, g2) ->
    let gp1 = get_list_of_predicates g1 in
    let gp2 = get_list_of_predicates g2 in
    merge_guards gp1 gp2
