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

let rec print_formula acc f_print_string formula =
  match formula with
  | P x -> f_print_string x acc
  | NOT x ->
    let acc = f_print_string ".not." acc in
    print_arg acc f_print_string False x
  | OR (x1, x2) ->
    let acc = print_arg acc f_print_string (OR (P (), P ())) x1 in
    let acc = f_print_string ".or." acc in
    print_arg acc f_print_string (OR (P (), P ())) x2
  | AND (x1, x2) ->
    let acc = print_arg acc f_print_string (AND (P (), P ())) x1 in
    let acc = f_print_string ".and." acc in
    print_arg acc f_print_string (AND (P (), P ())) x2
  | IMPLY (x1, x2) ->
    let acc = print_arg acc f_print_string False x1 in
    let acc = f_print_string "=>" acc in
    print_arg acc f_print_string False x2
  | True -> f_print_string ".T." acc
  | False -> f_print_string ".F." acc

and print_arg acc f_print_string scheme formula =
  match formula, scheme with
  | (P _ | True | False), _ -> print_formula acc f_print_string formula
  | OR (_, _), OR (_, _) -> print_formula acc f_print_string formula
  | AND (_, _), AND (_, _) -> print_formula acc f_print_string formula
  | (NOT _ | OR (_, _) | AND (_, _) | IMPLY (_, _)), _ ->
    let acc = f_print_string "(" acc in
    let acc = print_formula acc f_print_string formula in
    let acc = f_print_string ")" acc in
    acc

let rec formula_to_json value_to_json g =
  match g with
  | True -> `Assoc [ "type", JsonUtil.of_string "True" ]
  | False -> `Assoc [ "type", JsonUtil.of_string "False" ]
  | P value ->
    `Assoc [ "type", JsonUtil.of_string "Param"; "value", value_to_json value ]
  | NOT g1 ->
    `Assoc
      [
        "type", JsonUtil.of_string "Not";
        "guard", formula_to_json value_to_json g1;
      ]
  | AND (g1, g2) ->
    `Assoc
      [
        "type", JsonUtil.of_string "And";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]
  | OR (g1, g2) ->
    `Assoc
      [
        "type", JsonUtil.of_string "Or";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]
  | IMPLY (g1, g2) ->
    `Assoc
      [
        "type", JsonUtil.of_string "Imply";
        "left", formula_to_json value_to_json g1;
        "right", formula_to_json value_to_json g2;
      ]

let rec formula_of_json value_of_json json =
  match json with
  | `Assoc fields ->
    (try
       match List.assoc "type" fields with
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
       | _ -> raise (Yojson.Basic.Util.Type_error ("Incorrect guard", json))
     with _ -> raise (Yojson.Basic.Util.Type_error ("Incorrect guard", json)))
  | _ -> raise (Yojson.Basic.Util.Type_error ("Incorrect guard", json))

let rec convert_p convert error g =
  match g with
  | True -> error, True
  | False -> error, False
  | P p ->
    let error, conv_p = convert p error in
    error, P conv_p
  | NOT g1 ->
    let error, conv_g1 = convert_p convert error g1 in
    error, NOT conv_g1
  | AND (g1, g2) ->
    let error, conv_g1 = convert_p convert error g1 in
    let error, conv_g2 = convert_p convert error g2 in
    error, AND (conv_g1, conv_g2)
  | OR (g1, g2) ->
    let error, conv_g1 = convert_p convert error g1 in
    let error, conv_g2 = convert_p convert error g2 in
    error, OR (conv_g1, conv_g2)
  | IMPLY (g1, g2) ->
    let error, conv_g1 = convert_p convert error g1 in
    let error, conv_g2 = convert_p convert error g2 in
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


let rec rename_pos rename_pos1 rename a = 
  match a with 
  | P a -> P (rename_pos1 rename a)
  | NOT a -> NOT (rename_pos rename_pos1 rename a) 
  | IMPLY (a,b) -> IMPLY (rename_pos rename_pos1 rename a,rename_pos rename_pos1 rename b) 
  | AND (a,b) -> AND (rename_pos rename_pos1 rename a,rename_pos rename_pos1 rename b)
  | OR (a,b) -> OR (rename_pos rename_pos1 rename a,rename_pos rename_pos1 rename b) 
  | False | True -> a 

let rec diff_pos diff_atom (a:'a formula) a' l = 
  match a,a' with 
  | P a, P a' -> diff_atom a a' l 
  | NOT a, NOT a' -> diff_pos diff_atom a a' l 
  | IMPLY (a1,a2), IMPLY (a1',a2') | OR  (a1,a2), OR (a1',a2') | AND (a1,a2), AND (a1',a2') ->  diff_pos diff_atom a2 a2' (diff_pos diff_atom a1 a1' l)
  | False, False 
  | True, True -> l 
  | (P _ |NOT _ |IMPLY _| OR _ | AND _ | True | False ),
    (P _ |NOT _ |IMPLY _| OR _ | AND _ | True | False ) -> failwith (invalid_arg "diff_pos")