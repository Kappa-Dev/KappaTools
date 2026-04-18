type 'a bot_or_not = Bot | Not_bot of 'a
type maybe_bool = Sure_value of bool | Maybe
type 'a top_or_not = Top | Not_top of 'a
type 'a flat_lattice = Val of 'a | Any | Undefined

let lub a b =
  match a, b with
  | Undefined, _ -> b
  | _, Undefined -> a
  | Any, _ | _, Any -> Any
  | Val x, Val y when x = y -> a
  | Val _, Val _ -> Any

let lub_with_or fun_or parameters handler error a b =
  match a, b with
  | Undefined, _ -> error, handler, b
  | _, Undefined -> error, handler, a
  | Any, _ | _, Any -> error, handler, Any
  | Val (x, assox), Val (y, assoy) when x = y ->
    let error, handler, asso = fun_or parameters handler error assox assoy in
    error, handler, Val (x, asso)
  | Val _, Val _ -> error, handler, Any

let glb_list_with_and fun_and parameters handler error a b =
  match a, b with
  | Undefined, _ | _, Undefined -> error, handler, Undefined
  | Any, Val l | Val l, Any -> error, handler, Val l
  | Any, Any -> error, handler, Any
  | Val l, Val l' ->
    (*get the intersection of list*)
    let error, handler, l =
      Misc_sa.inter_list_with_and fun_and
        (fun a b -> compare a b)
        parameters handler error l l'
    in
    error, handler, Val l

let print_elt print_key print_data parameters handler error a =
  let error, handler =
    match a with
    | Undefined ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "Undefined"
      in
      error, handler
    | Any ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "Any"
      in
      error, handler
    | Val (key, data) ->
      let error, handler = print_key parameters handler error key in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "Any"
      in
      let error, handler = print_data parameters handler error data in
      error, handler
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  error, handler

let print_list print_key print_data parameters handler error a =
  let error, handler =
    match a with
    | Undefined ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "Undefined"
      in
      error, handler
    | Any ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "Any"
      in
      error, handler
    | Val l ->
      List.fold_left
        (fun (error, handler) (key, data) ->
          let error, handler = print_key parameters handler error key in
          let error, handler = print_data parameters handler error data in
          error, handler)
        (error, handler) l
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  error, handler
