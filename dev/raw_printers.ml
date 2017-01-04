let print_ast_alg f a =
  Alg_expr.print (fun _ () -> ()) (fun f (x,_) -> Format.pp_print_string f x)
    (fun f (x,_) -> Format.fprintf f "'%s'" x) f a
let print_alg = Kappa_printer.alg_expr ?env:None
let print_bool f a =
  Alg_expr.print_bool (fun _ () -> ())
    (fun f (x,_) -> Format.pp_print_string f x)
    (fun f (x,_) -> Format.pp_print_string f x) f a
let print_cc = Pattern.print_cc ?sigs:None ?cc_id:None
let print_place = Matching.Agent.print ?sigs:None
let print_transformation = Primitives.Transformation.print ?sigs:None
let print_rule = Kappa_printer.elementary_rule ?env:None
let print_modification = Kappa_printer.modification ?env:None
let print_perturbation = Kappa_printer.perturbation ?env:None
let print_path = Edges.print_path ?sigs:None
let print_injections = Rule_interpreter.print_injections ?domain:None
