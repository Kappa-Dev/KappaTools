let print_alg = Kappa_printer.alg_expr ?env:None
let print_bool = Expr.print_bool print_alg
let print_cc = Connected_component.print ?sigs:None true
let print_transformation = Transformations.print ?sigs:None
let print_rule = Kappa_printer.elementary_rule ?env:None
let print_modification = Kappa_printer.modification ?env:None
let print_perturbation = Kappa_printer.perturbation ?env:None
let print_injections = Rule_interpreter.print_injections ?sigs:None
