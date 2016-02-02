let print_ast_alg f a =
  Ast.print_ast_alg (fun _ () -> ()) (fun f (x,_) -> Format.pp_print_string f x)
		    (fun f (x,_) -> Format.pp_print_string f x) f a
let print_alg = Kappa_printer.alg_expr ?env:None
let print_bool = Ast.print_bool print_alg
let print_cc = Connected_component.print ?sigs:None ~with_id:()
let print_place = Agent_place.print ?sigs:None
let print_transformation = Primitives.Transformation.print ?sigs:None
let print_rule = Kappa_printer.elementary_rule ?env:None
let print_modification = Kappa_printer.modification ?env:None
let print_perturbation = Kappa_printer.perturbation ?env:None
let print_path = Edges.print_path ?sigs:None ?graph:None
let print_injections =
  Rule_interpreter.print_injections ?sigs:None Format.pp_print_int
let print_refined_step =
  Utilities.S.PH.B.PB.CI.Po.K.print_refined_step ~compact:false ?handler:None
