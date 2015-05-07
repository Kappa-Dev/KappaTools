let print_cc f x = Connected_component.print true !Debug.global_sigs f x
let print_edges f x = Edges.debug_print (*!Debug.global_sigs*) f x
