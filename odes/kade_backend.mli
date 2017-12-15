module Pattern:
sig
  val print_cc:
  ?full_species:bool ->
  ?sigs:Signature.s -> ?cc_id:Pattern.id -> with_id:bool ->
  ?symbol_table:Symbol_table.symbol_table ->
  Format.formatter -> Pattern.cc -> unit

  val print:
    ?domain:Pattern.Env.t ->
    with_id:bool ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter -> Pattern.id -> unit
end

module Kappa_printer:
sig
  val decompiled_rule:
    full:bool ->
    ?symbol_table:Symbol_table.symbol_table ->
    Model.t -> Format.formatter -> Primitives.elementary_rule -> unit

  val elementary_rule:
    ?env:Model.t ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter -> Primitives.elementary_rule -> unit

end
