module Utils : sig
  val print_binding_type :
    Symbol_table.symbol_table ->
    (int -> Format.formatter -> int -> unit) ->
    (Format.formatter -> int -> unit) ->
    int ->
    Format.formatter ->
    int ->
    unit

  val print_agent_sep_plus :
    Symbol_table.symbol_table -> Format.formatter -> unit
end

module Pattern : sig
  type id = Pattern.id

  val print_cc :
    ?full_species:bool ->
    ?sigs:Signature.s ->
    ?cc_id:Pattern.id ->
    noCounters:bool ->
    with_id:bool ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter ->
    Pattern.cc ->
    unit

  val print :
    ?domain:Pattern.Env.t ->
    noCounters:bool ->
    with_id:bool ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter ->
    Pattern.id ->
    unit
end

module Kappa_printer : sig
  val alg_expr :
    noCounters:bool ->
    ?env:Model.t ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter ->
    (Pattern.id array list, int) Alg_expr.e ->
    unit

  val decompiled_rule :
    noCounters:bool ->
    full:bool ->
    ?symbol_table:Symbol_table.symbol_table ->
    Model.t ->
    Format.formatter ->
    Primitives.elementary_rule ->
    unit

  val elementary_rule :
    noCounters:bool ->
    ?env:Model.t ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter ->
    Primitives.elementary_rule ->
    unit
end

module Model : sig
  val print_ast_rule :
    noCounters:bool ->
    ?env:Model.t ->
    ?symbol_table:Symbol_table.symbol_table ->
    Format.formatter ->
    int ->
    unit
end
