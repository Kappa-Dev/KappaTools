module Pattern:
sig
  val print_cc:
  ?dotnet:bool -> ?full_species:bool ->
  ?sigs:Signature.s -> ?cc_id:int -> with_id:bool ->
  ?symbol_table:Symbol_table.symbol_table ->
  Format.formatter -> Pattern.cc -> unit
end
