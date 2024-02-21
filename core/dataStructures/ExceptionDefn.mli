exception Syntax_Error of string Loc.annoted
exception Malformed_Decl of string Loc.annoted
exception Internal_Error of string Loc.annoted

val warning_buffer : (Loc.t option * (Format.formatter -> unit)) list ref
