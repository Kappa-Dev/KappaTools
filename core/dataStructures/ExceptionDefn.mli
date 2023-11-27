exception Syntax_Error of string Locality.annoted
exception Malformed_Decl of string Locality.annoted
exception Internal_Error of string Locality.annoted

val warning_buffer : (Locality.t option * (Format.formatter -> unit)) list ref
