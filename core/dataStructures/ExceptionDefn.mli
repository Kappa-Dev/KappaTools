exception Syntax_Error of string Locality.annot
exception Malformed_Decl of string Locality.annot
exception Internal_Error of string Locality.annot

val warning_buffer : (Locality.t option * (Format.formatter -> unit)) list ref
