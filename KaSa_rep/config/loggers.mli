type t

val fprintf: t -> ('a, Format.formatter, unit) format -> 'a
val print_newline: t -> unit
val print_cell: t -> string -> unit
val close_logger: t -> unit
val open_logger_from_formatter: ?html_mode:bool -> Format.formatter -> t
val open_logger_from_channel: ?html_mode:bool -> out_channel -> t
val open_row: t -> unit
val close_row: t -> unit

val dummy_txt_logger: t
val dummy_html_logger: t
val redirect: t -> Format.formatter -> t
val formatter_of_logger: t -> Format.formatter option
