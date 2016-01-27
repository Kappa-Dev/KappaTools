type encoding =
| HTML | HTML_Tabular | DOT | TXT | TXT_Tabular
type t

val fprintf: t -> ('a, Format.formatter, unit) format -> 'a
val print_newline: t -> unit
val print_cell: t -> string -> unit
val close_logger: t -> unit
val open_infinite_buffer: ?mode:encoding -> unit -> t
val open_circular_buffer: ?mode:encoding -> ?size:int -> unit -> t
val open_logger_from_formatter: ?mode:encoding -> Format.formatter -> t
val open_logger_from_channel: ?mode:encoding -> out_channel -> t
val open_row: t -> unit
val close_row: t -> unit
val print_breakable_space: t -> unit
val dummy_txt_logger: t
val dummy_html_logger: t
val redirect: t -> Format.formatter -> t
val formatter_of_logger: t -> Format.formatter option
val flush_buffer: t -> Format.formatter -> unit
