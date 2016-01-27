type encoding =
| HTML | HTML_Tabular | DOT | TXT | TXT_Tabular

type token =
| String of string
| Breakable_space

type logger =
| DEVNUL
| Formatter of Format.formatter
| Circular_buffer of string Circular_buffers.t ref
| Infinite_buffer of string Infinite_buffers.t ref

let breakable x =
  match
    x
  with
    | HTML_Tabular | HTML | TXT -> true
    | DOT | TXT_Tabular -> false

type t =
  {
    encoding:encoding;
    logger: logger;
    mutable current_line: token list;
  }


let dummy_html_logger =
  {
    encoding = HTML;
    logger = DEVNUL;
    current_line = [];}

let dummy_txt_logger =
  {
    encoding = TXT;
    logger = DEVNUL;
    current_line = []
  }

let fprintf logger =
  match
    logger.logger
  with
  | DEVNUL -> Format.ifprintf Format.std_formatter
  | Formatter fmt -> Format.fprintf fmt
  | Circular_buffer _
  | Infinite_buffer _ ->
    Format.kfprintf
      (fun _ -> logger.current_line <- (String (Format.flush_str_formatter ()))::logger.current_line)
      Format.str_formatter

let print_breakable_space logger =
  if breakable logger.encoding
  then
    match
      logger.logger
    with
  | DEVNUL
  | Formatter _ ->
    fprintf logger "@ "
  | Circular_buffer _
  | Infinite_buffer _ ->
    logger.current_line <- Breakable_space::logger.current_line
  else
    fprintf logger " "

let end_of_line_symbol logger =
  match
    logger.encoding
  with
  | HTML | HTML_Tabular -> "<Br>"
  | DOT | TXT | TXT_Tabular -> ""


let dump_token f x =
   match
     x
   with
   | String s ->
     Format.pp_print_string
       f s
   | Breakable_space ->
     Format.fprintf f "@ " (* Check with Pierre *)

let print_newline logger =
  let () =
    fprintf logger "%s%t"
      (end_of_line_symbol logger)
      (fun f -> Format.pp_print_newline f ())
  in
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer bf ->
    begin
      let bf' =
	Circular_buffers.add
	  (Format.asprintf "%a"
	     (Pp.list
		(fun _ -> ())
		dump_token)
	  (List.rev logger.current_line))
	  !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
    end
  | Infinite_buffer bf ->
      begin
      let bf' =
	Infinite_buffers.add
	  (Format.asprintf "%a"
	     (Pp.list
		(fun _ -> ())
		dump_token)
	     (List.rev logger.current_line))
	  !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
      end

let print_cell logger s =
  let open_cell_symbol,close_cell_symbol =
    match
      logger.encoding
    with
    | HTML_Tabular -> "<TD>","</TD>"
    | TXT_Tabular -> "","\t"
    | HTML | DOT | TXT -> "",""
  in
  fprintf logger "%s%s%s" open_cell_symbol s close_cell_symbol


let close_logger logger =
  let () =
    match
      logger.encoding
    with
    | HTML ->
      fprintf logger "</div>\n</body>\n"
    | HTML_Tabular | DOT | TXT | TXT_Tabular -> ()
  in
  let () =
    match
      logger.logger
    with
    | DEVNUL -> ()
    | Formatter fmt -> Format.pp_print_flush fmt ()
    | Circular_buffer _
    | Infinite_buffer _ -> ()
  in
  ()

let open_logger_from_channel ?mode:(mode=TXT) channel =
  let formatter = Format.formatter_of_out_channel channel in
  {
    logger = Formatter formatter;
     encoding = mode;
     current_line = []
  }

let open_logger_from_formatter ?mode:(mode=TXT) formatter =
    {
      logger = Formatter formatter;
      encoding = mode;
      current_line = []
    }


let open_circular_buffer ?mode:(mode=TXT) ?size:(size=10) () =
  {
    logger = Circular_buffer (ref (Circular_buffers.create size "" ));
    encoding = mode;
    current_line = [];
  }

let open_infinite_buffer ?mode:(mode=TXT) () =
  {
    logger = Infinite_buffer (ref (Infinite_buffers.create 0 ""));
    encoding = mode;
    current_line = [];
  }

let open_row logger =
  fprintf logger "<tr>"

let close_row logger =
  fprintf logger "</tr>"

let formatter_of_logger logger =
  match
    logger.logger
  with
  | Formatter fmt -> Some fmt
  | DEVNUL
  | Circular_buffer _
  | Infinite_buffer _ -> None

let redirect logger fmt =
  {logger with logger = Formatter fmt}

let flush_buffer logger fmt =
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer a -> Circular_buffers.iter (Format.fprintf fmt "%s") !a
  | Infinite_buffer b -> Infinite_buffers.iter (Format.fprintf fmt "%s") !b
