type t =
  {
    breakable: bool;
    formatter: Format.formatter option;
    end_of_line_symbol: string;
    open_cell_symbol: string;
    close_cell_symbol: string;
    open_row_symbol: string;
    close_row_symbol: string;
    tab:string;
  }

let dummy_html_logger =
  {
    breakable = true;
    formatter = None ;
    end_of_line_symbol = "<BR>" ;
    open_cell_symbol = "<TD>";
    close_cell_symbol = "</TD>";
    open_row_symbol = "<TR>";
    close_row_symbol = "</TR>";
    tab = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
  }

let dummy_txt_logger =
  {
    breakable = true;
    formatter = None ;
    end_of_line_symbol = "" ;
    open_cell_symbol = "";
    close_cell_symbol = "";
    open_row_symbol = "";
    close_row_symbol = "";
    tab = "";
  }

let fprintf logger =
  match
    logger.formatter
  with
  | None -> Format.ikfprintf (fun _ -> ()) Format.std_formatter
  | Some a -> Format.fprintf a

let print_newline logger =
  fprintf logger "%s%t" logger.end_of_line_symbol (fun f -> if logger.breakable then Format.pp_print_newline f () else ())

let print_cell logger s =
  fprintf logger "%s%s%s" logger.open_cell_symbol s logger.close_cell_symbol


let close_logger logger =
  match
    logger.formatter
  with
  | None -> ()
  | Some a -> Format.pp_print_flush a ()

let open_logger_from_channel ?html_mode:(html_mode=false) channel =
  let dummy =
    if html_mode
    then
      dummy_html_logger
    else
      dummy_txt_logger
  in
  let formatter = Format.formatter_of_out_channel channel in
  {
    dummy
   with
     formatter = Some formatter
  }

let open_logger_from_formatter ?html_mode:(html_mode=false) formatter =
  let dummy =
    if html_mode
    then
      dummy_html_logger
    else
      dummy_txt_logger
  in
  {
    dummy
   with
     formatter = Some formatter
  }

let open_row logger =
  fprintf logger "%s" logger.open_row_symbol

let close_row logger =
  fprintf logger "%s" logger.close_row_symbol

let redirect logger formatter =
  {
    logger
   with
     formatter = Some formatter
  }

let formatter_of_logger logger = logger.formatter
