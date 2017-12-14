type symbol_table =
  {
    bound : string;
    open_binding_state : string;
    close_binding_state : string;
    link_to_any : string;
    link_to_some : string;
    internal_state_symbol : string;
    free : string;
    open_internal_state : string;
    close_internal_state : string;
    at : string ;
    agent_open : string ;
    agent_close : string ;
    site_sep_comma : string ;
    btype_sep : string ;
    agent_sep_comma : string ;
    agent_sep_dot : string ;
    agent_sep_plus : string ;
    ghost_agent : string ;
    show_ghost : bool ;
    uni_arrow : string ;
    rev_arrow : string ;
    bi_arrow : string ;
    uni_arrow_nopoly : string ;
  }

let symbol_table_V3 =
  {
    bound = "!";
    open_binding_state = "";
    close_binding_state = "";
    link_to_any = "?";
    link_to_some = "!_";
    internal_state_symbol = "~";
    open_internal_state = "";
    close_internal_state = "";
    free = "";
    at = "." ;
    agent_open = "(" ;
    agent_close =  ")" ;
    agent_sep_comma = "," ;
    agent_sep_plus = "+" ;
    agent_sep_dot = "." ;
    btype_sep = ".";
    site_sep_comma = "," ;
    ghost_agent = "." ;
    show_ghost = false ;
    uni_arrow = "->" ;
    rev_arrow = "<-" ;
    bi_arrow = "<->" ;
    uni_arrow_nopoly = "-!->"
  }

let symbol_table_V3_light =
  {symbol_table_V3 with site_sep_comma=" "}

let symbol_table_V4 =
  {
    bound = "";
    open_binding_state = "[";
    close_binding_state = "]";
    link_to_any = "#";
    link_to_some = "_";
    internal_state_symbol = "";
    open_internal_state = "{";
    close_internal_state = "}";
    free = ".";
    at = "." ;
    agent_open = "(" ;
    agent_close =  ")" ;
    agent_sep_comma = "," ;
    agent_sep_plus = "+" ;
    agent_sep_dot = "." ;
    btype_sep = ".";
    site_sep_comma = "," ;
    ghost_agent = "." ;
    show_ghost = true ;
    uni_arrow = "->" ;
    rev_arrow = "<-" ;
    bi_arrow = "<->" ;
    uni_arrow_nopoly = "-!->"
  }
