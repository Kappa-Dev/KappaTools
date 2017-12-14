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
    dotnet_site_sep : string ;
    dotnet_agent_sep_comma : string ;
    dotnet_agent_sep_dot : string ;
    sotnet_agent_set_plus : string 
  }

val symbol_table_V4: symbol_table
val symbol_table_V3: symbol_table
val symbol_table_V3_light: symbol_table
