type break_hint =
    Space | No_space

type symbol_table =
  {
    agent_open : string ;
    agent_close : string ;
    agent_sep_comma : string * break_hint ;
    agent_sep_dot : string * break_hint ;
    agent_sep_plus : string * break_hint ;
    ghost_agent : string ;
    show_ghost : bool ;
    internal_state_symbol : string;
    open_internal_state : string;
    close_internal_state : string;
    open_internal_state_mod : string;
    close_internal_state_mod : string;
    internal_state_mod_symbol: string;
    internal_state_any: string;
    open_binding_state : string;
    close_binding_state : string;
    open_binding_state_mod: string;
    binding_state_mod_symbol: string;
    close_binding_state_mod: string;
    free : string;
    bound : string;
    link_to_any : string;
    link_to_some : string;
    at : string ;
    site_sep : string * break_hint;
    btype_sep : string ;
    uni_arrow : string ;
    rev_arrow : string ;
    bi_arrow : string ;
    uni_arrow_nopoly : string ;
    breakable : bool ;
  }

val symbol_table_V4: symbol_table
val symbol_table_V3: symbol_table
val symbol_table_V3_light: symbol_table
val symbol_table_dotnet: symbol_table
val unbreakable_symbol_table_V4: symbol_table
val unbreakable_symbol_table_V3: symbol_table
val unbreakable_symbol_table_V3_light: symbol_table
val unbreakable_symbol_table_dotnet: symbol_table

val with_dot_and_plus: symbol_table -> symbol_table 
