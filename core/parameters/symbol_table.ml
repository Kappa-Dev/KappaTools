type break_hint = Space | No_space

type symbol_table = {
  agent_open: string;
  agent_close: string;
  agent_sep_comma: string * break_hint;
  agent_sep_dot: string * break_hint;
  agent_sep_plus: string * break_hint;
  ghost_agent: string;
  show_ghost: bool;
  internal_state_symbol: string;
  open_internal_state: string;
  close_internal_state: string;
  open_internal_state_mod: string;
  close_internal_state_mod: string;
  internal_state_mod_symbol: string;
  internal_state_any: string;
  open_binding_state: string;
  close_binding_state: string;
  missing_binding_state: string;
  open_binding_state_mod: string;
  binding_state_mod_symbol: string;
  close_binding_state_mod: string;
  free: string;
  bound: string;
  link_to_any: string;
  link_to_some: string;
  at: string;
  site_sep: string * break_hint;
  btype_sep: string;
  uni_arrow: string;
  rev_arrow: string;
  bi_arrow: string;
  uni_arrow_nopoly: string;
  rev_arrow_nopoly: string;
  bi_arrow_nopoly: string;
  breakable: bool;
  open_int_interval_inclusive: string;
  open_int_interval_exclusive: string;
  open_int_interval_infinity: string;
  close_int_interval_inclusive: string;
  close_int_interval_exclusive: string;
  close_int_interval_infinity: string;
  int_interval_separator: string;
  plus_infinity: string;
  minus_infinity: string;
  open_counter_state: string;
  open_counterceq: string;
  open_countercgte: string;
  open_countercvar: string;
  open_counterdelta: string;
  open_counterval: string;
  close_counter_state: string;
  close_counterceq: string;
  close_countercgte: string;
  close_countercvar: string;
  close_counterdelta: string;
  close_counterval: string;
  counterceq_symbol: string;
  countercgte_symbol: string;
  countercvar_symbol: string;
  counterdeltaplus_symbol: string;
  counterdeltaminus_symbol: string;
  counterval_symbol: string;
}

let symbol_table_V3 =
  {
    bound = "!";
    open_binding_state = "";
    close_binding_state = "";
    missing_binding_state = "?";
    link_to_any = "?";
    link_to_some = "!_";
    internal_state_symbol = "~";
    open_internal_state = "";
    close_internal_state = "";
    open_internal_state_mod = "";
    close_internal_state_mod = "";
    internal_state_mod_symbol = "/";
    internal_state_any = "";
    open_binding_state_mod = "";
    close_binding_state_mod = "";
    binding_state_mod_symbol = "";
    free = "";
    at = ".";
    agent_open = "(";
    agent_close = ")";
    agent_sep_comma = ",", Space;
    agent_sep_plus = ",", Space;
    agent_sep_dot = ",", Space;
    btype_sep = ".";
    site_sep = ",", No_space;
    ghost_agent = ".";
    show_ghost = false;
    uni_arrow = "->";
    rev_arrow = "<-";
    bi_arrow = "<->";
    uni_arrow_nopoly = "-!->";
    rev_arrow_nopoly = "<-!-";
    bi_arrow_nopoly = "<-!->";
    breakable = true;
    open_int_interval_inclusive = "[";
    open_int_interval_exclusive = "]";
    open_int_interval_infinity = "]";
    plus_infinity = "+oo";
    minus_infinity = "-oo";
    close_int_interval_inclusive = "]";
    close_int_interval_exclusive = "[";
    close_int_interval_infinity = "[";
    int_interval_separator = " .. ";
    open_counter_state = "{";
    open_counterceq = "";
    open_countercgte = "";
    open_countercvar = "";
    open_counterdelta = "";
    open_counterval = "";
    close_counter_state = "}";
    close_counterceq = "";
    close_countercgte = "";
    close_countercvar = "";
    close_counterdelta = "";
    close_counterval = "";
    counterceq_symbol = "=";
    countercgte_symbol = ">=";
    countercvar_symbol = ":";
    counterval_symbol = ":=";
    counterdeltaplus_symbol = "+";
    counterdeltaminus_symbol = "-";
  }

let lighten symbol_table =
  { symbol_table with site_sep = " ", snd symbol_table.site_sep }

let to_dotnet symbol_table =
  {
    symbol_table with
    agent_sep_plus = " +", snd symbol_table.agent_sep_plus;
    agent_sep_dot = ".", No_space;
  }

let symbol_table_V4 =
  {
    bound = "";
    open_binding_state = "[";
    close_binding_state = "]";
    missing_binding_state = "";
    link_to_any = "#";
    link_to_some = "_";
    internal_state_symbol = "";
    open_internal_state = "{";
    close_internal_state = "}";
    internal_state_any = "{#}";
    open_internal_state_mod = "";
    close_internal_state_mod = "";
    internal_state_mod_symbol = "/";
    open_binding_state_mod = "";
    close_binding_state_mod = "";
    binding_state_mod_symbol = "";
    free = ".";
    at = ".";
    agent_open = "(";
    agent_close = ")";
    agent_sep_comma = ",", Space;
    agent_sep_plus = ",", Space;
    agent_sep_dot = ",", Space;
    btype_sep = ".";
    site_sep = ",", No_space;
    ghost_agent = ".";
    show_ghost = true;
    uni_arrow = "->";
    rev_arrow = "<-";
    bi_arrow = "<->";
    uni_arrow_nopoly = "-!->";
    rev_arrow_nopoly = "<-!-";
    bi_arrow_nopoly = "<-!->";
    breakable = true;
    open_int_interval_inclusive = "[";
    open_int_interval_exclusive = "]";
    open_int_interval_infinity = "]";
    plus_infinity = "+oo";
    minus_infinity = "-oo";
    open_counter_state = "{";
    close_int_interval_inclusive = "]";
    close_int_interval_exclusive = "[";
    close_int_interval_infinity = "[";
    int_interval_separator = " .. ";
    open_counterceq = "";
    open_countercgte = "";
    open_countercvar = "";
    open_counterdelta = "";
    open_counterval = "";
    close_counter_state = "}";
    close_counterceq = "";
    close_countercgte = "";
    close_countercvar = "";
    close_counterdelta = "";
    close_counterval = "";
    counterceq_symbol = "=";
    countercgte_symbol = ">=";
    countercvar_symbol = ":";
    counterval_symbol = ":=";
    counterdeltaplus_symbol = "+";
    counterdeltaminus_symbol = "-";
  }

let not_breakable symbol_table = { symbol_table with breakable = false }
let symbol_table_V3_light = lighten symbol_table_V3
let symbol_table_dotnet = to_dotnet symbol_table_V3
let unbreakable_symbol_table_V3 = not_breakable symbol_table_V3
let unbreakable_symbol_table_V4 = not_breakable symbol_table_V4
let unbreakable_symbol_table_V3_light = not_breakable symbol_table_V3_light
let unbreakable_symbol_table_dotnet = not_breakable symbol_table_dotnet

let with_dot_and_plus symbol_table =
  {
    symbol_table with
    agent_sep_plus = unbreakable_symbol_table_dotnet.agent_sep_plus;
    agent_sep_dot = unbreakable_symbol_table_dotnet.agent_sep_dot;
  }
