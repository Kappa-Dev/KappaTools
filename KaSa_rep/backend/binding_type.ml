let string_of_binding_type
    ?binding_type_symbol:(binding_type_symbol=".")
    ~agent_name 
    ~site_name
  =
  Format.sprintf "%s%s%s" site_name binding_type_symbol agent_name

let print_binding_type
    fmt ?binding_type_symbol:(binding_type_symbol=".")
    ~agent_name ~site_name =
  Format.fprintf fmt "%s"
    (string_of_binding_type ~binding_type_symbol ~agent_name ~site_name)
