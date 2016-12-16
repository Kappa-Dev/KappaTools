
let do_sbml logger f =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.SBML ->
    let () =
      f logger
    in
    ()
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave
  | Loggers.Matlab | Loggers.Maple | Loggers.Json -> ()

let print_sbml logger s =
  do_sbml logger
    (fun logger ->
       Loggers.fprintf logger "%s" s
    )

let break_sbml logger =
  do_sbml logger
    Loggers.print_breakable_space

let line_sbml  logger =
  do_sbml logger
    Loggers.print_newline


let open_box ?options:(options=fun () -> "") logger label =
  let () = print_sbml logger ("<"^label^(options ())^">") in
  break_sbml logger

let close_box logger label =
  let () = print_sbml logger ("</"^label^">") in
  line_sbml logger

let add_box ?options:(options=fun () -> "") logger label cont =
  let () = open_box ~options logger label in
  let () = cont logger in
  let () = Loggers.print_breakable_space logger in
  let () = close_box logger label in
  ()

let dump_species_reference print_chemical_species species_of_species_id compil loggers species =
  let s =
    Format.asprintf
      " species=\"%a\""
      (fun log id ->
         print_chemical_species ?compil log
           (fst (species_of_species_id id)))
      species
  in
  let () = open_box ~options:(fun () -> s) loggers "speciesReference" in
  let () = line_sbml loggers in
  ()

let dump_list_of_species_reference
    print_chemical_species
    species_of_species_id
    compil
    loggers
    list
    =
  List.iter
    (fun s ->
       dump_species_reference
         print_chemical_species species_of_species_id compil loggers s) list
let dump_sbml_reaction
    get_rule
    print_rule_name
    print_chemical_species
    species_of_species_id
    compil
    logger
    reactants
    products
    token_vector
    enriched_rule
    =
  let reaction_id = Loggers.get_fresh_reaction_id logger in
  let label_reaction  = "reaction" in
  let label_list_of_reactants = "listOfReactants" in
  let label_list_of_products = "listOfProducts" in
  let options =
    (fun () -> Format.asprintf
        "id=\"re%i\" name=\"%a\" reversible=\"false\" fast=\"false\" " reaction_id (print_rule_name ?compil) (get_rule enriched_rule))
  in
  let () =
    add_box ~options logger label_reaction
      (fun logger ->
         let () =
           add_box logger label_list_of_reactants
             (fun logger ->
                dump_list_of_species_reference
                  print_chemical_species
                  species_of_species_id
                  compil logger reactants)
         in
         let () =
           add_box logger label_list_of_products
             (fun logger ->
                dump_list_of_species_reference
                print_chemical_species
                species_of_species_id
                compil logger products)
         in
         ())
  in
  ()
