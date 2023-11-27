type comparison_op = LTEQ | LT | GT | GTEQ | EQ

let string_of_op = function
  | LTEQ -> "<="
  | LT -> "<"
  | GT -> ">"
  | GTEQ -> ">="
  | EQ -> "="

let string_of_var = Occu1.string_of_trans

type restriction = {
  tests: (Occu1.trans * comparison_op * int) list;
  invertible_assignments: (Occu1.trans * int) list;
  non_invertible_assignments: (Occu1.trans * int) list;
}

let empty_restriction =
  { tests = []; invertible_assignments = []; non_invertible_assignments = [] }

type static = {
  counters: Ckappa_sig.AgentSite_map_and_set.Set.t;
  packs:
    Ckappa_sig.Site_map_and_set.Set.t
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
  backward_pointers:
    Ckappa_sig.Site_map_and_set.Set.t
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
  rule_restrictions:
    restriction Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.t;
  rule_creation:
    (Occu1.trans * int) list list
    Ckappa_sig
    .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .t
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.t;
}

let print_packs parameters handler error (packs, _backward_dependences) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%sPacks (counters)"
      (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.iter parameters error
      (fun parameters error agent a ->
        let error, agent_string =
          Handler.translate_agent parameters error handler agent
        in
        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.iter parameters
          error
          (fun parameters error counter s ->
            let error, counter_string =
              match
                Handler.translate_site parameters error handler agent counter
              with
              | error, Ckappa_sig.Counter s -> error, s
              | error, (Ckappa_sig.Internal _ | Ckappa_sig.Binding _) ->
                Exception.warn parameters error __POS__ Exit "??"
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s  %s.%s"
                (Remanent_parameters.get_prefix parameters)
                agent_string counter_string
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s    "
                (Remanent_parameters.get_prefix parameters)
            in
            Ckappa_sig.Site_map_and_set.Set.fold
              (fun site error ->
                let error, site_string =
                  match
                    Handler.translate_site parameters error handler agent site
                  with
                  | ( error,
                      ( Ckappa_sig.Internal s
                      | Ckappa_sig.Binding s
                      | Ckappa_sig.Counter s ) ) ->
                    error, s
                in
                let () =
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "%s," site_string
                in
                error)
              s error)
          a)
      packs
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  error

let print_restriction parameters _handler error restriction =
  let () =
    if restriction.tests = [] then
      ()
    else (
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s        test: "
          (Remanent_parameters.get_prefix parameters)
      in
      let () =
        List.iter
          (fun (var, op, int) ->
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s%s%i," (string_of_var var) (string_of_op op) int)
          restriction.tests
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ()
    )
  in
  let () =
    if restriction.invertible_assignments = [] then
      ()
    else (
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s        inversible_assignemets: "
          (Remanent_parameters.get_prefix parameters)
      in
      let () =
        List.iter
          (fun (var, int) ->
            if int = 0 then
              ()
            else
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s%s%i," (string_of_var var)
                (if int > 0 then
                   "+="
                 else if int < 0 then
                   "-="
                 else
                   "")
                (if int > 0 then
                   int
                 else
                   -int))
          restriction.invertible_assignments
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ()
    )
  in
  let () =
    if restriction.non_invertible_assignments = [] then
      ()
    else (
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s        non_inversible_assignemets: "
          (Remanent_parameters.get_prefix parameters)
      in
      let () =
        List.iter
          (fun (var, int) ->
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s:=%i," (string_of_var var) int)
          restriction.non_invertible_assignments
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ()
    )
  in
  error

let print_agent_restriction parameters handler error agent_restriction =
  Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
    error
    (fun parameters error site_id restriction ->
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s       Pack %i"
          (Remanent_parameters.get_prefix parameters)
          (Ckappa_sig.int_of_site_name site_id)
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      print_restriction parameters handler error restriction)
    agent_restriction

let print_rule_restriction parameters handler error rule_restriction =
  Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.iter parameters error
    (fun parameters error agent_id agent_restriction ->
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s    Agent %i"
          (Remanent_parameters.get_prefix parameters)
          (Ckappa_sig.int_of_agent_id agent_id)
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      print_agent_restriction parameters handler error agent_restriction)
    rule_restriction

let print_restrictions parameters handler error restrictions =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%srule restrictions (counters)"
      (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error =
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
      error
      (fun parameters error rule_id rule_restriction ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s Rule %i"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.int_of_rule_id rule_id)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        print_rule_restriction parameters handler error rule_restriction)
      restrictions
  in
  error

let print_creations parameters _handler error creations =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%srule creations (counters)"
      (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error =
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
      error
      (fun parameters error rule_id rule_creation ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s Rule %i"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.int_of_rule_id rule_id)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        Ckappa_sig
        .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .iter parameters error
          (fun parameters error (agent_type, counter) creation ->
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s    Agent_type %i Counter %i "
                (Remanent_parameters.get_prefix parameters)
                (Ckappa_sig.int_of_agent_name agent_type)
                (Ckappa_sig.int_of_site_name counter)
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let () =
              List.iter
                (fun list ->
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "%s        "
                      (Remanent_parameters.get_prefix parameters)
                  in
                  let () =
                    List.iter
                      (fun (site, state) ->
                        let () =
                          Loggers.fprintf
                            (Remanent_parameters.get_logger parameters)
                            "%s:=%i,"
                            (Occu1.string_of_trans site)
                            state
                        in
                        ())
                      list
                  in
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters)
                  in
                  ())
                creation
            in
            error)
          rule_creation)
      creations
  in
  error

let print parameters handler error static =
  let packs = static.packs in
  let backward_pointers = static.backward_pointers in
  let error = print_packs parameters handler error (packs, backward_pointers) in
  let restrictions = static.rule_restrictions in
  let error = print_restrictions parameters handler error restrictions in
  let creations = static.rule_creation in
  let error = print_creations parameters handler error creations in
  error
