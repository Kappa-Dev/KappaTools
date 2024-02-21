(**
 * print_handler.ml
 * openkappa
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: 2011, the 17th of January
 * Last modification: Time-stamp: <Jan 08 2020>
 * *
 * Pretty printing of Ckappa handler
 *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let trace = false
let local_trace = false

let print_state parameters state =
  match state with
  | Ckappa_sig.Internal a ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%s"
      (Remanent_parameters.get_prefix parameters)
      a
  | Ckappa_sig.Counter a ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%i"
      (Remanent_parameters.get_prefix parameters)
      a
  | Ckappa_sig.Binding Ckappa_sig.C_Free ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%sfree"
      (Remanent_parameters.get_prefix parameters)
  | Ckappa_sig.Binding (Ckappa_sig.C_Lnk_type (a, b)) ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%sagent_type:%s@@site_type:%s"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_name a)
      (Ckappa_sig.string_of_site_name b)

let print_site parameters site =
  match site with
  | Ckappa_sig.Internal a ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%s(internal state)"
      (Remanent_parameters.get_prefix parameters)
      a
  | Ckappa_sig.Counter a ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%s(counter value)"
      (Remanent_parameters.get_prefix parameters)
      a
  | Ckappa_sig.Binding a ->
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%s(binding state)"
      (Remanent_parameters.get_prefix parameters)
      a

let string_of_site _parameters site =
  match site with
  | Ckappa_sig.Internal a -> a ^ "(internal state)"
  | Ckappa_sig.Binding a -> a ^ "(binding state)"
  | Ckappa_sig.Counter a -> a ^ " (counter value)"

let print_handler parameters error handler =
  let log = Remanent_parameters.get_logger parameters in
  let () =
    Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline log in
  let parameters_agent =
    Remanent_parameters.update_prefix parameters "agents:"
  in
  let () =
    Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters_agent)
  in
  let () = Loggers.print_newline log in
  let print_f print_aux parameters error i site () () =
    let parameters =
      Remanent_parameters.update_prefix parameters
        ("site_type:" ^ Ckappa_sig.string_of_site_name i ^ "->")
    in
    let () = print_aux parameters site in
    let () = Loggers.print_newline log in
    error
  in
  let print_state_f print_aux parameters error i state () () =
    let parameters =
      Remanent_parameters.update_prefix parameters
        ("state_id:" ^ Ckappa_sig.string_of_state_index i ^ "->")
    in
    let () = print_aux parameters state in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\n" in
    error
  in
  let error =
    Ckappa_sig.Dictionary_of_agents.iter parameters_agent error
      (fun _parameters error i agent_name () () ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_agent)
            "%sagent_type:%s:%s"
            (Remanent_parameters.get_prefix parameters_agent)
            (Ckappa_sig.string_of_agent_name i)
            agent_name
        in
        let () =
          Loggers.print_newline
            (Remanent_parameters.get_logger parameters_agent)
        in
        error)
      handler.Cckappa_sig.agents_dic
  in
  let parameters_sites =
    Remanent_parameters.update_prefix parameters "sites:"
  in
  let () =
    Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters_sites)
  in
  let () = Loggers.print_newline log in
  let error =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.print
      parameters_sites error
      (fun parameters error a ->
        let error =
          Ckappa_sig.Dictionary_of_sites.iter parameters error
            (print_f print_site) a
        in
        error)
      handler.Cckappa_sig.sites
  in
  let parameters_states =
    Remanent_parameters.update_prefix parameters "states:"
  in
  let () =
    Loggers.fprintf log "%s \n"
      (Remanent_parameters.get_prefix parameters_states)
  in
  let error =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .print parameters_states error
      (fun parameters error a ->
        Ckappa_sig.Dictionary_of_States.iter parameters error
          (print_state_f print_state)
          a)
      handler.Cckappa_sig.states_dic
  in
  let parameters_duals =
    Remanent_parameters.update_prefix parameters "duals:"
  in
  let () =
    Loggers.fprintf log "%s \n"
      (Remanent_parameters.get_prefix parameters_duals)
  in
  let error =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .print parameters_duals error
      (fun parameters error (a, b, (c : Ckappa_sig.c_state)) ->
        let _ =
          Loggers.fprintf log "%sagent_type:%s,site_type:%s,state_id:%s\n"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.string_of_agent_name a)
            (Ckappa_sig.string_of_site_name b)
            (Ckappa_sig.string_of_state_index c)
        in
        error)
      handler.Cckappa_sig.dual
  in
  error

let dot_of_contact_map ?loggers parameters (error : Exception.method_handler)
    handler =
  let parameters_dot =
    match loggers with
    | None -> Remanent_parameters.open_contact_map_file parameters
    | Some loggers -> Remanent_parameters.set_logger parameters loggers
  in
  let _ =
    List.iter
      (fun x ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_dot)
            "%s%s" Headers.dot_comment x
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters_dot)
        in
        ())
      (Headers.head parameters_dot)
  in
  let _ =
    List.iter
      (fun x ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_dot)
            "%s%s" Headers.dot_comment x
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters_dot)
        in
        ())
      Headers.head_contact_map_in_dot
  in
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters_dot)
      "graph G{ \n"
  in
  let _ =
    Ckappa_sig.Dictionary_of_agents.iter parameters_dot error
      (fun parameters_dot error i agent_name () () ->
        let _ =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_dot)
            "subgraph cluster%s {\n"
            (Ckappa_sig.string_of_agent_name i)
        in
        let error, site_dic =
          Misc_sa.unsome
            (Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
               parameters_dot error i handler.Cckappa_sig.sites) (fun error ->
              Exception.warn parameters_dot error __POS__ Exit
                (Ckappa_sig.Dictionary_of_sites.init ()))
        in
        let error =
          Ckappa_sig.Dictionary_of_sites.iter parameters_dot error
            (fun parameters_dot error j site () () ->
              let _ =
                match site with
                | Ckappa_sig.Counter site_name ->
                  if not (Remanent_parameters.get_pure_contact parameters_dot)
                  then (
                    let () =
                      Loggers.fprintf
                        (Remanent_parameters.get_logger parameters_dot)
                        "   %s.%s [style = filled label = \"%s\"  %s color = \
                         %s size = \"5\"]"
                        (Ckappa_sig.string_of_agent_name i)
                        (Ckappa_sig.string_of_site_name j)
                        site_name
                        (Graph_loggers.shape_in_dot
                           (Remanent_parameters.get_counter_site_shape
                              parameters_dot))
                        (Graph_loggers.dot_color_encoding
                           (Remanent_parameters.get_counter_site_color
                              parameters_dot))
                    in
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters_dot)
                  ) else
                    ()
                | Ckappa_sig.Internal site_name ->
                  if not (Remanent_parameters.get_pure_contact parameters_dot)
                  then (
                    let () =
                      Loggers.fprintf
                        (Remanent_parameters.get_logger parameters_dot)
                        "   %s.%s [style = filled label = \"%s\" %s color = %s \
                         size = \"5\"]"
                        (Ckappa_sig.string_of_agent_name i)
                        (Ckappa_sig.string_of_site_name j)
                        site_name
                        (Graph_loggers.shape_in_dot
                           (Remanent_parameters.get_internal_site_shape
                              parameters_dot))
                        (Graph_loggers.dot_color_encoding
                           (Remanent_parameters.get_internal_site_color
                              parameters_dot))
                    in
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters_dot)
                  ) else
                    ()
                | Ckappa_sig.Binding site_name ->
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters_dot)
                      "   %s.%s [style = filled label = \"%s\" %s color = %s \
                       size = \"5\"]"
                      (Ckappa_sig.string_of_agent_name i)
                      (Ckappa_sig.string_of_site_name j)
                      site_name
                      (Graph_loggers.shape_in_dot
                         (Remanent_parameters.get_binding_site_shape
                            parameters_dot))
                      (Graph_loggers.dot_color_encoding
                         (Remanent_parameters.get_binding_site_color
                            parameters_dot))
                  in
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters_dot)
                  in
                  ()
              in
              error)
            site_dic
        in
        let error, n_sites =
          Ckappa_sig.Dictionary_of_sites.last_entry parameters_dot error
            site_dic
        in
        let () =
          if Ckappa_sig.compare_site_name n_sites Ckappa_sig.dummy_site_name < 0
          then (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters_dot)
                "   %s.0 [shape = plaintext label = \"\"]"
                (Ckappa_sig.string_of_agent_name i)
            in
            let () =
              Loggers.print_newline
                (Remanent_parameters.get_logger parameters_dot)
            in
            ()
          )
        in
        let color = Ckappa_sig.get_agent_color n_sites parameters_dot in
        let shape = Ckappa_sig.get_agent_shape n_sites parameters_dot in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_dot)
            "label =  \"%s\";  %s; color = %s" agent_name
            (Graph_loggers.shape_in_dot shape)
            (Graph_loggers.dot_color_encoding color)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters_dot)
        in
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameters_dot) "} ; "
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters_dot)
        in
        error)
      handler.Cckappa_sig.agents_dic
  in
  let _ =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .iter parameters_dot error
      (fun parameters_dot error (i, (j, _k)) (i', j', _k') ->
        if i < i' || (i = i' && j <= j') then (
          let _ =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters_dot)
              "%s.%s -- %s.%s\n"
              (Ckappa_sig.string_of_agent_name i)
              (Ckappa_sig.string_of_site_name j)
              (Ckappa_sig.string_of_agent_name i')
              (Ckappa_sig.string_of_site_name j')
          in
          error
        ) else
          error)
      handler.Cckappa_sig.dual
  in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters_dot) "}" in
  let _ =
    Loggers.print_newline (Remanent_parameters.get_logger parameters_dot)
  in
  let () =
    match loggers with
    | None ->
      Loggers.close_logger (Remanent_parameters.get_logger parameters_dot)
    | Some _ ->
      Loggers.flush_logger (Remanent_parameters.get_logger parameters_dot)
  in
  error

let print_list_of_lines parameters list =
  List.iter
    (fun line ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" line
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ())
    list

let gexf_of_contact_map ?loggers parameters (error : Exception.method_handler)
    handler =
  let parameters_gexf =
    match loggers with
    | None -> Remanent_parameters.open_contact_map_file parameters
    | Some loggers -> Remanent_parameters.set_logger parameters loggers
  in
  let _ =
    print_list_of_lines parameters_gexf
      [
        "<?xml version='1.0' encoding='utf-8'?>";
        "<gexf version=\"1.2\" xmlns=\"http://www.gexf.net/1.2draft\" \
         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
         xsi:schemaLocation=\"http://www.w3.org/2001/XMLSchema-instance\">";
        "<graph defaultedgetype=\"undirected\" mode=\"static\" name=\"\">";
        "  <attributes class=\"node\" mode=\"static\">";
        "    <attribute id=\"0\" title=\"Type\" type=\"string\" />";
        "    <attribute id=\"1\" title=\"size\" type=\"integer\" />";
        "  </attributes>";
        "  <nodes> ";
      ]
  in
  let _ =
    Ckappa_sig.Dictionary_of_agents.iter parameters_gexf error
      (fun parameters_gexf error i agent_name () () ->
        let string_name = agent_name in
        let () =
          print_list_of_lines parameters_gexf
            [
              "     <node id=\"" ^ string_name ^ "\" label=\"" ^ string_name
              ^ "\">";
              "       <attvalues>";
              "         <attvalue for=\"0\" value=\"Agent\" />";
              "         <attvalue for=\"1\" value=\"10\" />";
              "       </attvalues>";
              "     </node>";
            ]
        in
        let error, site_dic =
          Misc_sa.unsome
            (Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
               parameters_gexf error i handler.Cckappa_sig.sites) (fun error ->
              Exception.warn parameters_gexf error __POS__ Exit
                (Ckappa_sig.Dictionary_of_sites.init ()))
        in
        let error =
          Ckappa_sig.Dictionary_of_sites.iter parameters_gexf error
            (fun parameters_gexf error _j site () () ->
              match site with
              | Ckappa_sig.Counter _ | Ckappa_sig.Internal _ -> error
              | Ckappa_sig.Binding site ->
                let site_name = string_name ^ ":" ^ site in
                let () =
                  print_list_of_lines parameters_gexf
                    [
                      "     <node id=\"" ^ site_name ^ "\"  label=\""
                      ^ site_name ^ "\">";
                      "       <attvalues>";
                      "         <attvalue for=\"0\" value=\"Domaine\" />";
                      "         <attvalue for=\"1\" value=\"1\" />";
                      "       </attvalues>";
                      "     </node>";
                    ]
                in
                error)
            site_dic
        in
        error)
      handler.Cckappa_sig.agents_dic
  in
  let () =
    print_list_of_lines parameters_gexf [ "   </nodes>"; "   <edges>" ]
  in
  let error, counter =
    Ckappa_sig.Dictionary_of_agents.fold
      (fun _ ((), ()) agent_name (error, counter) ->
        let string_name = Ckappa_sig.string_of_agent_name agent_name in
        let error, site_dic =
          Misc_sa.unsome
            (Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
               parameters_gexf error agent_name handler.Cckappa_sig.sites)
            (fun error ->
              Exception.warn parameters_gexf error __POS__ Exit
                (Ckappa_sig.Dictionary_of_sites.init ()))
        in
        let error, counter =
          Ckappa_sig.Dictionary_of_sites.fold
            (fun j ((), ()) _ (error, counter) ->
              match j with
              | Ckappa_sig.Counter _ | Ckappa_sig.Internal _ -> error, counter
              | Ckappa_sig.Binding site ->
                let site_name = string_name ^ ":" ^ site in
                let () =
                  print_list_of_lines parameters_gexf
                    [
                      "     <edge id=\"" ^ string_of_int counter
                      ^ "\" source =\"" ^ string_name ^ "\" target=\""
                      ^ site_name ^ "\" weight=\"10\" />";
                    ]
                in
                error, counter + 1)
            site_dic (error, counter)
        in
        error, counter)
      handler.Cckappa_sig.agents_dic (error, 0)
  in
  let _ =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .fold parameters_gexf error
      (fun parameters_gexf error (i, (j, _k)) (i', j', _k') counter ->
        if i < i' || (i = i' && j <= j') then (
          let error, agent_name =
            Handler.translate_agent ~message:"unknown agent type"
              ~ml_pos:(Some __POS__) parameters_gexf error handler i
          in
          let error, site =
            Handler.translate_site parameters_gexf error handler i j
          in
          let error, site_name =
            match site with
            | Ckappa_sig.Internal _ | Ckappa_sig.Counter _ ->
              Exception.warn parameters_gexf error __POS__ Exit ""
            | Ckappa_sig.Binding site_name -> error, site_name
          in

          let error, agent_name' =
            Handler.translate_agent ~message:"unknown agent type"
              ~ml_pos:(Some __POS__) parameters_gexf error handler i'
          in
          let error, site' =
            Handler.translate_site parameters_gexf error handler i' j'
          in
          let error, site_name' =
            match site' with
            | Ckappa_sig.Internal _ | Ckappa_sig.Counter _ ->
              Exception.warn parameters_gexf error __POS__ Exit ""
            | Ckappa_sig.Binding site_name -> error, site_name
          in

          let _ =
            print_list_of_lines parameters_gexf
              [
                "     <edge id=\"" ^ string_of_int counter ^ "\" source =\""
                ^ agent_name ^ ":" ^ site_name ^ "\" target=\"" ^ agent_name'
                ^ ":" ^ site_name' ^ "\" weight=\"1\" />";
              ]
          in
          error, counter + 1
        ) else
          error, counter)
      handler.Cckappa_sig.dual counter
  in
  let _ =
    print_list_of_lines parameters_gexf
      [ "    </edges>"; "  </graph>"; "</gexf>" ]
  in
  let () =
    match loggers with
    | None ->
      Loggers.close_logger (Remanent_parameters.get_logger parameters_gexf)
    | Some _ ->
      Loggers.flush_logger (Remanent_parameters.get_logger parameters_gexf)
  in
  error
