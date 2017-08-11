let convert ~nrules ~nvars influence_map =
  let bidirectional_map =
    {
      Remanent_state.positive_influence_rule_fwd =
        Array.make nrules [];
      Remanent_state.positive_influence_rule_bwd =
        Array.make nrules [];
      Remanent_state.positive_influence_var_fwd =
        Array.make nvars [];
      Remanent_state.positive_influence_var_bwd =
        Array.make nvars [];
      Remanent_state.negative_influence_rule_fwd =
        Array.make nrules [];
      Remanent_state.negative_influence_rule_bwd =
        Array.make nrules [];
      Remanent_state.negative_influence_var_fwd =
        Array.make nvars [];
      Remanent_state.negative_influence_var_bwd =
        Array.make nvars [];
    }
  in
  let f store_direct store_reverse map birectional_map =
    Ckappa_sig.PairRule_setmap.Map.fold
      (fun (id1,id2) edge_label bidirectional_map ->
         store_direct id1 id2 edge_label
           (store_reverse id2 id1 edge_label bidirectional_map))
      map birectional_map
  in
  let add get set i im bidirectional_map =
    let old = get i bidirectional_map in
    set i (im::old) bidirectional_map
  in
  let bidirectional_map =
    f
      (fun i j edge bidirectional_map ->
         let j =
           if Ckappa_sig.compare_rule_id
             j
             (Ckappa_sig.rule_id_of_int nrules) >= 0
           then
             Remanent_state.Var (Ckappa_sig.int_of_rule_id j - nrules)
           else
             Remanent_state.Rule (Ckappa_sig.int_of_rule_id j)
         in
         if Ckappa_sig.compare_rule_id
             i
             (Ckappa_sig.rule_id_of_int nrules) >= 0
         then
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.positive_influence_var_fwd.(i))
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.positive_influence_var_fwd.(i)<-im in
                bidirectional_map
             )
             ((Ckappa_sig.int_of_rule_id i)-nrules)
             (j,edge)
             bidirectional_map
         else
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.positive_influence_rule_fwd.(i))
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.positive_influence_rule_fwd.(i)<-im in
                bidirectional_map
             )
             (Ckappa_sig.int_of_rule_id i)
             (j,edge)
             bidirectional_map)
      (fun i j edge bidirectional_map ->
        let j =
          if Ckappa_sig.compare_rule_id
            j
            (Ckappa_sig.rule_id_of_int nrules) >= 0
          then
            Remanent_state.Var (Ckappa_sig.int_of_rule_id j - nrules)
          else
            Remanent_state.Rule (Ckappa_sig.int_of_rule_id j)
        in
        if
           Ckappa_sig.compare_rule_id
             i
             (Ckappa_sig.rule_id_of_int nrules) >= 0
         then
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.positive_influence_var_bwd.(i))
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.positive_influence_var_bwd.(i)<-im in
                bidirectional_map
             )
             ((Ckappa_sig.int_of_rule_id i)-nrules)
             (j,edge)
             bidirectional_map
         else
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.positive_influence_rule_bwd.(i))
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.positive_influence_rule_bwd.(i)<-im in
                bidirectional_map
             )
             (Ckappa_sig.int_of_rule_id i)
             (j,edge)
             bidirectional_map)
      (fst influence_map)
      bidirectional_map
  in
  let bidirectional_map =
    f
      (fun i j edge bidirectional_map ->
        let j =
          if Ckappa_sig.compare_rule_id
            j
            (Ckappa_sig.rule_id_of_int nrules) >= 0
          then
            Remanent_state.Var (Ckappa_sig.int_of_rule_id j - nrules)
          else
            Remanent_state.Rule (Ckappa_sig.int_of_rule_id j)
        in
        if
           Ckappa_sig.compare_rule_id
             i
             (Ckappa_sig.rule_id_of_int nrules) >= 0
         then
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.negative_influence_var_fwd.(i)
             )
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.negative_influence_var_fwd.(
                  i)<-im in
                bidirectional_map
             )
             ((Ckappa_sig.int_of_rule_id i)-nrules)
             (j,edge)
             bidirectional_map
         else
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.negative_influence_rule_fwd.(i)
             )
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.negative_influence_rule_fwd.(
                    i)<-im in
                bidirectional_map
             )
             (Ckappa_sig.int_of_rule_id i)
             (j,edge)
             bidirectional_map)
      (fun i j edge bidirectional_map ->
         let j =
           if Ckappa_sig.compare_rule_id
               j
               (Ckappa_sig.rule_id_of_int nrules) >= 0
           then
             Remanent_state.Var (Ckappa_sig.int_of_rule_id j - nrules)
           else
             Remanent_state.Rule (Ckappa_sig.int_of_rule_id j)
         in
         if
         Ckappa_sig.compare_rule_id
           i
           (Ckappa_sig.rule_id_of_int nrules) >= 0
         then
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.negative_influence_var_bwd.(i)
             )
             (fun i im bidirectional_map ->
                let () =
                  bidirectional_map.Remanent_state.negative_influence_var_bwd.(
                    i)<-im in
                bidirectional_map
             )
             ((Ckappa_sig.int_of_rule_id i)-nrules)
             (j,edge)
             bidirectional_map
         else
           add
             (fun i bidirectional_map ->
                bidirectional_map.Remanent_state.negative_influence_rule_bwd.(i)
             )
           (fun i im bidirectional_map ->
              let () =
                bidirectional_map.Remanent_state.negative_influence_rule_bwd.(i)<-im in
              bidirectional_map
           )
           (Ckappa_sig.int_of_rule_id i)
           (j,edge)
           bidirectional_map)
    (snd influence_map)
    bidirectional_map
  in bidirectional_map


let dump parameters handler error bidirectional_map =
  let f loggers error map =
    Tools.array_fold_lefti
      (fun i error j ->
         let () = Loggers.fprintf loggers "      %i:" i in
         let () = Loggers.print_newline loggers in
         List.fold_left
           (fun error (node,edge) ->
              let error, s3 =
                let s = Buffer.create 0 in
                let fmt = Format.formatter_of_buffer s in
                let parameters = Remanent_parameters.set_logger parameters
                    (Loggers.open_logger_from_formatter fmt) in
                let error  =
                  Handler.print_labels parameters error handler edge
                in
                let () = Format.pp_print_flush fmt () in
                let s = Buffer.contents s in
                error, s
              in
              let () =
                Loggers.fprintf loggers "            %s [%s]"
                  (match node
                   with
                   | Remanent_state.Rule i -> "Rule "^(string_of_int i)
                   | Remanent_state.Var i -> "Var "^(string_of_int i))
                  s3
              in
              let () =
                Loggers.print_newline loggers
              in
              error
           )
           error j
      )
      error map
  in
  let loggers = Remanent_parameters.get_logger parameters in
  let () = Loggers.fprintf loggers "Direct\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.positive_influence_rule_fwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.positive_influence_var_fwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.negative_influence_rule_fwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.negative_influence_var_fwd
  in
  let () = Loggers.fprintf loggers "Reverse\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.positive_influence_rule_bwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.positive_influence_var_bwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.negative_influence_rule_bwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let error =
    f loggers error
      bidirectional_map.Remanent_state.negative_influence_var_bwd
  in
  error
