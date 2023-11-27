let convert ~nrules ~nvars influence_map =
  let n = nrules + nvars in
  let _, pos, neg = influence_map in
  let bidirectional_map =
    {
      Remanent_state.positive_influence_fwd = Array.make n [];
      Remanent_state.positive_influence_bwd = Array.make n [];
      Remanent_state.negative_influence_fwd = Array.make n [];
      Remanent_state.negative_influence_bwd = Array.make n [];
    }
  in
  let f store_direct store_reverse map birectional_map =
    Ckappa_sig.PairRule_setmap.Map.fold
      (fun (id1, id2) edge_label bidirectional_map ->
        store_direct id1 id2 edge_label
          (store_reverse id2 id1 edge_label bidirectional_map))
      map birectional_map
  in
  let add get set i im bidirectional_map =
    let old = get i bidirectional_map in
    set i (im :: old) bidirectional_map
  in
  let bidirectional_map =
    f
      (fun i j edge bidirectional_map ->
        add
          (fun i bidirectional_map ->
            bidirectional_map.Remanent_state.positive_influence_fwd.(i))
          (fun i im bidirectional_map ->
            let () =
              bidirectional_map.Remanent_state.positive_influence_fwd.(i) <- im
            in
            bidirectional_map)
          (Ckappa_sig.int_of_rule_id i)
          (j, edge) bidirectional_map)
      (fun i j edge bidirectional_map ->
        add
          (fun i bidirectional_map ->
            bidirectional_map.Remanent_state.positive_influence_bwd.(i))
          (fun i im bidirectional_map ->
            let () =
              bidirectional_map.Remanent_state.positive_influence_bwd.(i) <- im
            in
            bidirectional_map)
          (Ckappa_sig.int_of_rule_id i)
          (j, edge) bidirectional_map)
      pos bidirectional_map
  in
  let bidirectional_map =
    f
      (fun i j edge bidirectional_map ->
        add
          (fun i bidirectional_map ->
            bidirectional_map.Remanent_state.negative_influence_fwd.(i))
          (fun i im bidirectional_map ->
            let () =
              bidirectional_map.Remanent_state.negative_influence_fwd.(i) <- im
            in
            bidirectional_map)
          (Ckappa_sig.int_of_rule_id i)
          (j, edge) bidirectional_map)
      (fun i j edge bidirectional_map ->
        add
          (fun i bidirectional_map ->
            bidirectional_map.Remanent_state.negative_influence_bwd.(i))
          (fun i im bidirectional_map ->
            let () =
              bidirectional_map.Remanent_state.negative_influence_bwd.(i) <- im
            in
            bidirectional_map)
          (Ckappa_sig.int_of_rule_id i)
          (j, edge) bidirectional_map)
      neg bidirectional_map
  in
  bidirectional_map

let dump parameters handler error bidirectional_map =
  let nrules = Handler.nrules parameters error handler in
  let f loggers error map =
    Tools.array_fold_lefti
      (fun i error j ->
        let () = Loggers.fprintf loggers "      %i:" i in
        let () = Loggers.print_newline loggers in
        List.fold_left
          (fun error (node, edge) ->
            let node = Ckappa_sig.int_of_rule_id node in
            let error, s3 =
              let s = Buffer.create 0 in
              let fmt = Format.formatter_of_buffer s in
              let parameters =
                Remanent_parameters.set_logger parameters
                  (Loggers.open_logger_from_formatter fmt)
              in
              let error = Handler.print_labels parameters error handler edge in
              let () = Format.pp_print_flush fmt () in
              let s = Buffer.contents s in
              error, s
            in
            let () =
              Loggers.fprintf loggers "            %s [%s]"
                (if node < nrules then
                   "Rule " ^ string_of_int node
                 else
                   "Var " ^ string_of_int (node - nrules))
                s3
            in
            let () = Loggers.print_newline loggers in
            error)
          error j)
      error map
  in
  let loggers = Remanent_parameters.get_logger parameters in
  let () = Loggers.fprintf loggers "Direct\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let error =
    f loggers error bidirectional_map.Remanent_state.positive_influence_fwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let error =
    f loggers error bidirectional_map.Remanent_state.negative_influence_fwd
  in
  let () = Loggers.fprintf loggers "Reverse\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let error =
    f loggers error bidirectional_map.Remanent_state.positive_influence_bwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let error =
    f loggers error bidirectional_map.Remanent_state.negative_influence_bwd
  in
  error
