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
             edge
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
             edge
             bidirectional_map)
      (fun i j edge bidirectional_map ->
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
             edge
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
             edge
             bidirectional_map)
      (fst influence_map)
      bidirectional_map
  in
  let bidirectional_map =
    f
      (fun i j edge bidirectional_map ->
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
             edge
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
             edge
             bidirectional_map)
      (fun i j edge bidirectional_map ->
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
             edge
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
           edge
           bidirectional_map)
    (snd influence_map)
    bidirectional_map
  in bidirectional_map


let dump loggers bidirectional_map =
  let f loggers map =
    Array.iteri
      (fun i j ->
         let () = Loggers.fprintf loggers "      %i:" i in
         let () = Loggers.print_newline loggers in
         List.iter
           (fun (node,edge) ->
              Loggers.fprintf loggers "            %s [%s]\n"
                "node" (*node*)
                "edge" (*edge*))
           j
      )
      map
  in
  let () = Loggers.fprintf loggers "Direct\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.positive_influence_rule_fwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.positive_influence_var_fwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.negative_influence_rule_fwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.negative_influence_var_fwd
  in
  let () = Loggers.fprintf loggers "Reverse\n" in
  let () = Loggers.fprintf loggers " Positive\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.positive_influence_rule_bwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.positive_influence_var_bwd
  in
  let () = Loggers.fprintf loggers " Negative\n" in
  let () = Loggers.fprintf loggers "  Rules\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.negative_influence_rule_bwd
  in
  let () = Loggers.fprintf loggers "  Variables\n" in
  let () =
    f loggers
      bidirectional_map.Remanent_state.negative_influence_var_bwd
  in
  ()
