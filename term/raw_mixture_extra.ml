let trace = false

let do_trace parameters =
  trace ||
    (match parameters with
     | None -> false
     | Some p -> Remanent_parameters.get_trace p)

let trace_print ?parameters x =
  let get_trace, fmt_opt =
    match parameters with
    | None -> false, Some Format.err_formatter
    | Some parameters ->
      Remanent_parameters.get_trace parameters,
      Loggers.formatter_of_logger (Remanent_parameters.get_logger parameters)
  in
  if trace || get_trace
    then
      match fmt_opt with
      | Some fmt ->
        Format.fprintf fmt "%s\n" x
      | None -> ()

let safe_print_str (i,j,k,l) parameters print print2 =
  if do_trace parameters
  then
    try
      let () = print Format.str_formatter in
      let s = Format.flush_str_formatter () in
      let () = trace_print ?parameters s in
      ()
    with
    | _ ->
      let () =
        Format.fprintf
          Format.str_formatter
          "An error has been encountered (%s,%i,%i,%i)\n Dumping while ignoring the signature\n" i j k l
      in
      let s = Format.flush_str_formatter () in
      let () = trace_print ?parameters s in
      let () = print2 Format.str_formatter in
      let s = Format.flush_str_formatter () in
      let () = trace_print ?parameters s in
      ()

let declare_bond work ag_pos site bond_id map =
  match Mods.IntMap.find_option bond_id map with
  | None ->
    work, Mods.IntMap.add bond_id [ag_pos,site] map
  | Some old ->
    let site1 = ag_pos, site in
    begin
      match old with
        [site2] ->
        Pattern.new_link work site1 site2,
        Mods.IntMap.add bond_id ((ag_pos,site)::old) map
      | [] | _ :: _ :: _ -> assert false
    end

let raw_mixture_to_pattern ?parameters ?signature preenv mix unspec =
  let () = trace_print ?parameters "Translation from raw_mixture to pattern" in
  let () = trace_print ?parameters "INPUT:" in
  let () =
    match signature with
    | None -> ()
    | Some sigs ->
      safe_print_str
        __POS__
        parameters
        (fun fmt ->
           Raw_mixture.print ~compact:false ~created:false ~sigs
             fmt mix)
        (fun fmt ->
           Raw_mixture.print ~compact:false ~created:false
             fmt mix)
  in
  let unspec =
    List.fold_left
      (fun map k -> Mods.Int2Set.add k map)
      Mods.Int2Set.empty
      unspec
  in
  let work = Pattern.begin_new preenv in
  let rec aux ag_id tail (work,bond_map) =
    match tail with
    | [] -> work,bond_map
    | mixture_agent::tail ->
      let () = trace_print
          (string_of_int mixture_agent.Raw_mixture.a_type) in
      let pattern_agent,work =
        Pattern.new_node work mixture_agent.Raw_mixture.a_type
      in
      let work =
        Tools.array_fold_lefti
          (fun site work state ->
             match state with
             | None -> work
             | Some state ->
               Pattern.new_internal_state work
                 (pattern_agent, site) state)
          work
          mixture_agent.Raw_mixture.a_ints
      in
      let work,bond_map =
        Tools.array_fold_lefti
          (fun site (work,bond_map) state ->
             match state with
             | Raw_mixture.FREE ->
               if
                 Mods.Int2Set.mem
                   (mixture_agent.Raw_mixture.a_type, site)
                   unspec
               then
                 work, bond_map
               else
                 Pattern.new_free work (pattern_agent,site), bond_map
             | Raw_mixture.VAL i ->
               declare_bond work pattern_agent site i bond_map)
          (work, bond_map)
          mixture_agent.Raw_mixture.a_ports
      in
      aux (ag_id + 1) tail (work, bond_map)
  in
  let work, _bond_map = aux 0 mix (work, Mods.IntMap.empty) in
  let (a, _, b, c) = Pattern.finish_new work in
  let () =
    match signature with
    | None -> ()
    | Some sigs ->
      let () = trace_print ?parameters "OUTPUT:" in
      let () =
        safe_print_str __POS__ parameters
          (fun fmt -> Pattern.print_cc ~sigs fmt b)
          (fun fmt -> Pattern.print_cc fmt b)
      in ()
  in (a, b, c)

let add_map i j map =
let old =
  Mods.IntMap.find_default [] i map
in
Mods.IntMap.add i (j::old) map

let top_sort list =
  let array = Array.of_list list in
  let (map1,map2,set)  =
    Tools.array_fold_lefti
      (fun pos (map1,map2,set) agent ->
       Tools.array_fold_lefti
         (fun _ (map1, map2, set) value ->
         match value with
           | Raw_mixture.VAL i ->
             add_map i pos map1,
             add_map pos i map2,
             pos :: set
           | Raw_mixture.FREE -> map1, map2, pos::set)
         (map1, map2, set)
         agent.Raw_mixture.a_ports)
      (Mods.IntMap.empty,Mods.IntMap.empty,[])
      array
  in
  let rec aux to_do black_listed list =
    match to_do with
    | [] -> List.rev list
    | h::t when Mods.IntSet.mem h black_listed ->
      aux t black_listed list
    | h::t ->
      let link_list =
        Mods.IntMap.find_default [] h map2
      in
      let t =
        List.fold_left
          (fun list link ->
             List.fold_left (fun list pos -> pos :: list) list
               (Mods.IntMap.find_default [] link map1)
          ) t link_list
      in
      let black_listed = Mods.IntSet.add h black_listed in
      aux t black_listed (array.(h)::list)
  in
  match set with
  | [] -> []
  | head :: _ -> aux [head] Mods.IntSet.empty []

let pattern_to_raw_mixture ?parameters sigs pattern =
  let () = trace_print ?parameters
      "Translation from patten to raw_mixture" in
  let () =
    let () = trace_print ?parameters "INPUT:" in
    let () =
      safe_print_str
        __POS__ parameters
        (fun fmt -> Pattern.print_cc ~sigs fmt pattern)
        (fun fmt -> Pattern.print_cc fmt pattern)
      in ()
  in
  let agent_list, site_list =
    Pattern.fold
      (fun ~pos ~agent_type (agent_list, site_list) ->
         (pos, agent_type) :: agent_list, site_list)
      (fun ~pos ~site state (agent_list, site_list) ->
         agent_list, (pos, site, state) :: site_list)
      pattern ([], [])
  in
  let bond_map =
    let rec aux tail fresh_bond_id bond_map =
      match tail with
      | [] -> bond_map
      | (_, _, ((Pattern.Free | Pattern.UnSpec), _)) :: tail
         -> aux tail fresh_bond_id bond_map
      | (pos, site, (Pattern.Link (ag_pos', site'), _)) :: tail ->
        match
          Mods.Int2Map.find_option (ag_pos', site') bond_map
        with
        | None ->
            aux tail (succ fresh_bond_id)
              (Mods.Int2Map.add (pos, site) fresh_bond_id bond_map)
        | Some i ->
        let () = trace_print (string_of_int i) in
          aux
            tail
            fresh_bond_id
            (Mods.Int2Map.add (pos, site) i bond_map)
    in
    aux site_list 0 Mods.Int2Map.empty
  in
  let agent_type_map = Mods.IntMap.empty in
  let agent_type_map =
    List.fold_left
      (fun map (pos, agent_type) ->
         Mods.IntMap.add pos agent_type map)
      agent_type_map
      agent_list
  in
  let agent_map =
    Mods.IntMap.map
      (fun ag_type ->
         let () = trace_print (string_of_int ag_type) in
         let n_site =
           if ag_type = -1
           then
             0
           else
             Signature.arity sigs ag_type
         in
         Array.make n_site (Raw_mixture.FREE, None)
      ) agent_type_map
  in
  let rec aux tail unspec =
    match tail with
    | [] -> Some (agent_map, unspec)
    | (pos, site, (binding_state, int_state)) :: tail ->
      let int_state =
        if int_state = -1
        then None
        else Some int_state
      in
      match binding_state with
      | Pattern.UnSpec ->
        let () = trace_print (string_of_int pos) in
        let () = trace_print (string_of_int site) in
        let () =
          match
            Mods.IntMap.find_option pos agent_map
          with
          | None -> raise Exit
          | Some array ->
            array.(site) <- (Raw_mixture.FREE, int_state)
        in
        let agent_type =
          Mods.IntMap.find_default (-1) pos agent_type_map
        in
        aux tail ((agent_type, site) :: unspec)
      | Pattern.Free  ->
        let () = trace_print (string_of_int pos) in
        let () = trace_print (string_of_int site) in
        let () =
          match
            Mods.IntMap.find_option pos agent_map
          with
          | None -> raise Exit
          | Some array ->
            array.(site) <- (Raw_mixture.FREE, int_state)
        in
        aux tail unspec
      | Pattern.Link _ ->
        begin
          match
            Mods.Int2Map.find_option (pos,site) bond_map
          with
          | None -> assert false
          | Some i ->
            let () =
              match
                Mods.IntMap.find_option pos agent_map
              with
              | None -> raise Exit
              | Some array ->
                array.(site) <- (Raw_mixture.VAL i, int_state)
            in
            aux tail unspec
        end
  in
  match aux site_list [] with
  | None -> None
  | Some (agent_map, unspec) ->
    begin
      let (),list =
        Mods.IntMap.monadic_fold2
          () ()
          (fun () () _ agent_type intf agent_list ->
             let internal = Array.map snd intf in
             let binding = Array.map fst intf in
             (),
             ({
               Raw_mixture.a_type = agent_type ;
               a_ports = binding ;
               a_ints = internal ;
               } :: agent_list))
          (fun () () _ _ agent_list ->
             let () =
               safe_print_str
                 __POS__ parameters (fun _fmt -> raise Exit)
                 (fun _fmt -> ())
                 in
             (), agent_list)
          (fun () () _ _ agent_list ->
             let () =
               safe_print_str
                 __POS__ parameters (fun _fmt -> raise Exit)
                 (fun _fmt -> ())
             in
             (), agent_list)
          agent_type_map agent_map []
      in
      let output = top_sort list in
      let () = trace_print ?parameters "OUTPUT:" in
      let () =
        safe_print_str
          __POS__ parameters
          (fun fmt ->
             Raw_mixture.print ~compact:false ~created:false ~sigs
               fmt output)
          (fun fmt ->
             Raw_mixture.print ~compact:false ~created:false
               fmt output)
      in
      Some (output, unspec)
    end
