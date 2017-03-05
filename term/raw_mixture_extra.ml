let trace = false
let trace_print x =
  if trace
  then
    Printf.fprintf stderr "%s\n" x

let declare_bond work ag_pos site bond_id map =
  match Mods.IntMap.find_option bond_id map with
  | None ->
    work, Mods.IntMap.add bond_id [ag_pos,site] map
  | Some old ->
    let site1 = ag_pos,site in
    begin
      match old with
        [site2] ->
        Pattern.new_link work site1 site2,
        Mods.IntMap.add bond_id ((ag_pos,site)::old) map
      | [] | _::_::_ -> assert false
    end

let raw_mixture_to_pattern signature preenv mix unspec =
  let () = trace_print "BEGIN RMIX -> PATTERN" in
  let () =
    Raw_mixture.print
      ~compact:false
      ~created:false
      (*~sigs:signature *)
        Format.str_formatter
        mix
  in
  let s = Format.flush_str_formatter () in
  let () = trace_print s in

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
      let () = trace_print "AUX" in
      let () = trace_print (string_of_int mixture_agent.Raw_mixture.a_type) in
      let pattern_agent,work =
        Pattern.new_node work mixture_agent.Raw_mixture.a_type
      in
      let () = trace_print "NEW NODE OK" in
      let work =
        Tools.array_fold_lefti
          (fun site work state ->
             match state with
             | None -> work
             | Some state ->
               Pattern.new_internal_state work (pattern_agent,site) state)
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
               declare_bond work pattern_agent site i bond_map )
          (work,bond_map)
          mixture_agent.Raw_mixture.a_ports
      in
      aux (ag_id+1) tail (work,bond_map)
  in
  let () = trace_print "STEP1" in
  let work, bond_map = aux 0 mix (work, Mods.IntMap.empty) in
  let () = trace_print "STEP2" in
  let () = trace_print "STEP2'" in
  let (a,_,b,c) = Pattern.finish_new work in
  let () = trace_print "END RMIX -> PATTERN" in
  (a,b,c)

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
       let port = agent.Raw_mixture.a_ports in
       Tools.array_fold_lefti
         (fun _ (map1,map2,set) value ->
         match value
         with
           | Raw_mixture.VAL i ->
             add_map i pos map1,
             add_map pos i map2,
             pos::set
           | Raw_mixture.FREE -> map1,map2,pos::set)
         (map1,map2,set)
         agent.Raw_mixture.a_ports )
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
             List.fold_left
               (fun list pos -> pos::list)
               list
               (Mods.IntMap.find_default [] link map1)
          )
          t  link_list
      in
      let black_listed = Mods.IntSet.add h black_listed in
      aux t black_listed (array.(h)::list)
  in
  match set with
  | [] -> []
  | head::_ ->
    aux [head] Mods.IntSet.empty []






let pattern_to_raw_mixture signature pattern =
  let () = trace_print "BEGIN PATTERN -> RMIX" in
  (*  let () = Pattern.print_cc
      ~sigs:signature
      Format.str_formatter
      pattern
      in*)
  let s = Format.flush_str_formatter () in
  let () = trace_print s in

  let agent_list, site_list =
    Pattern.fold
      (fun ~pos ~agent_type (agent_list,site_list) ->
         (pos,agent_type)::agent_list, site_list)
      (fun ~pos ~site state (agent_list,site_list) ->
         agent_list,
         (pos,site,state)::site_list)
      pattern
      ([],[])
  in
  let bond_map =
    let rec aux tail fresh_bond_id bond_map =
      match tail with
      | [] -> bond_map
      | (_,_,((Pattern.Free | Pattern.UnSpec),_))::tail
         -> aux tail fresh_bond_id bond_map
      | (pos,site,(Pattern.Link (ag_pos',site'),_))::tail ->
        match
          Mods.Int2Map.find_option (ag_pos',site') bond_map
        with
        | None ->
          let () = trace_print "NEW ADDRESS" in
          let () = trace_print (string_of_int ag_pos') in
          let () = trace_print (string_of_int site') in
          let () = trace_print (string_of_int pos) in
          let () = trace_print (string_of_int site) in
          let () = trace_print (string_of_int fresh_bond_id) in
          aux
            tail
            (succ fresh_bond_id)
            (Mods.Int2Map.add (pos,site) fresh_bond_id bond_map)
        | Some i ->
        let () = trace_print "OLD ADDRESS" in
        let () = trace_print (string_of_int i) in

          aux
            tail
            fresh_bond_id
            (Mods.Int2Map.add (pos,site) i bond_map)
    in
    aux site_list 0 Mods.Int2Map.empty
  in
  let agent_type_map = Mods.IntMap.empty in
  let agent_type_map =
    List.fold_left
      (fun map (pos,agent_type) ->
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
             Signature.arity signature ag_type
         in
         let () = trace_print "OK" in
         Array.make n_site (Raw_mixture.FREE, None)
      )
      agent_type_map
  in
  let rec aux tail unspec =
    let () =
      trace_print "OK"
    in
    match tail with
    | [] -> Some (agent_map, unspec)
    | (pos,site,(binding_state,int_state))::tail ->
      match
        binding_state
      with
      | Pattern.UnSpec ->
        let () =
          trace_print (string_of_int pos)
        in
        let () =
          trace_print (string_of_int site)
        in
        let () =
          match
            Mods.IntMap.find_option pos agent_map
          with
          | None -> raise Exit
          | Some array ->
            array.(site)<-(Raw_mixture.FREE, Some int_state)
        in
        let () =
          trace_print "OK"
        in
        aux tail ((pos,site)::unspec)
      | Pattern.Free  ->
        let () =
          trace_print (string_of_int pos)
        in
        let () =
          trace_print (string_of_int site)
        in
        let () =
          match
            Mods.IntMap.find_option pos agent_map
          with
          | None -> raise Exit
          | Some array ->
            array.(site)<-(Raw_mixture.FREE, Some int_state)
        in
        let () =
          trace_print "OK"
        in
        aux tail unspec
      | Pattern.Link _ ->
        begin
          match
          let () =
            trace_print (string_of_int pos)
          in
          let () =
            trace_print (string_of_int site)
          in
          Mods.Int2Map.find_option
            (pos,site)
            bond_map
          with
          | None -> assert false
          | Some i ->
            let () =
              trace_print "OK LINK"
            in
            let () =
              trace_print (string_of_int pos)
            in
            let () =
              trace_print (string_of_int i)
            in
            let () =
              match
                Mods.IntMap.find_option pos agent_map
              with
              | None -> raise Exit
              | Some array ->
                array.(site) <- (Raw_mixture.VAL i, Some int_state)
            in
            aux tail unspec
        end
  in
  match aux site_list [] with
  | None ->
    let () = trace_print "FAILLURE PATTERN -> RMIX" in
    None
  | Some (agent_map, unspec) ->
    begin
      let (),list =
        Mods.IntMap.monadic_fold2
          () ()
          (fun () () _ agent_type intf agent_list ->
            let internal =
              Array.map
                snd
                intf
            in
            let binding =
              Array.map
                fst
                intf
            in
            (),({
              Raw_mixture.a_type = agent_type ;
              a_ports = binding ;
              a_ints = internal ;
              }::agent_list))
          (fun () () _ _ _ -> raise Exit)
          (fun () () _ _ _ -> raise Exit)
          agent_type_map agent_map []
      in
      Some (top_sort list, unspec)
    end
