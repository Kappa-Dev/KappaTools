let declare_bond ag_pos site bond_id map =
  let old = Mods.IntMap.find_default [] bond_id map in
  Mods.IntMap.add bond_id ((ag_pos,site)::old) map

let raw_mixture_to_pattern preenv mix unspec =
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
      let pattern_agent,work =
        Pattern.new_node work mixture_agent.Raw_mixture.a_type
      in
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
               work,
               declare_bond pattern_agent site i bond_map)
          (work,bond_map)
          mixture_agent.Raw_mixture.a_ports
      in
      aux (ag_id+1) tail (work,bond_map)
  in
  let work, bond_map = aux 0 mix (work, Mods.IntMap.empty) in
  let work =
    Mods.IntMap.fold
      (fun _ l work ->
         match l with
         | [] | [_] | _::_::_::_ -> assert false
                              (* each bond value shall occur exactly twice *)
         | [site1;site2] ->
           Pattern.new_link work site1 site2)
      bond_map
      work
  in
  let (a,_,b,c) = Pattern.finish_new work in
  (a,b,c)

let pattern_to_raw_mixture signature pattern =
  let max_pos, agent_list, site_list =
    Pattern.fold
      (fun ~pos ~agent_type (n,agent_list,site_list) ->
         max pos n,
         (pos-1,agent_type)::agent_list, site_list)
      (fun ~pos ~site state (n,agent_list,site_list) ->
         n,
         agent_list,
         (pos-1,site,state)::site_list)
      pattern
      (-1,[],[])
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
          aux
            tail
            (succ fresh_bond_id)
            (Mods.Int2Map.add (pos,site) fresh_bond_id bond_map)
        | Some i ->
          aux
            tail
            fresh_bond_id
            (Mods.Int2Map.add (pos,site) i bond_map)
    in
    aux site_list 0 Mods.Int2Map.empty
  in
  let agent_type_array = Array.make max_pos (-1) in
  let () =
    List.iter
      (fun (pos,agent_type) -> agent_type_array.(pos)<-agent_type)
      agent_list
  in
  let agent_array =
    Array.map
      (fun ag_type ->
         let n_site =
             Signature.arity signature ag_type
          in
         Array.make n_site (Raw_mixture.FREE, None)
      )
      agent_type_array
  in
  let rec aux tail unspec =
    match tail with
    | [] -> Some (agent_array, unspec)
    | (pos,site,(binding_state,int_state))::tail ->
      match
        binding_state
      with
      | Pattern.UnSpec ->
      let () =
        agent_array.(pos).(site)<-(Raw_mixture.FREE, Some int_state)
      in
      aux tail ((pos,site)::unspec)
      | Pattern.Free  ->
        let () =
          agent_array.(pos).(site)<-(Raw_mixture.FREE, Some int_state)
        in
        aux tail unspec
      | Pattern.Link _ ->
        begin
          match
            Mods.Int2Map.find_option
              (agent_type_array.(pos),site)
              bond_map
          with
          | None ->
            None (* Bound sites shall have a bond id *)
          | Some i ->
            let () =
              agent_array.(pos).(site) <- (Raw_mixture.VAL i, Some int_state)
            in
            aux tail unspec
        end
  in
  match aux site_list [] with
  | None -> None
  | Some (agent_array, unspec) ->
    begin
      let rec aux k agent_list =
        if k<0 then Some (agent_list, unspec)
        else
          let internal =
            Array.map
              snd
              agent_array.(k)
          in
          let binding =
            Array.map
              fst
              agent_array.(k)
          in
          aux (k-1)
            ({
              Raw_mixture.a_type = agent_type_array.(k) ;
              a_ports = binding ;
              a_ints = internal ;
            }::agent_list)
      in aux (max_pos-1) []
    end
