module Make(I:Ode_interface.Interface) =
struct

  module SpeciesSetMap =
    SetMap.Make
      (struct
        type t = I.chemical_species
        let compare = compare
        let print = I.print_chemical_species
      end)
  module SpeciesSet = SpeciesSetMap.Set
  module SpeciesMap = SpeciesSetMap.Map


  module Store =
    SetMap.Make
      (struct
        type t = I.rule_id * I.connected_component_id
        let compare = compare
        let print a (r,cc) =
          let () = Format.fprintf a "Component_wise:(%a,%a)" I.print_rule_id r I.print_connected_component_id cc  in
          let () = I.print_rule_id a r in
          let () = I.print_connected_component_id a cc in
          ()
      end)

  module StoreMap = Store.Map

  type id = int
  type ode_var_id = id
  type intro_coef_id = id
  type var_id = id
  let fst_id = 1
  let next_id id = id + 1

  type ode_var = Nembed of I.canonic_species | Token of string | Dummy
  type lhs_decl = Init_decl | Var_decl of string | Init_value of ode_var


  module VarSetMap =
    SetMap.Make
      (struct
        type t = ode_var
        let compare = compare
        let print log x =
          match x with
          | Nembed x -> I.print_canonic_species log x
          | Token x -> Format.fprintf log "%s" x
          | Dummy -> ()
      end)
  module VarSet = VarSetMap.Set
  module VarMap = VarSetMap.Map

  type decl =
    | Var of (var_id * string Location.annot option * (ode_var_id, string) Ast.ast_alg_expr Location.annot)
    | Init_expr of var_id * string Location.annot option  * (ode_var_id, string) Ast.ast_alg_expr Location.annot * ode_var_id list
    | Dummy_decl

  type network =
    {
      ode_variables : VarSet.t ;
      reactions: (id list * id list * ((I.connected_component,string) Ast.ast_alg_expr Location.annot * id Location.annot) list * I.rule) list ;

      ode_vars_tab: ode_var Mods.DynArray.t ;
      id_of_ode_var: id VarMap.t ;
      fresh_ode_var_id: id ;

      species_tab: I.chemical_species Mods.DynArray.t ;

      varmap: var_id Mods.StringMap.t ;
      fresh_var_id: var_id ;
      var_declaration: decl list ;
    }


  let get_fresh_var_id network = network.fresh_var_id
  let inc_fresh_var_id network =
    {network with fresh_var_id = next_id network.fresh_var_id}
  let get_fresh_ode_var_id network = network.fresh_ode_var_id
  let inc_fresh_ode_var_id network =
    {network with fresh_ode_var_id = next_id network.fresh_ode_var_id}


  let fold_left_swap f a b =
    List.fold_left
      (fun a b -> f b a)
      b a

  let get_compil = I.get_compil
  let init () =
    {
      reactions = [] ;
      ode_variables = VarSet.empty ;
      ode_vars_tab = Mods.DynArray.create 0 Dummy ;
      id_of_ode_var = VarMap.empty ;
      species_tab = Mods.DynArray.create 0 I.dummy_chemical_species ;
      fresh_ode_var_id = fst_id ;
      fresh_var_id = fst_id ;
      varmap = Mods.StringMap.empty ;
      var_declaration = [];
    }

  let is_known_variable variable network =
    VarSet.mem variable network.ode_variables

  let add_new_var var network =
    let () =
      Mods.DynArray.set
        network.ode_vars_tab
        (get_fresh_ode_var_id network)
        var
    in
    let network =
    { network
      with
        ode_variables = VarSet.add var network.ode_variables ;
        id_of_ode_var = VarMap.add var network.fresh_ode_var_id network.id_of_ode_var ;
    }
    in
    inc_fresh_ode_var_id network,
    get_fresh_ode_var_id network

  let add_new_canonic_species canonic species network =
    let () =
      Mods.DynArray.set
        network.species_tab
        (get_fresh_ode_var_id network)
        species
    in
    add_new_var (Nembed canonic) network

  let add_new_token token network =
    add_new_var (Token token) network

  let enrich_rule rule =
    let lhs = I.lhs rule in
    let lhs_cc = I.connected_components_of_patterns lhs in
    (rule,lhs,lhs_cc)

  let add_embedding key embed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    StoreMap.add key (embed::old_list) store

  let add_embedding_list key lembed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    let new_list =
      fold_left_swap (fun a b -> a::b)
        lembed
        old_list
    in
    StoreMap.add key new_list store

  let translate_canonic_species canonic species remanent =
    let id_opt =
      VarMap.find_option
        (Nembed canonic)
        (snd remanent).id_of_ode_var in
    match
      id_opt
    with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_canonic_species canonic species network
      in
      (species::to_be_visited,network), id
    | Some i -> remanent,i

  let translate_species species remanent =
    translate_canonic_species
      (I.canonic_form species) species remanent

  let translate_token token remanent =
    let id_opt =
      VarMap.find_option
        (Token token) (snd remanent).id_of_ode_var
    in
    match id_opt with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_token token network in
      (to_be_visited, network), id
    | Some i -> remanent, i

  (*  let petrify_canonic_species = translate_canonic_species*)
  let petrify_species species =
    translate_canonic_species (I.canonic_form species) species
  let petrify_species_list l remanent =
    fold_left_swap
      (fun species (remanent,l) ->
         let remanent, i =
           petrify_species species remanent
         in
         remanent,(i::l))
      l
      (remanent,[])

  let petrify_mixture mixture =
    petrify_species_list (I.connected_components_of_mixture mixture)

  let add_to_prefix_list connected_component key prefix_list store acc =
    let list_embeddings =
      StoreMap.find_default [] key store
    in
    List.fold_left
      (fun new_list prefix ->
         List.fold_left
           (fun new_list (embedding,chemical_species) ->
              ((connected_component,embedding,chemical_species)::prefix)::new_list)
           new_list
           list_embeddings
      )
      acc prefix_list

  let add_reaction rule embedding_forest mixture remanent =
    let remanent, reactants = petrify_mixture mixture remanent in
    let products = I.apply rule embedding_forest mixture in
    let tokens = I.token_vector rule in
    let remanent, products = petrify_mixture products remanent in
    let remanent, tokens =
      List.fold_left
        (fun (remanent, tokens) (a,(b,c)) ->
           let remanent, id = translate_token b remanent in
           remanent,(a,(id,c))::tokens)
        (remanent,[])
        tokens
    in
    let to_be_visited, network = remanent in
    let network =
      {
        network
        with reactions = (List.rev reactants, List.rev products, List.rev tokens, rule)::network.reactions
      }
    in
    to_be_visited, network

  let initial_network initial_states =
    List.fold_left
      (fun remanent species -> fst (translate_species species remanent))
      ([], init ())
      initial_states

  let compute_reactions rules initial_states =
    (* Let us annotate the rules with cc decomposition *)
    let rules = List.rev_map enrich_rule (List.rev rules) in
    let to_be_visited, network = initial_network initial_states in
    let store = StoreMap.empty in
    (* store maps each cc in the lhs of a rule to the list of embedding between this cc and a pattern in set\to_be_visited *)
    let rec aux to_be_visited network store =
      match
        to_be_visited
      with
      | []   -> network

      | new_species::to_be_visited ->
        (* add in store the embeddings from cc of lhs to new_species,
           for unary application of binary rule, the dictionary of species is updated, and the reaction entered directly *)
        let store, to_be_visited, network  =
          List.fold_left
            (fun
              (store_old_embeddings, to_be_visited, network)  (rule,lhs,lhs_cc)->
              let rule_id = I.rule_id rule in
              (* regular application of tules, we store the embeddings*)
              let store_new_embeddings =
                List.fold_left
                  (fun store (cc_id, cc) ->
                     let lembed = I.find_embeddings cc new_species in
                     add_embedding_list
                       (rule_id,cc_id)
                       (List.rev_map (fun a -> a,new_species) (List.rev lembed))
                       store
                  )
                  StoreMap.empty
                  lhs_cc
              in
              let (),store_all_embeddings =
                StoreMap.map2_with_logs
                  (fun _ a _ _ _ -> a)
                  ()
                  ()
                  (fun _ _ b -> (),b)
                  (fun _ _ b -> (),b)
                  (fun _ _ b c ->
                     (),List.fold_left
                       (fun list elt -> elt::list)
                       b c)
                  store_old_embeddings
                  store_new_embeddings
              in
              (* compute the embedding betwen lhs and tuple of species that contain at least one occurence of new_species *)
              let _,new_embedding_list =
                List.fold_left
                  (fun (partial_emb_list,partial_emb_list_with_new_species) (cc_id,cc) ->
                     (* First case, we complete with an embedding towards the new_species *)
                     let partial_emb_list =
                       add_to_prefix_list cc (rule_id,cc_id) partial_emb_list store_old_embeddings []
                     in
                     let partial_emb_list_with_new_species =
                       add_to_prefix_list cc (rule_id,cc_id)
                         partial_emb_list
                         store_new_embeddings
                         (add_to_prefix_list cc (rule_id,cc_id) partial_emb_list_with_new_species
                            store_all_embeddings [])
                     in
                     partial_emb_list, partial_emb_list_with_new_species
                  )
                  ([[]],[[]])
                  lhs_cc
              in
              (* compute the corresponding rhs, and put the new species in the working list, and store the corrsponding reactions *)
              let to_be_visited, network =
                List.fold_left
                  (fun remanent list ->
                     let _,embed,mixture = I.disjoint_union list in
                     add_reaction rule embed mixture remanent)
                  (to_be_visited,network)
                  new_embedding_list
              in
              (* unary application of binary rules *)
              let to_be_visited, network =
                if I.binary_rule_that_can_be_applied_in_a_unary_context rule
                then
                  begin
                    let lembed = I.find_embeddings_unary_binary lhs new_species in
                    fold_left_swap
                      (fun embed ->
                         add_reaction rule embed
                           (I.lift_species new_species))
                      lembed
                      (to_be_visited, network)
                  end
                else
                  to_be_visited, network
              in
              store_all_embeddings, to_be_visited, network
            )
            (store, to_be_visited, network)
            rules
        in
        aux to_be_visited network store
    in
    aux to_be_visited network store

  let translate_species species network =
    snd (translate_species species ([],network))

  let translate_token token network =
    snd (translate_token token ([],network))

  let convert_cc connected_component network =
    VarMap.fold
      (fun vars id alg ->
         match vars with
         | Nembed _ ->
           begin
             let species = Mods.DynArray.get network.species_tab id in
             let n_embs =
               List.length
                 (I.find_embeddings connected_component species)
             in
             if n_embs = 0
             then
               alg
             else
               let species = Ast.KAPPA_INSTANCE id in
               let term =
                 if n_embs = 1
                 then
                   species
                 else
                   Ast.BIN_ALG_OP
                     (
                       Operator.MULT,
                       Location.dummy_annot (Ast.CONST (Nbr.I n_embs)),
                       Location.dummy_annot species)
               in
               if alg = Ast.CONST (Nbr.I 0) then term
               else
                 Ast.BIN_ALG_OP
                   (
                     Operator.SUM,
                     Location.dummy_annot alg,
                     Location.dummy_annot term)
           end
         | Token _ | Dummy -> alg

      )
      network.id_of_ode_var
      ((Ast.CONST (Nbr.I 0)))

  let rec convert_alg_expr alg network =
    match
      alg
    with
    | Ast.BIN_ALG_OP (op, arg1, arg2 ),loc ->
      Ast.BIN_ALG_OP (op, convert_alg_expr arg1 network, convert_alg_expr arg2 network),loc
    | Ast.UN_ALG_OP (op, arg),loc ->
      Ast.UN_ALG_OP (op, convert_alg_expr arg network),loc
    | Ast.KAPPA_INSTANCE pattern, loc ->
      let cc = I.connected_components_of_patterns pattern in
      begin
        match cc with
        | [] -> Ast.CONST Nbr.zero
        | (_,h)::t ->
          List.fold_left
            (fun expr (_,h) ->
            Ast.BIN_ALG_OP
              (Operator.MULT,
               Location.dummy_annot expr,
               Location.dummy_annot (convert_cc h network)))
            (convert_cc h network)
            t
      end, loc
    | Ast.TOKEN_ID a, loc -> Ast.TOKEN_ID a, loc
    | Ast.OBS_VAR a, loc -> Ast.OBS_VAR a, loc
    | Ast.CONST a , loc -> Ast.CONST a, loc
    | Ast.STATE_ALG_OP op,loc ->
      Ast.STATE_ALG_OP op,loc

  let convert_initial_state intro network =
    let a,b,c = intro in
    a,
    convert_alg_expr ((*Location.dummy_annot*) b) network,
    match
      fst c
    with
    | Ast.INIT_MIX m ->
      begin
        let cc = I.connected_components_of_mixture m in
        let list =
          List.rev_map
            (fun x -> translate_species x network)
            (List.rev cc)
        in
        list
        end
    | Ast.INIT_TOK token ->
      [translate_token
         token
         network]


  let convert_var_def variable_def network =
    let a,b = variable_def in
    a,convert_alg_expr b network

  let convert_var_defs compil network =
    let list_var = I.get_variables compil in
    let list, network =
      List.fold_left
        (fun (list,network) def ->
         let a,b = convert_var_def def network in
         (Var (get_fresh_var_id network,Some a,b))::list,
         inc_fresh_var_id
           {network with varmap = Mods.StringMap.add (fst a) (get_fresh_var_id network) network.varmap})     
      ([],network)
      list_var
    in
    let list_init = I.get_initial_state compil in
    let init_tab =
      Mods.DynArray.make (get_fresh_ode_var_id network) []
    in
    let add i j =
      Mods.DynArray.set
        init_tab
        i
        (j::(Mods.DynArray.get init_tab i))
    in
    let list, network =
      List.fold_left
        (fun (list,network) def ->
           let a,b,c = convert_initial_state def network in
           let () =
             List.iter
               (fun id -> add id (get_fresh_var_id network))
                c
           in
           (Init_expr (network.fresh_var_id,a,b,c))::list,
           (inc_fresh_var_id network)
        )
        (list,network)
        list_init
    in
    let size = List.length list in
    let npred =
      Mods.DynArray.create (get_fresh_var_id network) 0
    in
    let lsucc =
      Mods.DynArray.create (get_fresh_var_id network) []
    in
    let dec_tab =
      Mods.DynArray.create network.fresh_var_id
        (Dummy_decl,None,Location.dummy_annot (Ast.CONST Nbr.zero))
    in
    let add_succ i j =
      let () = Mods.DynArray.set npred j (1+(Mods.DynArray.get npred j)) in
      let () = Mods.DynArray.set lsucc i (j::(Mods.DynArray.get lsucc i)) in
      ()
    in
    let () =
      List.iter
        (fun decl ->
           match decl
           with
           | Dummy_decl -> ()
           | Init_expr (id,a,b,_)
           | Var (id,a,b) ->
             begin
               let () = Mods.DynArray.set dec_tab id (decl,a,b) in
               let rec aux expr =
                 match expr with
                 | Ast.CONST _,_ -> ()
                 | Ast.BIN_ALG_OP (_,a,b),_ -> (aux a;aux b)
                 | Ast.UN_ALG_OP (_,a),_ -> aux a
                 | Ast.STATE_ALG_OP _,_ -> ()
                 | Ast.OBS_VAR string,_ ->
                   let id' =
                     Mods.StringMap.find_option string
                       network.varmap in
                   begin
                     match id' with
                     | Some id' ->
                       let _ = Printf.fprintf stdout "SOME\n" in
                        add_succ id' id
                     | None ->
                       let _ = Printf.fprintf stdout "None\n" in ()
                   end
                 | Ast.TOKEN_ID _,_ -> ()
                 | Ast.KAPPA_INSTANCE id',_ ->
                   let _ = Printf.fprintf stdout "Kappa INstance\n" in
                   let list =
                     Mods.DynArray.get
                       init_tab
                       id'
                   in
                   List.iter (fun id' -> add_succ id id') list
               in
               aux b
             end
        )
        list
    in
    let _ = Printf.fprintf stdout "INIT TAB\n" in
    let () =
      Mods.DynArray.iteri (fun i j ->
          List.iter (fun j -> Printf.fprintf stdout "%i -> %i \n" j i) j) init_tab
    in
    let _ = Printf.fprintf stdout "SUCC\n " in

    let () =
      Mods.DynArray.iteri (fun i j ->
          List.iter (fun j -> Printf.fprintf stdout "%i -> %i \n" j i) j) lsucc
    in
    let top_sort =
      let clean k to_be_visited =
        let l = Mods.DynArray.get lsucc k in
        List.fold_left
          (fun to_be_visited j ->
             let old = Mods.DynArray.get npred j in
             let () = Mods.DynArray.set npred j (old-1) in
             if old = 1 then j::to_be_visited else to_be_visited)
          to_be_visited l
      in
      let to_be_visited =
        let rec aux k l =
          if k < fst_id
          then l
          else
          if Mods.DynArray.get npred k = 0
          then
            aux (k-1) (k::l)
          else
            aux (k-1) l
        in
        aux (network.fresh_var_id-1) []
      in
      let rec aux to_be_visited l =
        match to_be_visited with
        | [] -> List.rev l
        | h::t -> aux (clean h t) (h::l)
      in
      let l = aux to_be_visited [] in
      let l =
        List.rev_map
          (fun x ->
             let decl,_,_ = Mods.DynArray.get dec_tab x in decl
          ) l
      in l
    in
    let size' = List.length top_sort in
    if size' = size
    then
      {network with var_declaration = top_sort}
    else
      let () = Printf.fprintf stdout "Circular dependencies\n" in
      assert false


  let species_of_initial_state =
    List.fold_left
      (fun list (_,_,(b,_)) ->
         match b with
         | Ast.INIT_MIX b ->
           begin
             List.fold_left
               (fun list a -> a::list)
               list
               (I.connected_components_of_mixture b)
           end
         | Ast.INIT_TOK _ -> list)
      []

  let network_from_compil compil =
    let rules = I.get_rules compil in
    let initial_state = species_of_initial_state (I.get_initial_state compil) in
    let network = compute_reactions rules initial_state in
    let network = convert_var_defs compil network in
    network

  let export_network logger network =
    let handler_expr =
      {
        Ode_loggers.int_of_obs = (fun string  -> Mods.StringMap.find_default 0 string network.varmap) ;
        Ode_loggers.int_of_kappa_instance = (fun i -> i) ;
        Ode_loggers.int_of_token_id = (fun _ -> 0) ;
      }
    in
    let handler_init =
      {
        Ode_loggers.int_of_obs = (fun i  -> i) ;
        Ode_loggers.int_of_kappa_instance = (fun i -> i) ;
        Ode_loggers.int_of_token_id = (fun i -> i) ;
      }
    in
    let is_zero = Mods.DynArray.create (get_fresh_ode_var_id network) true in
    let is_zero x =
      if Mods.DynArray.get is_zero x
      then
        let () = Mods.DynArray.set is_zero x false in
        true
      else
        false
    in
    let increment ?init_mode:(init_mode=false) x =
      if is_zero x
      then
        Ode_loggers.associate ~init_mode logger (Ode_loggers.Init x)
      else
        Ode_loggers.increment ~init_mode logger (Ode_loggers.Init x)
    in
    let affect_var ?init_mode:(init_mode=false) decl =
      match decl with
      | Dummy_decl -> ()
      | Init_expr (id',_comment, expr, list) ->
        begin
          match list with
        | [] -> ()
        | [a] ->
          increment ~init_mode a expr handler_expr
        | _ ->
        let () = Ode_loggers.associate ~init_mode logger (Ode_loggers.Expr id') expr handler_expr in
        List.iter
          (fun id ->
             increment ~init_mode id (Location.dummy_annot (Ast.OBS_VAR (id':int))) handler_init)
          list
        end
      | Var (id,_comment,expr) ->
        Ode_loggers.associate ~init_mode logger (Ode_loggers.Expr id) expr handler_expr

(*  | Init_species (_,id,list) ->
        Ode_loggers.associate logger (Ode_loggers.Init id)
          (match list with
             [] -> Location.dummy_annot (Ast.CONST (Nbr.zero))
           | [a] -> Location.dummy_annot (Ast.OBS_VAR a)
           | h::t ->
             let expr = Location.dummy_annot (Ast.OBS_VAR h)in
             List.fold_left
               (fun expr a ->
                  Location.dummy_annot (
                    Ast.BIN_ALG_OP (Operator.SUM,
                                    Location.dummy_annot (Ast.OBS_VAR a),expr)))
               expr t
          )
          handler*)
    in

    let () = Ode_loggers.open_procedure logger "main" "main" [] in
    let () = Ode_loggers.print_ode_preamble logger () in
    let () = Loggers.print_newline logger in
    let () = Ode_loggers.associate logger Ode_loggers.Tinit (Location.dummy_annot (Ast.CONST Nbr.zero)) handler_expr in
    let () =
      Ode_loggers.associate logger Ode_loggers.Tend
        (Location.dummy_annot (Ast.CONST (Nbr.F
                      (match (*!KaSim.maxTimeValue *) Some 6.
                       with None -> 1.
                          | Some f -> f))))
        handler_expr
    in
    let () = Ode_loggers.associate logger Ode_loggers.InitialStep
        (Location.dummy_annot (Ast.CONST (Nbr.F 0.000001))) handler_expr in
    let () = Ode_loggers.associate logger Ode_loggers.Num_t_points
        (Location.dummy_annot (Ast.CONST (Nbr.I (*!KaSim.pointNumberValue*) 100))) handler_expr in
    let () = Loggers.print_newline logger in
    let () = Ode_loggers.declare_global logger (Ode_loggers.Expr network.fresh_var_id) in
    let () = Ode_loggers.initialize logger (Ode_loggers.Expr network.fresh_var_id) in
    let () = Loggers.print_newline logger in
    let () =
      List.iter
        (affect_var ~init_mode:true)
        network.var_declaration
    in
    let () = Loggers.print_newline logger in
    let () = Ode_loggers.print_license_check logger in
    let () = Loggers.print_newline logger in
    let () = Ode_loggers.print_options logger in
    let () = Loggers.print_newline logger in
    let () = Ode_loggers.close_procedure logger in
    ()


  let species_of_species_id network =
    (fun i -> Mods.DynArray.get network.species_tab i)
  let get_reactions network = network.reactions


end
