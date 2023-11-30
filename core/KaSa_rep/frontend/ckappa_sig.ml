(**
 * ckappa_sig.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 01/17/2011
 * Last modification: Time-stamp: <May 21 2019>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

module Int_Set_and_Map = Map_wrapper.Make (Mods.IntSetMap)

let local_trace = true
let _ = local_trace

type position = Locality.t
type agent_name = string
type site_name = string
type internal_state = string
type counter_name = string
type counter_state = int
type c_agent_name = int
type c_agent_id = int
type c_site_name = int
type c_state = int
type c_rule_id = int
type c_link_value = int
type c_counter_name = int
type binding_state = Free | Lnk_type of agent_name * site_name

type mixture =
  | SKIP of mixture
  | COMMA of agent * mixture
  | DOT of c_agent_id * agent * mixture
  | PLUS of c_agent_id * agent * mixture
  | EMPTY_MIX

and agent = {
  ag_nme: string;
  ag_intf: interface;
  ag_nme_pos: position; (*; ag_pos:position*)
}

and interface =
  | EMPTY_INTF
  | PORT_SEP of port * interface
  | COUNTER_SEP of counter * interface

and port = {
  port_nme: string;
  port_int: internal;
  port_lnk: link;
  (*port_pos: position ;*)
  port_free: bool option;
}

and counter = {
  count_nme: string;
  count_test: counter_test option;
  count_delta: int option;
}

and counter_test = CEQ of int | CGTE of int | CVAR of string | UNKNOWN
and internal = string option list

and link =
  | LNK_VALUE of (c_agent_id * agent_name * site_name * c_link_value * position)
  | FREE
  | LNK_ANY of position
  | LNK_SOME of position
  | LNK_TYPE of (string Locality.annot * string Locality.annot)
  | LNK_MISSING

let rec skip_only mix =
  match mix with
  | EMPTY_MIX -> true
  | SKIP mix -> skip_only mix
  | COMMA _ | DOT _ | PLUS _ -> false

type direction = Direct | Reverse

type 'pattern rule = {
  position: Locality.t;
  prefix: int;
  interprete_delta: direction;
  delta: int;
  (* to go from Ckappa id to KaSim id: *)
  (* in direct mode:
        substract delta to agents with id >= prefix in the rhs *)
  (* in reverse mode:
        substract delta to agents with id >= prefix in the lhs *)
  lhs: 'pattern;
  rhs: 'pattern;
  k_def: ('pattern, string) Alg_expr.e Locality.annot;
  k_un: ('pattern, string) Alg_expr.e Locality.annot option;
  ast: string;
  ast_no_rate: string;
  original_ast: string;
  original_ast_no_rate: string;
  from_a_biderectional_rule: bool;
}

type ('pattern, 'rule) perturbation =
  ('pattern, 'pattern, string, 'rule) Ast.perturbation

type ('pattern, 'rule) modif_expr =
  ('pattern, 'pattern, string, 'rule) Ast.modif_expr

type 'pattern variable = ('pattern, string) Ast.variable_def

type ('agent, 'pattern, 'mixture, 'rule) compil =
  ('agent, 'pattern, 'mixture, string, 'rule) Ast.compil

type ('a, 'b, 'c) site_type = Internal of 'a | Binding of 'b | Counter of 'c
type site = (site_name, site_name, site_name) site_type
type state = (internal_state, binding_state, counter_state) site_type

(****************************************************************************)

let rule_id_to_json x = `Assoc [ "rule_id", `Int x ]

let rule_id_of_json json =
  match json with
  | `Assoc [ (s, json) ] when s = "rule_id" -> Yojson.Basic.Util.to_int json
  | _ ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "rule id", json))

let write_c_rule_id ob f = Yojson.Basic.to_buffer ob (rule_id_to_json f)

let string_of_c_rule_id ?(len = 1024) x =
  let ob = Buffer.create len in
  write_c_rule_id ob x;
  Buffer.contents ob

let read_c_rule_id p lb =
  rule_id_of_json (Yojson.Basic.from_lexbuf ~stream:true p lb)

let c_rule_id_of_string s =
  read_c_rule_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let dummy_agent_name = 0
let dummy_site_name = 0
let dummy_state_index = 0
let dummy_rule_id = 0
let dummy_agent_id = 0
let dummy_site_name_1 = 1
let dummy_site_name_minus1 = -1 (*REMOVE:Use in views_domain*)
let dummy_state_index_1 = 1

let dummy_agent =
  { ag_nme = ""; ag_intf = EMPTY_INTF; ag_nme_pos = Locality.dummy }

let dummy_link_value = 1
let fst_site = 1
let snd_site = 2
let string_of_agent_name (a : c_agent_name) : string = string_of_int a
let int_of_agent_name (a : c_agent_name) : int = a
let agent_name_of_int (a : int) : c_agent_name = a
let _int_of_lnk_value (a : c_link_value) : int = a
let lnk_value_of_int (a : int) : c_link_value = a
let next_lnk_value (i : c_link_value) : c_link_value = i + 1
let site_name_of_int (a : int) : c_site_name = a
let int_of_site_name (a : c_site_name) : int = a
let string_of_site_name (a : c_site_name) : string = string_of_int a
let state_index_of_int (a : int) : c_state = a
let int_of_state_index (a : c_state) : int = a
let string_of_state_index (a : c_state) : string = string_of_int a

let string_of_state_index_option_min parameters a =
  match a with
  | Some a -> string_of_state_index a
  | None -> Remanent_parameters.get_minus_infinity_symbol parameters

let string_of_state_index_option_max parameters a =
  match a with
  | Some a -> string_of_state_index a
  | None -> Remanent_parameters.get_plus_infinity_symbol parameters

let int_of_rule_id (a : c_rule_id) : int = a
let rule_id_of_int (a : int) : c_rule_id = a
let string_of_rule_id (a : c_rule_id) : string = string_of_int a
let int_of_agent_id (a : c_agent_id) : int = a
let agent_id_of_int (a : int) : c_agent_id = a
let string_of_agent_id (a : c_agent_id) : string = string_of_int a
let string_of_c_link_value (a : c_link_value) : string = string_of_int a

let get_agent_shape n_sites parameters =
  Misc_sa.fetch_array (int_of_site_name n_sites)
    (Remanent_parameters.get_agent_shape_array parameters)
    (Remanent_parameters.get_agent_shape_def parameters)

let get_agent_color n_sites parameters =
  Misc_sa.fetch_array (int_of_site_name n_sites)
    (Remanent_parameters.get_agent_color_array parameters)
    (Remanent_parameters.get_agent_color_def parameters)

(***************************************************************)
(*RENAME*)
(***************************************************************)

let rename_link parameters error f link =
  match link with
  | LNK_VALUE (ag, x, y, value, position) ->
    let error, ag = f parameters error ag in
    error, LNK_VALUE (ag, x, y, value, position)
  | LNK_MISSING | FREE | LNK_ANY _ | LNK_SOME _ | LNK_TYPE _ -> error, link

let rename_port parameters error f port =
  let error, port_lnk = rename_link parameters error f port.port_lnk in
  error, { port with port_lnk }

let rec rename_interface parameters error f interface =
  match interface with
  | EMPTY_INTF -> error, EMPTY_INTF
  | COUNTER_SEP (counter, interface) ->
    let error, interface = rename_interface parameters error f interface in
    error, COUNTER_SEP (counter, interface)
  | PORT_SEP (port, interface) ->
    let error, port = rename_port parameters error f port in
    let error, interface = rename_interface parameters error f interface in
    error, PORT_SEP (port, interface)

let rename_agent parameters error f agent =
  let error, interface = rename_interface parameters error f agent.ag_intf in
  error, { agent with ag_intf = interface }

let rename_mixture parameters error f mixture =
  let rec aux parameters error f pos mixture =
    match mixture with
    | SKIP m ->
      let error, map, dot, plus = aux parameters error f (pos + 1) m in
      error, map, dot, plus
    | COMMA (agent, m) ->
      let error, agent = rename_agent parameters error f agent in
      let error, m, dot, plus = aux parameters error f (pos + 1) m in
      let error, pos = f parameters error pos in
      error, Mods.IntMap.add pos agent m, dot, plus
    | DOT (id, agent, mixture) ->
      let error, id = f parameters error id in
      let error, agent = rename_agent parameters error f agent in
      let error, m, dot, plus = aux parameters error f (pos + 1) mixture in
      let error, pos = f parameters error pos in
      let min, max =
        if compare id pos < 0 then
          id, pos
        else
          pos, id
      in
      error, Mods.IntMap.add pos agent m, Mods.IntMap.add min max dot, plus
    | PLUS (id, agent, mixture) ->
      let error, id = f parameters error id in
      let error, agent = rename_agent parameters error f agent in
      let error, m, dot, plus = aux parameters error f (pos + 1) mixture in
      let error, pos = f parameters error pos in
      let min, max =
        if compare id pos < 0 then
          id, pos
        else
          pos, id
      in
      error, Mods.IntMap.add pos agent m, dot, Mods.IntMap.add min max plus
    | EMPTY_MIX ->
      error, Mods.IntMap.empty, Mods.IntMap.empty, Mods.IntMap.empty
  in
  let error, m, dot, plus = aux parameters error f 0 mixture in
  (* first agent has id 0 ???*)
  let list_m = Mods.IntMap.bindings m in
  let dot = Mods.IntMap.bindings dot in
  let plus = Mods.IntMap.bindings plus in
  let rec aux parameters error pos list_m dot plus =
    match list_m with
    | [] -> error, EMPTY_MIX
    | (pos', agent) :: tail ->
      if compare pos pos' >= 0 then (
        let opt1, dot =
          match dot with
          | (pos', pos'') :: q when pos = pos' -> Some pos'', q
          | _ -> None, dot
        in
        let opt2, plus =
          match plus with
          | (pos', pos'') :: q when pos = pos' -> Some pos'', q
          | _ -> None, plus
        in
        let error, mixture = aux parameters error (pos + 1) tail dot plus in
        match opt1, opt2 with
        | Some _, Some _ ->
          Exception.warn parameters error __POS__ Exit (SKIP mixture)
        | Some pos'', None -> error, DOT (pos'', agent, mixture)
        | None, Some pos'' -> error, PLUS (pos'', agent, mixture)
        | None, None -> error, COMMA (agent, mixture)
      ) else (
        let error, mixture = aux parameters error (pos + 1) list_m dot plus in
        error, SKIP mixture
      )
  in
  aux parameters error 0 list_m dot plus

(***************************************************************)
(*JOIN*)
(***************************************************************)

let join_link parameters error link1 link2 =
  if link1 = link2 then
    error, link1
  else (
    match link1, link2 with
    | (LNK_ANY _ | LNK_MISSING), _ -> error, link2
    | _, (LNK_ANY _ | LNK_MISSING) -> error, link1
    | FREE, _ | _, FREE ->
      Exception.warn parameters error __POS__ Exit (LNK_ANY Locality.dummy)
    | LNK_SOME _, _ -> error, link2
    | _, LNK_SOME _ -> error, link1
    | LNK_TYPE ((a, _), (b, _)), LNK_TYPE ((a', _), (b', _))
      when a = a' && b = b' ->
      error, link1
    | LNK_TYPE _, LNK_TYPE _ ->
      Exception.warn parameters error __POS__ Exit (LNK_ANY Locality.dummy)
    | LNK_VALUE (_, x, y, _, _), LNK_TYPE ((a, _), (b, _)) when x = a && b = y
      ->
      error, link1
    | LNK_TYPE ((a, _), (b, _)), LNK_VALUE (_, x, y, _, _) when x = a && b = y
      ->
      error, link2
    | LNK_VALUE (ag, x, y, _, _), LNK_VALUE (ag', x', y', _, _)
      when ag = ag' && x = x' && y = y' ->
      error, link1
    | (LNK_VALUE _ | LNK_TYPE _), (LNK_VALUE _ | LNK_TYPE _) ->
      Exception.warn parameters error __POS__ Exit (LNK_ANY Locality.dummy)
  )

let join_port parameters error port1 port2 =
  if
    port1.port_nme = port2.port_nme
    && port1.port_int = port2.port_int
    && port1.port_free = port2.port_free
  then (
    let error, lnk = join_link parameters error port1.port_lnk port2.port_lnk in
    error, { port1 with port_lnk = lnk }
  ) else
    Exception.warn parameters error __POS__ Exit port1

let join_counter_test parameters error test1 test2 =
  match test1, test2 with
  | None, None -> error, None
  | None, _ -> error, test2
  | _, None -> error, test1
  | Some a, Some b when a = b -> error, test1
  | Some _, Some _ -> Exception.warn parameters error __POS__ Exit test1

let join_counter parameters error counter1 counter2 =
  if
    counter1.count_nme = counter2.count_nme
    && counter1.count_delta = counter2.count_delta
  then (
    let error, test =
      join_counter_test parameters error counter1.count_test counter2.count_test
    in
    error, { counter1 with count_test = test }
  ) else
    Exception.warn parameters error __POS__ Exit counter1

type interface_elt = Port of port | CounterP of counter

let rev_list_of_interface x =
  let rec aux x output =
    match x with
    | PORT_SEP (port, interface) -> aux interface (Port port :: output)
    | COUNTER_SEP (counter, interface) ->
      aux interface (CounterP counter :: output)
    | EMPTY_INTF -> output
  in
  aux x []

let rev_interface_of_list x =
  let rec aux list x =
    match list with
    | [] -> x
    | Port t :: q -> aux q (PORT_SEP (t, x))
    | CounterP t :: q -> aux q (COUNTER_SEP (t, x))
  in
  aux x EMPTY_INTF

let _list_of_interface x = List.rev (rev_list_of_interface x)

let join_interface parameters error interface1 interface2 =
  let rec collect interface map_ports map_counters =
    match interface with
    | EMPTY_INTF -> map_ports, map_counters
    | COUNTER_SEP (counter, interface) ->
      let map_counters =
        Mods.StringMap.add counter.count_nme counter map_counters
      in
      collect interface map_ports map_counters
    | PORT_SEP (port, interface) ->
      let map_ports = Mods.StringMap.add port.port_nme port map_ports in
      collect interface map_ports map_counters
  in
  let map_ports_1, map_counters_1 =
    collect interface1 Mods.StringMap.empty Mods.StringMap.empty
  in
  let map_ports_2, map_counters_2 =
    collect interface2 Mods.StringMap.empty Mods.StringMap.empty
  in
  let error, map_ports_3 =
    Mods.StringMap.monadic_fold2 parameters error
      (fun parameters error key port1 port2 map ->
        let error, port = join_port parameters error port1 port2 in
        error, Mods.StringMap.add key port map)
      (fun _parameters error key port map ->
        error, Mods.StringMap.add key port map)
      (fun _parameters error key port map ->
        error, Mods.StringMap.add key port map)
      map_ports_1 map_ports_2 Mods.StringMap.empty
  in
  let error, map_counters_3 =
    Mods.StringMap.monadic_fold2 parameters error
      (fun parameters error key counter1 counter2 map ->
        let error, counter = join_counter parameters error counter1 counter2 in
        error, Mods.StringMap.add key counter map)
      (fun _parameters error key counter map ->
        error, Mods.StringMap.add key counter map)
      (fun _parameters error key counter map ->
        error, Mods.StringMap.add key counter map)
      map_counters_1 map_counters_2 Mods.StringMap.empty
  in
  let list_ports = Mods.StringMap.bindings map_ports_3 in
  let list_ports = List.rev_map (fun (_, a) -> Port a) list_ports in
  let list_counters = Mods.StringMap.bindings map_counters_3 in
  let list =
    List.fold_left (fun l (_, b) -> CounterP b :: l) list_ports list_counters
  in
  error, rev_interface_of_list list

let join_agent parameters error agent1 agent2 =
  if agent1.ag_nme = agent2.ag_nme then (
    let error, interface =
      join_interface parameters error agent1.ag_intf agent2.ag_intf
    in
    error, { agent1 with ag_intf = interface }
  ) else
    Exception.warn parameters error __POS__
      ?message:(Some (agent1.ag_nme ^ agent2.ag_nme))
      Exit dummy_agent

let rec join_mixture parameters error mixture1 mixture2 =
  match mixture1, mixture2 with
  | EMPTY_MIX, _ -> error, mixture2
  | _, EMPTY_MIX -> error, mixture1
  | SKIP m, SKIP m' ->
    let error, m'' = join_mixture parameters error m m' in
    error, SKIP m''
  | SKIP m, COMMA (ag, m') | COMMA (ag, m), SKIP m' ->
    let error, m'' = join_mixture parameters error m m' in
    error, COMMA (ag, m'')
  | SKIP m, DOT (id, ag, m') | DOT (id, ag, m), SKIP m' ->
    let error, m'' = join_mixture parameters error m m' in
    error, DOT (id, ag, m'')
  | SKIP m, PLUS (id, ag, m') | PLUS (id, ag, m), SKIP m' ->
    let error, m'' = join_mixture parameters error m m' in
    error, PLUS (id, ag, m'')
  | COMMA (ag, m), COMMA (ag', m') ->
    let error, ag = join_agent parameters error ag ag' in
    let error, m'' = join_mixture parameters error m m' in
    error, COMMA (ag, m'')
  | DOT _, _ | PLUS _, _ | _, DOT _ | _, PLUS _ ->
    Exception.warn parameters error __POS__ Exit EMPTY_MIX

let add_agent parameters error agent_id agent_name mixture =
  let agent = { dummy_agent with ag_nme = agent_name } in
  let k = int_of_agent_id agent_id in
  let rec aux k mixture =
    match mixture with
    | SKIP mixture' ->
      let error, mixture'' = aux (k - 1) mixture' in
      if k = 0 then
        error, COMMA (agent, mixture'')
      else
        error, SKIP mixture'
    | COMMA (agent, mixture') ->
      if k = 0 then
        Exception.warn parameters error __POS__ Exit mixture
      else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, COMMA (agent, mixture'')
      )
    | DOT (id, agent, mixture') ->
      if k = 0 then
        Exception.warn parameters error __POS__ Exit mixture
      else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, DOT (id, agent, mixture'')
      )
    | PLUS (id, agent, mixture') ->
      if k = 0 then
        Exception.warn parameters error __POS__ Exit mixture
      else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, PLUS (id, agent, mixture'')
      )
    | EMPTY_MIX ->
      let rec aux2 k =
        if k = 0 then
          COMMA (agent, EMPTY_MIX)
        else (
          let mixture' = aux2 (k - 1) in
          SKIP mixture'
        )
      in
      error, aux2 k
  in
  aux k mixture

let mod_agent_gen parameters error agent_id f mixture =
  let k = int_of_agent_id agent_id in
  let rec aux k mixture =
    match mixture with
    | SKIP mixture' ->
      if k = 0 then
        Exception.warn parameters error __POS__ Exit mixture
      else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, SKIP mixture''
      )
    | COMMA (agent, mixture') ->
      if k = 0 then (
        let error, agent = f parameters error agent in
        error, COMMA (agent, mixture')
      ) else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, COMMA (agent, mixture'')
      )
    | DOT (id, agent, mixture') ->
      if k = 0 then (
        let error, agent = f parameters error agent in
        error, DOT (id, agent, mixture')
      ) else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, DOT (id, agent, mixture'')
      )
    | PLUS (id, agent, mixture') ->
      if k = 0 then (
        let error, agent = f parameters error agent in
        error, PLUS (id, agent, mixture')
      ) else (
        let error, mixture'' = aux (k - 1) mixture' in
        error, PLUS (id, agent, mixture'')
      )
    | EMPTY_MIX -> Exception.warn parameters error __POS__ Exit mixture
  in
  aux k mixture

let rec has_site x interface =
  match interface with
  | EMPTY_INTF -> false
  | COUNTER_SEP (_, intf) -> has_site x intf
  | PORT_SEP (p, intf) ->
    if p.port_nme = x then
      true
    else
      has_site x intf

let rec has_counter x interface =
  match interface with
  | EMPTY_INTF -> false
  | PORT_SEP (_, intf) -> has_site x intf
  | COUNTER_SEP (c, intf) ->
    if c.count_nme = x then
      true
    else
      has_counter x intf

let add_site parameters error agent_id site_name mixture =
  mod_agent_gen parameters error agent_id
    (fun _parameters error agent ->
      if has_site site_name agent.ag_intf then
        error, agent
      else (
        let port =
          {
            port_nme = site_name;
            port_lnk = LNK_ANY Locality.dummy;
            port_int = [];
            port_free = None;
          }
        in
        let interface = PORT_SEP (port, agent.ag_intf) in
        error, { agent with ag_intf = interface }
      ))
    mixture

let add_counter parameters error agent_id counter_name mixture =
  mod_agent_gen parameters error agent_id
    (fun _parameters error agent ->
      if has_counter counter_name agent.ag_intf then
        error, agent
      else (
        let counter =
          { count_nme = counter_name; count_test = None; count_delta = None }
        in
        let interface = COUNTER_SEP (counter, agent.ag_intf) in
        error, { agent with ag_intf = interface }
      ))
    mixture

let mod_site_gen parameters error agent_id site_name f mixture =
  mod_agent_gen parameters error agent_id
    (fun parameters error agent ->
      let rec aux interface =
        match interface with
        | EMPTY_INTF -> error, EMPTY_INTF
        | COUNTER_SEP (counter, intf) ->
          let error, intf = aux intf in
          error, COUNTER_SEP (counter, intf)
        | PORT_SEP (port, intf) ->
          if port.port_nme = site_name then (
            let error, port = f parameters error port in
            error, PORT_SEP (port, intf)
          ) else (
            let error, intf = aux intf in
            error, PORT_SEP (port, intf)
          )
      in
      let error, interface = aux agent.ag_intf in
      error, { agent with ag_intf = interface })
    mixture

let add_binding_state parameters error agent_id site_name p state bool_opt
    mixture =
  mod_site_gen parameters error agent_id site_name
    (fun parameters error port ->
      let error, b = p parameters error port.port_lnk in
      if b then
        error, { port with port_lnk = state; port_free = bool_opt }
      else
        Exception.warn parameters error __POS__ Exit port)
    mixture

let add_free parameters error agent_id site_name mixture =
  add_binding_state parameters error agent_id site_name
    (fun _parameters error lnk ->
      match lnk with
      | LNK_MISSING | LNK_ANY _ -> error, true
      | FREE | LNK_VALUE _ | LNK_SOME _ | LNK_TYPE _ -> error, false)
    FREE (Some true) mixture

let add_binding_type parameters error agent_id site_name agent_name' site_name'
    mixture =
  add_binding_state parameters error agent_id site_name
    (fun _parameters error lnk ->
      match lnk with
      | LNK_MISSING | LNK_SOME _ | LNK_ANY _ -> error, true
      | FREE | LNK_VALUE _ | LNK_TYPE _ -> error, false)
    (LNK_TYPE (Locality.dummy_annot agent_name', Locality.dummy_annot site_name'))
    (Some false) mixture

let add_bound parameters error agent_id site_name mixture =
  add_binding_state parameters error agent_id site_name
    (fun _parameters error lnk ->
      match lnk with
      | LNK_MISSING | LNK_ANY _ -> error, true
      | LNK_SOME _ | FREE | LNK_VALUE _ | LNK_TYPE _ -> error, false)
    (LNK_SOME Locality.dummy) (Some false) mixture

let add_pointer parameters error agent_id site_name agent_id' agent_name'
    site_name' lnk_value mixture =
  add_binding_state parameters error agent_id site_name
    (fun _parameters error lnk ->
      match lnk with
      | LNK_MISSING | LNK_SOME _ | LNK_ANY _ -> error, true
      | LNK_TYPE ((agent_name'', _), (site_name'', _)) ->
        error, agent_name'' = agent_name' && site_name'' = site_name'
      | FREE | LNK_VALUE _ -> error, false)
    (LNK_VALUE (agent_id', agent_name', site_name', lnk_value, Locality.dummy))
    (Some false) mixture

let rec get_agent_name parameters error k mixture =
  match mixture with
  | SKIP mixture ->
    if k = 0 then
      Exception.warn parameters error __POS__ Exit ""
    else
      get_agent_name parameters error (k - 1) mixture
  | COMMA (agent, mixture) | DOT (_, agent, mixture) | PLUS (_, agent, mixture)
    ->
    if k = 0 then
      error, agent.ag_nme
    else
      get_agent_name parameters error (k - 1) mixture
  | EMPTY_MIX -> Exception.warn parameters error __POS__ Exit ""

let get_agent_name ?agent_name parameters error k mixture =
  match agent_name with
  | None -> get_agent_name parameters error k mixture
  | Some ag -> error, ag

let add_link parameters error agent_id ?agent_name site_name agent_id'
    ?agent_name' site_name' lnk_value mixture =
  let error, agent_name =
    get_agent_name parameters error agent_id ?agent_name mixture
  in
  let error, agent_name' =
    get_agent_name parameters error agent_id' ?agent_name:agent_name' mixture
  in
  let error, mixture =
    add_pointer parameters error agent_id site_name agent_id' agent_name'
      site_name' lnk_value mixture
  in
  add_pointer parameters error agent_id' site_name' agent_id agent_name
    site_name lnk_value mixture

let add_internal_state parameters error agent_id (site_name : site_name)
    (state : internal_state) mixture =
  mod_site_gen parameters error agent_id site_name
    (fun parameters error port ->
      match port.port_int with
      | [] -> error, { port with port_int = [ Some state ] }
      | _ :: _ -> Exception.warn parameters error __POS__ Exit port)
    mixture

(**********************************************************)
(*TYPE C*)
(**********************************************************)

type c_binding_state = C_Free | C_Lnk_type of c_agent_name * c_site_name
type state' = (internal_state, c_binding_state, counter_state) site_type

module State = struct
  type t = state'

  let compare = compare
  let print _ _ = ()
end

module Dictionary_of_States :
  Dictionary.Dictionary with type key = c_state and type value = state' =
  Dictionary.Dictionary_of_Ord (State)

type internal_state_specification = { string: internal_state option }

module Site = struct
  type t = site

  let compare = compare
  let print _ _ = ()
end

module Kasim_agent_name = struct
  type t = agent_name

  let compare = compare
  let print = Format.pp_print_string
end

module Dictionary_of_agents :
  Dictionary.Dictionary with type key = c_agent_name and type value = agent_name =
  Dictionary.Dictionary_of_Ord (Kasim_agent_name)

module Dictionary_of_sites :
  Dictionary.Dictionary with type key = c_site_name and type value = site =
  Dictionary.Dictionary_of_Ord (Site)

type site_list = {
  used: (site_name list * position) list;
  declared: (site_name list * position) list;
  creation: (site_name list * position) list;
}

type agent_dic = (unit, unit) Dictionary_of_agents.dictionary
type site_dic = (unit, unit) Dictionary_of_sites.dictionary
type state_dic = (unit, unit) Dictionary_of_States.dictionary

type agent_specification = {
  binding_sites_usage: site_list;
  marked_sites_usage: site_list;
}

type kappa_handler = {
  agents_dic: agent_dic;
  interface_constraints: agent_specification Int_storage.Nearly_inf_Imperatif.t;
  sites: site_dic Int_storage.Nearly_inf_Imperatif.t;
  states_dic:
    state_dic Int_storage.Nearly_inf_Imperatif.t
    Int_storage.Nearly_inf_Imperatif.t;
}

type 'a interval = { min: 'a option; max: 'a option }

type c_port = {
  c_site_name: c_site_name;
  c_site_position: position;
  c_site_interval: c_state interval;
}

module Site_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_site_name

  let compare = compare
  let print = Format.pp_print_int
end))

type c_interface = c_port Site_map_and_set.Map.t

type c_proper_agent = {
  c_agent_kasim_id: c_agent_id;
  c_agent_name: c_agent_name;
  c_agent_interface: c_interface;
  c_agent_position: position;
}

type site_address = { agent_index: c_agent_id; site: c_site_name }
type c_bond = site_address * site_address
type c_agent = C_ghost | C_agent of c_proper_agent

type c_mixture = {
  c_views: c_agent Int_storage.Quick_Nearly_inf_Imperatif.t;
  c_bonds:
    site_address Site_map_and_set.Map.t Int_storage.Nearly_inf_Imperatif.t;
  c_plus: (int * int) list;
  c_dot: (int * int) list;
}

type c_variable = (c_mixture, string) Alg_expr.e
type action = Release of c_bond | Bind of c_bond | Half_breaf of site_address

type c_rule = {
  c_rule_lhs: c_mixture;
  c_rule_bidirectional: bool;
  c_rule_rhs: c_mixture;
  c_diff_direct: c_mixture;
  c_diff_reverse: c_mixture;
  c_side_effects: action list;
}

type c_modif_expr =
  | C_APPLY of ((c_mixture, string) Alg_expr.e * c_rule * position)
  | C_UPDATE of (string * (c_mixture, string) Alg_expr.e * position)
    (*TODO: pause*)
  | C_STOP of position
  | C_SNAPSHOT of position (*maybe later of mixture too*)

type c_perturbation =
  (((c_mixture, string) Alg_expr.bool * position)
  * c_modif_expr list
  * ((c_mixture, string) Alg_expr.bool * position) option)
  * position

type enriched_rule = {
  e_rule_label: (string * position) option;
  e_rule_direct: bool;
  e_rule_rule: c_mixture rule;
  e_rule_c_rule: c_rule;
}

type enriched_init = {
  e_init_factor: int;
  e_init_mixture: mixture;
  e_init_c_mixture: c_mixture;
  e_init_pos: position;
}

type c_compil = {
  c_variables: c_variable Int_storage.Nearly_inf_Imperatif.t;
  (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
  c_signatures: (agent * position) Int_storage.Nearly_inf_Imperatif.t;
  (*agent signature declaration*)
  c_rules: enriched_rule Int_storage.Nearly_inf_Imperatif.t;
  (*rules (possibly named)*)
  c_observables:
    (c_mixture, string) Alg_expr.e Int_storage.Nearly_inf_Imperatif.t;
  (*list of patterns to plot*)
  c_init: enriched_init Int_storage.Nearly_inf_Imperatif.t;
  (*initial graph declaration*)
  c_perturbations:
    (c_mixture Locality.annot, enriched_rule) perturbation
    Int_storage.Nearly_inf_Imperatif.t;
}

let lift to_int from_int p a i = from_int (p (to_int a) i)
let pred_site_name = pred
let pred_agent_name = pred
let pred_state_index = pred
let gen_rule_id = lift int_of_rule_id rule_id_of_int
let sub_rule_id = gen_rule_id (fun a b -> a - b)
let add_rule_id = gen_rule_id (fun a b -> a + b)
let add_agent_id = lift int_of_agent_id agent_id_of_int (fun a b -> a + b)
let next_rule_id = succ
let next_agent_id = succ
let next_agent_name = succ
let next_site_name = succ
let next_state_index = succ
let compare_rule_id = compare
let compare_agent_id = compare
let compare_site_name = compare
let compare_state_index = compare
let compare_agent_name = compare

let compare_state_index_option_min a b =
  match a, b with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some a, Some b -> compare_state_index a b

let compare_state_index_option_max a b =
  match a, b with
  | None, None -> 0
  | None, _ -> 1
  | _, None -> -1
  | Some a, Some b -> compare_state_index a b

let compare_unit_agent_site _ _ = 0
let compare_unit _ _ = 0
let compare_unit_agent_name _ _ = dummy_agent_name
let compare_unit_site_name _ _ = dummy_site_name
let compare_unit_state_index _ _ = dummy_state_index

let array_of_list_rule_id create set parameters error list =
  let n = List.length list in
  let a = create parameters error n in
  let rec aux l k a =
    match l with
    | [] -> a
    | t :: q -> aux q (next_rule_id k) (set parameters (fst a) k t (snd a))
  in
  aux list dummy_rule_id a

(***************************************************************************)
(*MODULE*)
(***************************************************************************)

module Agent_type_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_agent_name and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module Agent_type_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_agent_name and type dimension = int =
  Int_storage.Quick_key_list (Agent_type_nearly_Inf_Int_storage_Imperatif)

module Rule_id_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_rule_id and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module Rule_id_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_rule_id and type dimension = int =
  Int_storage.Quick_key_list (Rule_id_nearly_Inf_Int_storage_Imperatif)

module Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif :
  Int_storage.Storage
    with type key = c_agent_name * c_site_name
     and type dimension = int * int =
  Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif

module Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif :
  Int_storage.Storage
    with type key = c_agent_name * c_site_name
     and type dimension = int * int =
  Int_storage.Quick_key_list
    (Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif)

module Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif :
  Int_storage.Storage
    with type key = c_agent_name * (c_site_name * c_state)
     and type dimension = int * (int * int) =
  Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif

(*site*)
module Site_type_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_site_name and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module Site_type_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_site_name and type dimension = int =
  Int_storage.Quick_key_list (Site_type_nearly_Inf_Int_storage_Imperatif)

(*state*)
module State_index_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_state and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module State_index_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_state and type dimension = int =
  Int_storage.Quick_key_list (State_index_nearly_Inf_Int_storage_Imperatif)

(*rule_id*)
module Rule_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_rule_id and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module Rule_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_rule_id and type dimension = int =
  Int_storage.Quick_key_list (Rule_nearly_Inf_Int_storage_Imperatif)

module Site_union_find =
  Union_find.Make (Site_type_nearly_Inf_Int_storage_Imperatif)

(****************************************************************************)
(*Define module fifo take rule_id*)

module Rule = struct
  type t = c_rule_id

  let compare = compare
  let print = Format.pp_print_int
end

module Rule_FIFO = Working_list.WlMake (Rule)

(****************************************************************************)

module Agent_id_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_agent_id and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

module Agent_id_quick_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = c_agent_id and type dimension = int =
  Int_storage.Quick_key_list (Agent_id_nearly_Inf_Int_storage_Imperatif)

(***************************************************************************)

module Agent_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_name

  let compare = compare
  let print = Format.pp_print_int
end))

module Agent_id_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_id

  let compare = compare
  let print = Format.pp_print_int
end))

module Lnk_id_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_link_value

  let compare = compare
  let print = Format.pp_print_int
end))

module Rule_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_rule_id

  let compare = compare
  let print = Format.pp_print_int
end))

module State_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_state

  let compare = compare
  let print = Format.pp_print_int
end))

module AgentRule_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_name * c_rule_id

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end))

module RuleAgent_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_rule_id * c_agent_id

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end))

(*use in site_accross_bonds_domain*)
module SiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_site_name * c_state

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end))

module AgentSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_name * c_site_name * c_state

  let compare = compare
  let print f (a, b, c) = Format.fprintf f "(%i, %i, %i)" a b c
end))

module Rule_setmap = SetMap.Make (struct
  type t = c_rule_id

  let compare = compare
  let print = Format.pp_print_int
end)

module Agent_id_setmap = SetMap.Make (struct
  type t = c_agent_id

  let compare = compare
  let print = Format.pp_print_int
end)

module PairRule_setmap = SetMap.Make (struct
  type t = c_rule_id * c_rule_id

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end)

(****************************************************************************)

module AgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_name * c_site_name

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end))

module Agents_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_id * c_agent_name

  let compare = compare
  let print = Pp.pair Format.pp_print_int Format.pp_print_int
end))

module AgentsSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_id * c_agent_name * c_site_name

  let compare = compare
  let print f (a, b, c) = Format.fprintf f "(%i, %i, %i)" a b c
end))

module AgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_id * c_agent_name * c_site_name * c_state

  let compare = compare
  let print f (a, b, c, d) = Format.fprintf f "(%i, %i, %i, %i)" a b c d
end))

type pair_of_states = c_state option * c_state option

module AgentsSitePState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = c_agent_id * c_agent_name * c_site_name * pair_of_states

  let compare = compare

  (*let print f (a,b,c,d) = Format.fprintf f "(%i, %i, %i, %i)" a b c d*)
  let print _ _ = ()
end))

(***************************************************************************)
(*bonds in rhs and lhs*)

(*module Agentnames_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = c_agent_name * c_agent_name
         let compare = compare
         let print = Pp.pair Format.pp_print_int Format.pp_print_int
       end))*)

module PairAgentSite_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = (c_agent_name * c_site_name) * (c_agent_name * c_site_name)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentsSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (c_agent_id * c_agent_name * c_site_name * c_state)
    * (c_agent_id * c_agent_name * c_site_name * c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSiteState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (c_agent_name * c_site_name * c_state)
    * (c_agent_name * c_site_name * c_state)

  let compare = compare
  let print _ _ = ()
end))

module PairAgentSitesState_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    (c_agent_name * c_site_name * c_site_name * c_state)
    * (c_agent_name * c_site_name * c_site_name * c_state)

  let compare = compare
  let print _ _ = ()
end))

(*******************************************************************)

module Views_bdu :
  Mvbdu_wrapper.Mvbdu
    with type key = c_site_name
     and type value = c_state
    with type mvbdu = Mvbdu_wrapper.Mvbdu.mvbdu =
  Mvbdu_wrapper.Mvbdu

module Views_intbdu = Mvbdu_wrapper.Internalize (Views_bdu)

type side_effects = {
  not_seen_yet:
    (c_agent_name * c_site_name * c_state) AgentsSiteState_map_and_set.Map.t;
  seen: AgentSiteState_map_and_set.Set.t;
}

let empty_side_effects =
  {
    not_seen_yet = AgentsSiteState_map_and_set.Map.empty;
    seen = AgentSiteState_map_and_set.Set.empty;
  }
