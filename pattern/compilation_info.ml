type place = Connected_component.ContentAgent.t * int

type t = {
  agents_in_syntactic_rule : place list;
  sites_of_side_effect : (place * int) list;
  sites_of_internal_state_side_effect : (place * int) list;
}

let of_empty_rule =
  {
  agents_in_syntactic_rule = [];
  sites_of_side_effect = [];
  sites_of_internal_state_side_effect = [];
  }

let add_agent info ag_id = function
  | None -> info
  | Some (side_effect_ports,side_effect_internals) ->
     {
       agents_in_syntactic_rule = ag_id :: info.agents_in_syntactic_rule;
       sites_of_side_effect =
	 Tools.array_fold_lefti
	   (fun i acc e -> if e then (ag_id,i)::acc else acc)
	   info.sites_of_side_effect side_effect_ports;
       sites_of_internal_state_side_effect =
	 Tools.array_fold_lefti
	   (fun i acc e -> if e then (ag_id,i)::acc else acc)
	   info.sites_of_internal_state_side_effect side_effect_internals;
     }

let rename_place wk id cc inj (n, id' as x) =
  if id <> id' then x else
    let n' = Connected_component.ContentAgent.rename wk cc inj n in
    if n == n' then x else (n',id')

let rename_place' wk id cc inj (pl,i as x) =
  let aux = rename_place wk id cc inj pl in
  if aux == pl then x else (aux,i)

let rename wk id cc inj info =
  {
    agents_in_syntactic_rule =
      Tools.list_smart_map
	(rename_place wk id cc inj) info.agents_in_syntactic_rule;
    sites_of_side_effect =
      Tools.list_smart_map
	(rename_place' wk id cc inj) info.sites_of_side_effect;
    sites_of_internal_state_side_effect =
      Tools.list_smart_map
	(rename_place' wk id cc inj) info.sites_of_internal_state_side_effect;
  }
