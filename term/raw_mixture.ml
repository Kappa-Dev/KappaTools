(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type internal = int option

type link = FREE | VAL of int

type agent =
  {
    a_type: int;
    a_ports: link array;
    a_ints: internal array;
  }

type t = agent list

let print_link ~explicit_free f = function
  | FREE -> if explicit_free then Format.pp_print_string f "!."
  | VAL i -> Format.fprintf f "!%i" i

let aux_pp_si sigs a s f i =
  match sigs with
  | Some sigs -> Signature.print_site_internal_state sigs a s f i
  | None ->
    match i with
    | Some i -> Format.fprintf f "%i~%i" s i
    | None -> Format.pp_print_int f s

let print_intf ~explicit_free compact with_link ?sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      let () = Format.fprintf
          f "%t%a%a"
          (if empty then Pp.empty
           else if compact then Pp.compact_comma else Pp.comma)
          (aux_pp_si sigs ag_ty i)
          ints.(i)
          (if with_link then print_link ~explicit_free else (fun _ _ -> ()))
          ports.(i) in
      aux false (succ i) in
  aux true 0

let aux_pp_ag sigs f a =
  match sigs with
  | Some sigs -> Signature.print_agent sigs f a
  | None -> Format.pp_print_int f a

let print_agent ~explicit_free compact created link ?sigs f ag =
  Format.fprintf f "%t%a(@[<h>%a@])"
    (fun f -> if created then Format.pp_print_string f "+")
    (aux_pp_ag sigs) ag.a_type
    (print_intf ~explicit_free compact link ?sigs ag.a_type)
    (ag.a_ports, ag.a_ints)

let print ~explicit_free ~compact ~created ?sigs f mix =
  Pp.list Pp.comma (print_agent ~explicit_free compact created true ?sigs) f mix

let agent_to_json a =
  `Assoc
    [("type",`Int a.a_type);
     ("sites", `List
        (Array.fold_right (fun x acc ->
             (match x with FREE -> `Null|VAL i -> `Int i)::acc) a.a_ports []));
     ("internals", `List
        (Array.fold_right (fun x acc ->
             (match x with None -> `Null|Some i -> `Int i)::acc) a.a_ints []))]
let agent_of_json = function
  | (`Assoc [("type",`Int t);("sites",`List s);("internals",`List i)] |
     `Assoc [("type",`Int t);("internals",`List i);("sites",`List s)] |
     `Assoc [("internals",`List i);("type",`Int t);("sites",`List s)] |
     `Assoc [("internals",`List i);("sites",`List s);("type",`Int t)] |
     `Assoc [("sites",`List s);("internals",`List i);("type",`Int t)] |
     `Assoc [("sites",`List s);("type",`Int t);("internals",`List i)]) ->
    { a_type = t;
      a_ports =
        Tools.array_map_of_list (function
            | `Null -> FREE
            | `Int p -> VAL p
            | x ->
              raise (Yojson.Basic.Util.Type_error ("Invalid site link",x)))
          s;
      a_ints =
        Tools.array_map_of_list (function
            | `Null -> None
            | `Int p -> Some p
            | x ->
              raise (Yojson.Basic.Util.Type_error ("Invalid internal state",x)))
          i}
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid raw_agent",x))

let to_json m = `List (List.map agent_to_json m)
let of_json = function
  | `List l -> List.map agent_of_json l
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid raw_mixture",x))

let get_color =
  let store = Hashtbl.create 10 in
  fun i ->
    try Hashtbl.find store i
    with Not_found ->
      let v = Format.sprintf "#%x%x%x" (Random.int 255)
          (Random.int 255) (Random.int 255) in
      let () = Hashtbl.add store i v in v

let print_dot sigs nb_cc f mix =
  Pp.listi
    Pp.empty
    (fun i f ag ->
       Format.fprintf
         f "node%d_%d [label = \"@[<h>%a@]\", color = \"%s\", style=filled];@,"
         nb_cc i (print_agent ~explicit_free:false true false false ~sigs) ag
         (get_color ag.a_type);
       Format.fprintf
         f "node%d_%d -> counter%d [style=invis];@," nb_cc i nb_cc) f mix;
  ignore @@
  List.fold_left
    (fun (a,acc) ag ->
       let acc' =
         Tools.array_fold_lefti
           (fun s acc -> function
              | FREE -> acc
              | VAL i ->
                match Mods.IntMap.pop i acc with
                | None,acc -> Mods.IntMap.add i (a,ag,s) acc
                | Some (a',ag',s'),acc' ->
                  Format.fprintf
                    f
                    "node%d_%d -> node%d_%d [taillabel=\"%a\", headlabel=\"%a\", dir=none];@,"
                    nb_cc a nb_cc a' (Signature.print_site sigs ag.a_type) s
                    (Signature.print_site sigs ag'.a_type) s';
                  acc')
           acc ag.a_ports in
       (succ a,acc'))
    (0,Mods.IntMap.empty) mix

let get_destination_of i l =
  let get_port_linked_on i ports =
    Tools.array_fold_lefti
      (fun s acc ->
         function
         | FREE -> acc
         | VAL j ->
           if i = j then
             match acc with
             | None -> Some s
             | Some _ ->
               failwith "Raw_mixture.get_destination_of"
           else acc)
      None ports in
  List.fold_left
    (fun (oui,non) ag ->
       match get_port_linked_on i ag.a_ports with
       | None -> (oui,ag::non)
       | Some s -> ((s,ag)::oui,non)) ([],[]) l

let rec agents_are_compatibles remains = function
  | [] -> remains = ([],[])
  | (o,p)::q ->
    o.a_type = p.a_type &&
    let i_ok =
      Tools.array_fold_left2i
        (fun _ b x y ->
           b && match x,y with
           | Some a, Some b -> (a = b)
           | None, None -> true
           | (Some _ | None), _ -> false) true o.a_ints p.a_ints in
    i_ok &&
    match Tools.array_fold_left2i
            (fun _ c x y ->
               match c with
               | None -> c
               | Some (todo,(g,h)) ->
                 match x, y with
                 | (FREE, VAL _ | VAL _, FREE) -> None
                 | FREE, FREE -> c
                 | VAL a, VAL b ->
                   match get_destination_of a g,get_destination_of b h with
                   | ([],_), ([],_) -> c
                   | ([s,x],g'), ([s',y],h')
                     when s = s' -> Some ((x,y)::todo,(g',h'))
                   | _, _ -> None
            )
            (Some (q,remains)) o.a_ports p.a_ports with
    | Some (todo',rem') -> agents_are_compatibles rem' todo'
    | _ -> false

let classify_by_type sigs mix =
  let len = Signature.size sigs in
  let out = Array.make len (0,[]) in
  let classify ag =
    let nb,ags = out.(ag.a_type) in
    out.(ag.a_type) <- (succ nb,ag::ags) in
  let () = List.iter classify mix in
  out

let equal cbt_a a cbt_b b =
  let rem_me x l = List.filter (fun y -> x != y) l in
  match Tools.array_min_equal_not_null cbt_a cbt_b with
  | None -> false
  | Some ([],ags) -> ags = []
  | Some (h1::_,ags) ->
    let a' = rem_me h1 a in
    List.fold_left
      (fun bool ag -> bool || agents_are_compatibles (a',rem_me ag b) [h1,ag])
      false ags

let hash_prime = 29
let coarse_hash cbt =
  Array.fold_right (fun (l,_) acc -> l + hash_prime * acc) cbt 0

type snapshot =  (int * (int * agent list) array * t) list Mods.IntMap.t

let empty_snapshot = Mods.IntMap.empty

let increment_in_snapshot sigs x s =
  let cbt_x = classify_by_type sigs x in
  let hs = coarse_hash cbt_x in
  let l = Mods.IntMap.find_default [] hs s in
  let rec aux_increment = function
  | [] -> [1,cbt_x,x]
  | (n,cbt_y,y as h)::t ->
    if equal cbt_x x cbt_y y then (succ n,cbt_y,y)::t
    else h::aux_increment t in
  Mods.IntMap.add hs (aux_increment l) s

let output_snapshot s =
  Mods.IntMap.fold (fun _ l acc ->
      List_util.rev_map_append (fun (x,_,y) -> (x,y)) l acc) s []
