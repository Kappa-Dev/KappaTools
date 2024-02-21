(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type links =
  | LINKS of ((int * int) * int) list
  | WHATEVER
  | SOME
  | TYPE of string * string

type cc_port = {
  port_links: links;
  port_states: string list option;  (** [None] means WHATEVER *)
}

type site = Port of cc_port | Counter of int
type cc_site = { site_name: string; site_type: site }

type cc_node = {
  node_type: string;
  node_id: int option;
  node_sites: cc_site array;
}

type connected_component = cc_node option array array

module LinkSetMap = SetMap.Make (struct
  type t = (int * int) * int

  let print =
    Pp.pair
      (Pp.pair Format.pp_print_int Format.pp_print_int)
      Format.pp_print_int

  let compare (x, y) (x', y') =
    let c = Mods.int_pair_compare x x' in
    if c = 0 then
      Mods.int_compare y y'
    else
      c
end)

let print_link (dangling, free_id) p f = function
  | WHATEVER -> Format.pp_print_string f "[#]"
  | SOME -> Format.pp_print_string f "[_]"
  | TYPE (si, ty) -> Format.fprintf f "[%s.%s]" si ty
  | LINKS [] -> Format.pp_print_string f "[.]"
  | LINKS l ->
    let myself =
      ref (LinkSetMap.Map.find_default LinkSetMap.Map.empty p !dangling)
    in
    let () =
      Format.fprintf f "[%a]"
        (Pp.list Pp.space (fun f p' ->
             let i =
               if p = p' then
                 -1
               else (
                 match
                   Option_util.bind
                     (LinkSetMap.Map.find_option p)
                     (LinkSetMap.Map.find_option p' !dangling)
                 with
                 | None ->
                   let () = incr free_id in
                   let () = myself := LinkSetMap.Map.add p' !free_id !myself in
                   !free_id
                 | Some va -> va
               )
             in
             Format.fprintf f "%i" i))
        l
    in
    dangling := LinkSetMap.Map.add p !myself !dangling

let print_port with_link node p id f =
  Format.fprintf f "%a%a"
    (fun f -> function
      | None -> Format.pp_print_string f "{#}"
      | Some [] -> ()
      | Some l ->
        Format.fprintf f "{%a}"
          (Pp.list Pp.space (fun f i -> Format.fprintf f "%s" i))
          l)
    p.port_states
    (match with_link with
    | Some pack -> print_link pack (node, id)
    | None -> fun _ _ -> ())
    p.port_links

let print_intf with_link node =
  Pp.array Pp.space (fun id f si ->
      let () = Format.fprintf f "%s" si.site_name in
      match si.site_type with
      | Port p -> print_port with_link node p id f
      | Counter i -> Format.fprintf f "{=%i}" i)

let print_agent with_id link node f = function
  | None -> Format.pp_print_string f "."
  | Some ag ->
    Format.fprintf f "%a%s(@[<h>%a@])"
      (Pp.option ~with_space:false (fun f i -> Format.fprintf f "x%i:" i))
      (if with_id then
         ag.node_id
       else
         None)
      ag.node_type (print_intf link node) ag.node_sites

let print_cc f mix =
  let link = Some (ref LinkSetMap.Map.empty, ref 0) in
  Pp.array
    (fun f -> Format.fprintf f "\\@ ")
    (fun al -> Pp.array Pp.comma (fun ar -> print_agent true link (al, ar)))
    f mix

let get_color =
  let store = Hashtbl.create 10 in
  fun i ->
    try Hashtbl.find store i
    with Not_found ->
      let v =
        Format.sprintf "#%x%x%x" (Random.int 255) (Random.int 255)
          (Random.int 255)
      in
      let () = Hashtbl.add store i v in
      v

let print_dot_cc nb_cc f mix =
  Pp.array Pp.empty
    (fun il ->
      Pp.array Pp.empty (fun ir f -> function
        | None -> ()
        | Some ag ->
          Format.fprintf f
            "node%d_%d_%d [label = \"@[<h>%a@]\", color = \"%s\", \
             style=filled];@,"
            nb_cc il ir
            (print_agent false None (il, ir))
            (Some ag) (get_color ag.node_type);
          Format.fprintf f "node%d_%d_%d -> counter%d [style=invis];@," nb_cc il
            ir nb_cc))
    f mix;
  ignore
  @@ Array.iteri
       (fun al ->
         Array.iteri (fun ar -> function
           | None -> ()
           | Some ag ->
             Array.iteri
               (fun s si ->
                 match si.site_type with
                 | Counter _ -> ()
                 | Port p ->
                   (match p.port_links with
                   | WHATEVER -> assert false
                   | SOME -> assert false
                   | TYPE (_si, _ty) -> assert false
                   | LINKS links ->
                     Pp.list Pp.empty
                       (fun f ((al', ar'), s') ->
                         if
                           al < al'
                           || (al = al' && (ar < ar' || (ar = ar' && s < s')))
                         then (
                           match mix.(al').(ar') with
                           | None -> assert false
                           | Some ag' ->
                             Format.fprintf f
                               "node%d_%d_%d -> node%d_%d_%d \
                                [taillabel=\"%s\", headlabel=\"%s\", \
                                dir=none];@,"
                               nb_cc al ar nb_cc al' ar' si.site_name
                               ag'.node_sites.(s').site_name
                         ))
                       f links))
               ag.node_sites))
       mix

(*
type cc_node = {
  node_type: string;
  node_sites: cc_site array;
}
type connected_component = cc_node array
 *)

let write_cc_port ob p =
  let () = Buffer.add_char ob '{' in
  let () =
    JsonUtil.write_field "port_links"
      (fun ob -> function
        | WHATEVER -> Yojson.Basic.write_null ob ()
        | SOME -> Yojson.Basic.write_bool ob true
        | TYPE (si, ty) ->
          let () = Buffer.add_string ob "{\"site_name\":\"" in
          let () = Buffer.add_string ob si in
          let () = Buffer.add_string ob "\",\"agent_type\":\"" in
          let () = Buffer.add_string ob ty in
          Buffer.add_string ob "\"}"
        | LINKS l ->
          JsonUtil.write_list
            (JsonUtil.write_compact_pair
               (JsonUtil.write_compact_pair Yojson.Basic.write_int
                  Yojson.Basic.write_int)
               Yojson.Basic.write_int)
            ob l)
      ob p.port_links
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "port_states"
      (JsonUtil.write_option (JsonUtil.write_list Yojson.Basic.write_string))
      ob p.port_states
  in
  Buffer.add_char ob '}'

let write_site ob f =
  let () = Buffer.add_char ob '[' in
  let () =
    match f.site_type with
    | Counter i ->
      let () = Yojson.Basic.write_string ob "counter" in
      let () = Buffer.add_char ob ',' in
      Yojson.Basic.write_int ob i
    | Port p ->
      let () = Yojson.Basic.write_string ob "port" in
      let () = Buffer.add_char ob ',' in
      write_cc_port ob p
  in
  Buffer.add_char ob ']'

let write_cc_site ob f =
  let () = Buffer.add_char ob '{' in
  let () =
    JsonUtil.write_field "site_name" Yojson.Basic.write_string ob f.site_name
  in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "site_type" write_site ob f in
  Buffer.add_char ob '}'

let links_of_yojson = function
  | `Null -> WHATEVER
  | `Bool b ->
    let () = assert b in
    SOME
  | `Assoc [ ("site_name", `String si); ("agent_type", `String ty) ]
  | `Assoc [ ("agent_type", `String ty); ("site_name", `String si) ] ->
    TYPE (si, ty)
  | `List _ as x ->
    let error_msg = None in
    LINKS
      (JsonUtil.to_list
         (JsonUtil.compact_to_pair
            (JsonUtil.compact_to_pair
               (JsonUtil.to_int ?error_msg)
               (JsonUtil.to_int ?error_msg))
            (JsonUtil.to_int ?error_msg))
         x)
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect cc_port", x))

let read_cc_port p lb =
  let port_links, port_states =
    Yojson.Basic.read_fields
      (fun (s, i) key p lb ->
        if key = "port_links" then
          links_of_yojson (Yojson.Basic.read_json p lb), i
        else (
          let () = assert (key = "port_states") in
          ( s,
            JsonUtil.read_option
              (Yojson.Basic.read_list Yojson.Basic.read_string)
              p lb )
        ))
      (LINKS [], Some []) p lb
  in
  { port_links; port_states }

let read_site p lb =
  let () = Yojson.Basic.read_lbr p lb in
  let key = JsonUtil.read_between_spaces Yojson.Basic.read_string p lb in
  let () = Yojson.Basic.read_comma p lb in
  let out =
    JsonUtil.read_between_spaces
      (fun p lb ->
        if key = "counter" then
          Counter (Yojson.Basic.read_int p lb)
        else (
          let () = assert (key = "port") in
          Port (read_cc_port p lb)
        ))
      p lb
  in
  let () = Yojson.Basic.read_rbr p lb in
  out

let read_cc_site p lb =
  let site_name, site_type =
    Yojson.Basic.read_fields
      (fun (n, s) key p lb ->
        if key = "site_name" then
          Yojson.Basic.read_string p lb, s
        else (
          let () = assert (key = "site_type") in
          n, read_site p lb
        ))
      ("", Counter (-1)) p lb
  in
  { site_name; site_type }

let write_cc_node ob x =
  JsonUtil.write_option
    (fun ob f ->
      let () = Buffer.add_char ob '{' in
      let () =
        JsonUtil.write_field "node_type" Yojson.Basic.write_string ob
          f.node_type
      in
      let () = JsonUtil.write_comma ob in
      let () =
        match f.node_id with
        | None -> ()
        | Some node_id ->
          let () =
            JsonUtil.write_field "node_id" Yojson.Basic.write_int ob node_id
          in
          JsonUtil.write_comma ob
      in
      let () =
        JsonUtil.write_field "node_sites"
          (JsonUtil.write_array write_cc_site)
          ob f.node_sites
      in
      Buffer.add_char ob '}')
    ob x

let read_cc_node p lb =
  JsonUtil.read_option
    (fun p lb ->
      let node_id, node_type, node_sites =
        Yojson.Basic.read_fields
          (fun (id, n, s) key p lb ->
            if key = "node_id" then
              Some (Yojson.Basic.read_int p lb), n, s
            else if key = "node_type" then
              id, Yojson.Basic.read_string p lb, s
            else (
              let () = assert (key = "node_sites") in
              id, n, Yojson.Basic.read_array read_cc_site p lb
            ))
          (None, "", [||]) p lb
      in
      { node_id; node_type; node_sites })
    p lb

let write_connected_component ob f =
  JsonUtil.write_array (JsonUtil.write_array write_cc_node) ob f

let read_connected_component ob f =
  Yojson.Basic.read_array (Yojson.Basic.read_array read_cc_node) ob f

let string_of_connected_component ?(len = 1024) x =
  let ob = Buffer.create len in
  let () = write_connected_component ob x in
  Buffer.contents ob

let connected_component_of_string s =
  read_connected_component (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
