(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cc_port = {
  port_links: (int * int) list;
  port_states: string list;
}
type site =
  | Port of cc_port
  | Counter of int
type cc_site = {
  site_name: string;
  site_type: site
}
type cc_node = {
  node_type: string;
  node_sites: cc_site array;
}
type connected_component = cc_node array

let print_link ~explicit_free (dandling,free_id) p f = function
  | [] -> if explicit_free then Format.pp_print_string f "!."
  | l ->
    let myself =
      ref (Mods.Int2Map.find_default Mods.Int2Map.empty p !dandling) in
    let () =
      Pp.list
        Pp.empty
        (fun f p' ->
           let i =
             if p = p' then -1 else
               match Option_util.bind
                       (Mods.Int2Map.find_option p)
                       (Mods.Int2Map.find_option p' !dandling) with
               | None ->
                 let () = incr free_id in
                 let () = myself := Mods.Int2Map.add p' !free_id !myself in
                 !free_id
               | Some va -> va in
           Format.fprintf f "!%i" i)
        f l in
    dandling := Mods.Int2Map.add p !myself !dandling

let print_port ~explicit_free with_link node p id f=
  Format.fprintf f "%a%a"
  (Pp.list Pp.empty (fun f i -> Format.fprintf f "~%s" i)) p.port_states
  (match with_link with
   | Some pack -> print_link ~explicit_free pack (node,id)
   | None -> (fun _ _ -> ()))
  p.port_links

let print_intf ~explicit_free compact with_link node =
  Pp.array
    (if compact then Pp.compact_comma else Pp.comma)
    (fun id f si ->
       let () = Format.fprintf f "%s" si.site_name in
         (match si.site_type with
          | Port p -> print_port ~explicit_free with_link node p id f
          | Counter i -> Format.fprintf f ":%i" i))

let print_agent ~explicit_free compact link node f ag =
  Format.fprintf f "%s(@[<h>%a@])"
    ag.node_type
    (print_intf ~explicit_free compact link node)
    ag.node_sites

let print_cc ~explicit_free ~compact f mix =
  let link = Some (ref(Mods.Int2Map.empty),ref 0) in
  Pp.array Pp.comma (print_agent ~explicit_free compact link) f mix

let get_color =
  let store = Hashtbl.create 10 in
  fun i ->
    try Hashtbl.find store i
    with Not_found ->
      let v = Format.sprintf "#%x%x%x" (Random.int 255)
          (Random.int 255) (Random.int 255) in
      let () = Hashtbl.add store i v in v

let print_dot_cc nb_cc f mix =
  Pp.array
    Pp.empty
    (fun i f ag ->
       Format.fprintf
         f "node%d_%d [label = \"@[<h>%a@]\", color = \"%s\", style=filled];@,"
         nb_cc i (print_agent ~explicit_free:false true None i) ag
         (get_color ag.node_type);
       Format.fprintf
         f "node%d_%d -> counter%d [style=invis];@," nb_cc i nb_cc) f mix;
  ignore @@
  Array.iteri
    (fun a ag ->
       Array.iteri
         (fun s si ->
           match si.site_type with
           | Counter _ -> ()
           | Port p ->
              Pp.list
                Pp.empty
                (fun f (a',s') ->
                  if a < a' || (a = a' && s < s') then
                    Format.fprintf
                      f
                      "node%d_%d -> node%d_%d [taillabel=\"%s\", headlabel=\"%s\", dir=none];@,"
                      nb_cc a nb_cc a' si.site_name
                      mix.(a').node_sites.(s').site_name)
                f p.port_links)
         ag.node_sites)
    mix

(*
type cc_node = {
  node_type: string;
  node_sites: cc_site array;
}
type connected_component = cc_node array
 *)

let write_cc_port ob p =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
             "port_links" (JsonUtil.write_list
                             (JsonUtil.write_compact_pair
                                Yojson.Basic.write_int Yojson.Basic.write_int))
             ob p.port_links in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
             "port_states" (JsonUtil.write_list Yojson.Basic.write_string)
             ob p.port_states in
  Bi_outbuf.add_char ob '}'

let write_site ob f =
  let () = Bi_outbuf.add_char ob '{' in
  let () = match f.site_type with
    | Counter i -> JsonUtil.write_field "counter" (Yojson.Basic.write_int) ob i
    | Port p -> JsonUtil.write_field "port" write_cc_port ob p in
  Bi_outbuf.add_char ob '}'

let write_cc_site ob f =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
      "site_name" Yojson.Basic.write_string ob f.site_name in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "site_type" write_site ob f in
  Bi_outbuf.add_char ob '}'

let read_cc_port p lb =
  let (port_links, port_states) =
  Yojson.Basic.read_fields
    (fun (s,i) key p lb ->
     if key = "port_links" then
       (Yojson.Basic.read_list
            (JsonUtil.read_compact_pair
               Yojson.Basic.read_int Yojson.Basic.read_int) p lb,i)
     else let () = assert (key = "port_states") in
          (s,Yojson.Basic.read_list Yojson.Basic.read_string p lb))
    ([],[]) p lb in
  {port_links; port_states}

let read_site p lb =
  Yojson.Basic.read_fields
    (fun _ key p lb ->
      if key = "counter" then (Counter (Yojson.Basic.read_int p lb))
      else let () = assert (key = "port") in Port (read_cc_port p lb))
    (Counter (-1)) p lb

let read_cc_site p lb =
  let (site_name,site_type) =
    Yojson.Basic.read_fields
      (fun (n,s) key p lb ->
         if key = "site_name" then (Yojson.Basic.read_string p lb,s)
         else let () = assert (key = "site_type") in (n,read_site p lb))
      ("",Counter (-1)) p lb in
  { site_name; site_type }

let write_cc_node ob f =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
      "node_type" Yojson.Basic.write_string ob f.node_type in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "node_sites" (JsonUtil.write_array write_cc_site) ob f.node_sites in
  Bi_outbuf.add_char ob '}'

let read_cc_node p lb =
  let (node_type,node_sites) =
    Yojson.Basic.read_fields
      (fun (n,s) key p lb ->
         if key = "node_type" then (Yojson.Basic.read_string p lb,s)
         else let () = assert (key = "node_sites") in
           (n,Yojson.Basic.read_array read_cc_site p lb))
      ("",[||]) p lb in
  { node_type; node_sites }

let write_connected_component ob f = JsonUtil.write_array write_cc_node ob f

let read_connected_component ob f = Yojson.Basic.read_array read_cc_node ob f

let string_of_connected_component ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  let () = write_connected_component ob x in
  Bi_outbuf.contents ob

let connected_component_of_string s =
  read_connected_component (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
