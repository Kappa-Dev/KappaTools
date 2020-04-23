(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
module Svg = Tyxml_js.Svg
open Lwt.Infix

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let navli () = ReactiveData.RList.empty

let sandbox_svg = Dom_svg.createSvg Dom_svg.document
let sandbox_text = Dom_svg.createTextElement Dom_svg.document
let () = Dom.appendChild sandbox_svg sandbox_text

let pi = 0x1.921fb54442d18p+1

let link_compare (x,y) (x',y') =
  let c = Mods.int_pair_compare x x' in
  if c=0 then Mods.int_compare y y' else c

let size_of_text x =
  let () =
    Js.Opt.iter
      sandbox_text##.firstChild
      (fun x -> Dom.removeChild sandbox_text x) in
  let () = Dom.appendChild
      sandbox_text (Dom_svg.document##createTextNode (Js.string x)) in
  let bbox = sandbox_text##getBBox in
  (bbox##.width,bbox##.height)

let zbox = (0., 0.)
let max_box (x,y) (a,b) = (max x a, max y b)
let plus_box (x,y) (a,b) = (x +. a, y +. b)
let diag_box (x,y) = sqrt (x *. x +. y *. y)

let sizes_of_agents =
  let open Kappa_mixtures.User_graph in
  Array.map (
    Array.map (function
        | None -> (zbox,[||])
        | Some ag ->
          let sites =
            Array.map
              (fun site ->
                 let states =
                   match site.site_type with
                   | Counter c -> size_of_text (string_of_int c)
                   | Port { port_states; _ } ->
                     Option_util.fold
                       (List.fold_left (fun a x -> max_box a (size_of_text x)))
                       zbox port_states in
                 (size_of_text site.site_name,states))
              ag.node_sites in
          (size_of_text ag.node_type,sites)))

let max_site_box ~only_site =
  Array.fold_left
    (fun a (s,t) ->
       let p = plus_box s t in
       max_box a (if only_site then p else (plus_box p t)))
    zbox

let height_of_lines =
  Array.map
    (Array.fold_left
       (fun acc (v,sites) ->
          let s = max_site_box ~only_site:false sites in
          max acc (2. *. diag_box s +. diag_box v))
       0.)

let width_of_rows sizes =
  if sizes = [||] then [||] else
    let max_width = Array.make (Array.length sizes.(0)) 0. in
    let () =
      Array.iter
        (Array.iteri (fun i (v,sites) ->
             let s = max_site_box ~only_site:false sites in
             max_width.(i) <- max max_width.(i) (2. *. diag_box s +. diag_box v)))
        sizes in
    max_width

let svg_of_node cx cy r color text =
  let c =
    Svg.circle
      ~a:[Svg.a_cx (cx,None); Svg.a_cy (cy,None);
          Svg.a_r (r,None);
          Svg.a_stroke (`Color ("black",None));
          Svg.a_fill (`Color (color,None))] [] in
  let o =
    Svg.text
      ~a:[Svg.a_x_list [(cx,None)]; Svg.a_y_list [(cy,None)];
          Svg.a_text_anchor `Middle; Svg.a_dominant_baseline `Middle]
      [Svg.txt text] in
  Svg.g [c;o]

(*let svg_of_self_link cx cy r nb_sites site1 site2 =
  let dem = float_of_int nb_sites in
  let l = 8. in
  let angle1 = 2. *. pi *. float_of_int site1 /. dem in
  let angle2 = 2. *. pi *. float_of_int site2 /. dem in
  let x1 = cx +. r *. cos angle1 in
  let y1 = cy +. r *. sin angle1 in
  let x2 = cx +. (r+.l) *. cos ((angle1 +. angle2) /. 2.) in
  let y2 = cy +. (r+.l) *. sin ((angle1 +. angle2) /. 2.) in
  let x3 = cx +. r *. cos angle2 in
  let y3 = cy +. r *. sin angle2 in
  Svg.polyline
    ~a:[
      Svg.a_points [x1,y1;x2,y2;x3,y3];
      Svg.a_stroke (`Color ("black",None));
    ] []*)

let add_link
    (ag_id,site_id as id) x1 y1 (semi_links,links as acc) (ag_d,site_d as dest) =
  let c = Mods.int_pair_compare ag_id ag_d in
  if c = 0 then
    if site_id > site_d then
      (semi_links, (*svg_of_self_link () () () () site_id site_d::*)links)
    else acc
  else
  if c > 0 then
    match List.partition (fun (x,_) -> link_compare x dest = 0) semi_links with
    | [], _ -> assert false
    |  (_,(x2,y2)) :: _ , semi_links' ->
      let link = Svg.line
          ~a:[Svg.a_x1 (x1,None); Svg.a_y1 (y1,None);
              Svg.a_x2 (x2,None); Svg.a_y2 (y2,None);
              Svg.a_stroke (`Color ("black",None));
             ]
          [] in
      (semi_links', link::links)
  else
    ((id,(x1,y1))::semi_links,links)

let svg_of_link_states id cx cy r angle acc = function
  | Kappa_mixtures.User_graph.WHATEVER -> None,acc
  | Kappa_mixtures.User_graph.LINKS [] ->
    let dr = 2. in
    Some (Svg.circle
            ~a:[Svg.a_cx (cx,None); Svg.a_cy (cy,None);
                Svg.a_r (r -. dr,None);
                Svg.a_stroke (`Color ("black",None));
                Svg.a_fill `None]
            []),acc
  | Kappa_mixtures.User_graph.LINKS (_ :: _ as x) ->
    None,List.fold_left (add_link id cx cy) acc x
  | Kappa_mixtures.User_graph.SOME ->
    let l = 8. in
    let x1 = cx +. r *. cos angle in
    let y1 = cy +. r *. sin angle in
    let x2 = cx +. (r+.l) *. cos angle in
    let y2 = cy +. (r+.l) *. sin angle in
    Some (Svg.line
            ~a:[Svg.a_x1 (x1,None); Svg.a_y1 (y1,None);
                Svg.a_x2 (x2,None); Svg.a_y2 (y2,None);
                Svg.a_stroke (`Color ("black",None));
               ]
            []),acc
  | Kappa_mixtures.User_graph.TYPE (_, _) -> None,acc

let svg_of_sites id cx cy r sizes links defs =
  let open Kappa_mixtures.User_graph in
  let nb = Array.length defs in
  if nb = 0 then ([],links) else
    let dem = float_of_int nb in
    Tools.array_fold_lefti
      (fun i (acc,links) site ->
         let (pbox,sbox) = sizes.(i) in
         let angle = 2. *. pi *. float_of_int i /. dem in
         let cx = cx +. r *. cos angle in
         let cy = cy -. r *. sin angle in
         let r' = (diag_box sbox +. diag_box pbox) /. 2. in
         let cx' = cx +. r' *. cos angle in
         let cy' = cy -. r' *. sin angle in
         match site.site_type with
         | Counter va ->
           svg_of_node cx cy r' "lightgrey" site.site_name ::
           svg_of_node
             cx' cy' (diag_box sbox /. 2.) "whitesmoke" (string_of_int va) ::
           acc, links
         | Port { port_states = None | Some []; port_links } ->
           let state,links' =
             svg_of_link_states (id,i) cx cy r' angle links port_links in
           svg_of_node cx cy r' "lightgrey" site.site_name ::
           List_util.cons_option state acc, links'
         | Port { port_states = Some (s::_); port_links } ->
           let state,links' =
             svg_of_link_states (id,i) cx cy r' angle links port_links in
           svg_of_node cx cy r' "lightgrey" site.site_name ::
           List_util.cons_option state
             (svg_of_node cx' cy' (diag_box sbox /. 2.) "whitesmoke" s :: acc),
           links'
      )
      ([],links)
    defs

let svg_of_graph mix =
  let open Kappa_mixtures.User_graph in
  let sizes = sizes_of_agents mix in
  let max_width = width_of_rows sizes in
  let max_height = height_of_lines sizes in
  let ((ags,(semi_links,links)),_) =
    Tools.array_fold_lefti (fun i (acc,top_y) arr ->
        let cy = top_y +. max_height.(i) /. 2. in
        let (acc',_) =
          Tools.array_fold_lefti (fun j ((ags,links as acc),left_x) -> function
              | None -> (acc,left_x +. max_width.(j))
              | Some ag ->
                let cx = left_x +. max_width.(j) /. 2. in
                let (agbox,sites) = sizes.(i).(j) in
                let radius =
                  (diag_box agbox +.
                   diag_box (max_site_box ~only_site:true sites))
                    /. 2. in
                let sites,links' = svg_of_sites (i,j) cx cy radius sites links ag.node_sites in
                let n = svg_of_node cx cy radius "silver" ag.node_type in
                let out = if sites = [] then n else Svg.g (n::sites) in
                ((out::ags,links'),left_x +. max_width.(j)))
            (acc,0.) arr in
        (acc',top_y +. max_height.(i)))
      (([],([],[])),0.) mix in
  let () = assert (semi_links = []) in
  [ Html.svg
      ~a:[Svg.a_width (Array.fold_left (+.) 0. max_width,None);
          Svg.a_height (Array.fold_left (+.) 0. max_height,None)]
      (List.rev_append links ags) ]

let content () =
  let scc =
    State_project.on_project_change_async ~on:tab_is_active
      (React.S.value State_file.model) State_file.model []
      (fun (manager : Api.concrete_manager) -> function
         | { State_file.current = None; _ } -> Lwt.return_nil
         | { State_file.current = Some { State_file.cursor_pos; rank; _ } ;
             directory } ->
           match Mods.IntMap.find_option rank directory with
           | None -> Lwt.return_nil
           | Some { State_file.name; _ } ->
             manager#mixture_at_position name cursor_pos >|=
             Result_util.fold
               ~ok:(fun mix ->
                   Html.p [
                     Html.txt
                       (Format.asprintf
                          "@[%a@]" Kappa_mixtures.User_graph.print_cc mix) ] ::
                   svg_of_graph mix)
               ~error:(fun mh ->
                   List.map
                     (fun m ->
                        Html.p
                          [Html.txt
                             (Format.asprintf "@[%a@]" Result_util.print_message m)])
                     mh)) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll"]]
      (ReactiveData.RList.from_signal scc);
    Tyxml_js.Html.tot (sandbox_svg :> Dom.node Js.t)
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navpattern" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navpattern" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()

let onresize () = ()
