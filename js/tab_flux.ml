(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let tab_is_active, set_tab_is_active = React.S.create false

let has_fluxmap
    (simulation_info : Api_types_j.simulation_info option) :
  bool =
  match simulation_info with
  | None -> false
  | Some simulation_info ->
    simulation_info.Api_types_j.simulation_info_output.Api_types_j.simulation_output_flux_maps > 0

let flux_table, table_handle = ReactiveData.RList.create []
let flux_header, set_flux_header = React.S.create []
let flux =
  let thead = React.S.map (fun x -> Html.thead x) flux_header in
  Tyxml_js.R.Html5.tablex
    ~a:[Html.a_class
          ["table"; "table-condensed";"table-bordered"]]
    ~thead flux_table

let fill_table flux_map =
  let open Api_types_j in
  let all = flux_map.flux_data.flux_kind = Primitives.PROBABILITY in
  let header =
    Html.tr
      ((Html.th [Html.pcdata "affects"]) ::
       Array.fold_right
         (fun r acc -> Html.th [Html.pcdata r] :: acc)
         flux_map.flux_rules []) in
  let body =
    Tools.array_fold_righti
      (fun i data acc ->
         if all || flux_map.flux_data.flux_hits.(i) > 0 then
           Html.tr
             (Html.th
                [Html.pcdata (flux_map.flux_rules.(i)^" ("^
                              string_of_int flux_map.flux_data.flux_hits.(i)^
                              " hits)")] ::
              Array.fold_right
                (fun v acc -> Html.td
                    ~a:[Html.a_class (if v > 0. then ["success"]
                                      else if v < 0. then ["info"]
                                      else [])]
                    [Html.pcdata (string_of_float v)] :: acc)
                data [])
           ::acc
         else acc) flux_map.flux_data.flux_fluxs [] in
  let () = set_flux_header [header] in
  ReactiveData.RList.set table_handle [Html.tbody body]

let update_flux_map fluxmap_id: unit =
  State_simulation.when_ready
    ~label:__LOC__
    (fun manager -> manager#simulation_detail_flux_map fluxmap_id >>=
      (Api_common.result_map
         ~ok:(fun _ (flux_map : Api_types_j.flux_map) ->
             let () = fill_table flux_map in
             Lwt.return (Api_common.result_ok ()))
         ~error:(fun result_code e ->
             let () = ReactiveData.RList.set table_handle [] in
           Lwt.return (Api_common.result_messages ~result_code e ))))

let flux_list, flux_handle = ReactiveData.RList.create []
let flux_select =
  Tyxml_js.R.Html5.select
    ~a:[ Html.a_class ["form-control"] ]
    flux_list

let select_fluxmap () =
  let fluxmap_id =
    Js.to_string ((Tyxml_js.To_dom.of_select flux_select)##.value) in
  update_flux_map fluxmap_id

let _ = React.S.map
    (fun _ ->
       State_simulation.with_simulation_info
         ~label:__LOC__
         ~stopped:(fun _ ->
             let () = ReactiveData.RList.set flux_handle [] in
             let () = ReactiveData.RList.set table_handle [] in
             Lwt.return (Api_common.result_ok ()))
         ~initializing:(fun _ ->
             let () = ReactiveData.RList.set flux_handle [] in
             let () = ReactiveData.RList.set table_handle [] in
             Lwt.return (Api_common.result_ok ()))
         ~ready:(fun manager _ ->
           manager#simulation_catalog_flux_map >>=
           (Api_common.result_bind_lwt
              ~ok:(fun (data : Api_types_t.flux_map_catalog) ->
                  let () = ReactiveData.RList.set
                      flux_handle
                      (List.rev_map
                         (fun id -> Html.option
                             ~a:[ Html.a_value id ]
                             (Html.pcdata id))
                         data.Api_types_t.flux_map_ids) in
                  let () = select_fluxmap () in
                  Lwt.return (Api_common.result_ok ()))
           )
           ) ()
    )
    (React.S.on
       tab_is_active State_simulation.dummy_model State_simulation.model)

let content () = [
  Html.div ~a:[Html.a_class ["flex_content"; "table-responsive"]]
    [Html.form [flux_select]; flux ]
]

let navli () =
  Ui_common.badge
    (fun state ->
       match state with
       | None -> 0
       | Some state ->
         state.Api_types_j.simulation_info_output.Api_types_j.simulation_output_flux_maps)

let onload () =
  let () =
    (Tyxml_js.To_dom.of_select flux_select)##.onchange :=
      Dom.handler (fun _ -> let () = select_fluxmap () in Js._false) in
  let () = Common.jquery_on "#navflux"
      "shown.bs.tab" (fun _ -> set_tab_is_active true) in
  Common.jquery_on "#navflux"
    "hide.bs.tab" (fun _ -> set_tab_is_active false)

let onresize () : unit = ()
