(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let toggle_element (projection : Api_types_j.simulation_info option -> bool)
    (content : [< Html_types.div_content_fun ] Html.elt Html.list_wrap) =
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (React.S.bind State_simulation.model (fun model ->
               React.S.const
                 (if projection (State_simulation.t_simulation_info model) then
                    [ "show" ]
                  else
                    [ "hidden" ])));
      ]
    content

let label_news tab_is_active counter =
  let last_value =
    ref
      (let simulation_info =
         State_simulation.t_simulation_info
           (React.S.value State_simulation.model)
       in
       counter simulation_info)
  in
  ReactiveData.RList.from_signal
    (React.S.l2
       (fun tab_active model ->
         if tab_active then
           []
         else (
           let simulation_info = State_simulation.t_simulation_info model in
           let v = counter simulation_info in
           if v <> !last_value && v > 0 then (
             let () = last_value := v in
             [
               Html.txt " ";
               Html.span
                 ~a:[ Html.a_class [ "label"; "label-default" ] ]
                 [ Html.txt "New" ];
             ]
           ) else
             []
         ))
       tab_is_active State_simulation.model)

let badge (counter : Api_types_j.simulation_info option -> int) =
  ReactiveData.RList.from_signal
    (React.S.map
       (fun model ->
         let simulation_info = State_simulation.t_simulation_info model in
         let count = counter simulation_info in
         if count > 0 then
           [
             Html.txt " ";
             Html.span
               ~a:[ Html.a_class [ "badge" ] ]
               [ Html.txt (string_of_int count) ];
           ]
         else
           [])
       State_simulation.model)
