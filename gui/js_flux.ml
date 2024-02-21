(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type flux_configuration = object
  val beginTimeId : Js.js_string Js.t Js.prop
  val endTimeId : Js.js_string Js.t Js.prop
  val selectCorrectionId : Js.js_string Js.t Js.prop
  val checkboxSelfInfluenceId : Js.js_string Js.t Js.prop
  val toggleRulesId : Js.js_string Js.t Js.prop
  val nbEventsId : Js.js_string Js.t Js.prop
  val svgId : Js.js_string Js.t Js.prop
  val rulesCheckboxesId : Js.js_string Js.t Js.prop
  val height : int Js.t Js.prop
  val width : int Js.t Js.prop
  val shortLabels : bool Js.t Js.prop
end

let constructor_configuration : flux_configuration Js.t Js.constr =
  Js.Unsafe.pure_js_expr "Object"

let create_configuration ~(short_labels : bool) ~(begin_time_id : string)
    ~(end_time_id : string) ~(select_correction_id : string)
    ~(toggle_rules_id : string) ~(checkbox_self_influence_id : string)
    ~(nb_events_id : string) ~(svg_id : string) ~(rules_checkboxes_id : string)
    ~(height : int) ~(width : int) : flux_configuration Js.t =
  let configuration : flux_configuration Js.t =
    new%js constructor_configuration
  in
  let () =
    (Js.Unsafe.coerce configuration)##.beginTimeId := Js.string begin_time_id;
    (Js.Unsafe.coerce configuration)##.endTimeId := Js.string end_time_id;
    (Js.Unsafe.coerce configuration)##.selectCorrectionId
    := Js.string select_correction_id;
    (Js.Unsafe.coerce configuration)##.checkboxSelfInfluenceId
    := Js.string checkbox_self_influence_id;
    (Js.Unsafe.coerce configuration)##.toggleRulesId
    := Js.string toggle_rules_id;
    (Js.Unsafe.coerce configuration)##.nbEventsId := Js.string nb_events_id;
    (Js.Unsafe.coerce configuration)##.svgId := Js.string svg_id;
    (Js.Unsafe.coerce configuration)##.rulesCheckboxesId
    := Js.string rules_checkboxes_id;
    (Js.Unsafe.coerce configuration)##.height := height;
    (Js.Unsafe.coerce configuration)##.width := width;
    (Js.Unsafe.coerce configuration)##.shortLabels := short_labels
  in
  configuration

class type flux_data = object
  val bioBeginTime : float Js.t Js.prop
  val bioEndTime : float Js.t Js.prop
  val normalized : bool Js.t Js.prop
  val rules : Js.js_string Js.js_array Js.t Js.prop
  val hits : int Js.js_array Js.t Js.prop
  val fluxs : int Js.js_array Js.js_array Js.t Js.prop
end

let constructor_data : flux_data Js.t Js.constr =
  Js.Unsafe.pure_js_expr "Object"

let create_data ~(flux_begin_time : float) ~(flux_end_time : float)
    ~(normalized : bool) ~(flux_rules : string array) ~(flux_hits : int array)
    ~(flux_fluxs : float array array) : flux_data Js.t =
  let data : flux_data Js.t = new%js constructor_data in
  let () =
    (Js.Unsafe.coerce data)##.bioBeginTime := flux_begin_time;
    (Js.Unsafe.coerce data)##.bioEndTime := flux_end_time;
    (Js.Unsafe.coerce data)##.normalized := normalized;
    (Js.Unsafe.coerce data)##.rules := Js.array (Array.map Js.string flux_rules);
    (Js.Unsafe.coerce data)##.hits := Js.array flux_hits;
    (Js.Unsafe.coerce data)##.fluxs := Js.array (Array.map Js.array flux_fluxs);
    ()
  in
  data

class type flux_map = object
  method exportJSON : Js.js_string Js.t -> unit Js.meth
  method setFlux : flux_data Js.t -> unit Js.meth
end

let create_flux_map (configuration : flux_configuration Js.t) : flux_map Js.t =
  Js.Unsafe.new_obj
    (Js.Unsafe.pure_js_expr "fluxMap")
    [| Js.Unsafe.inject configuration |]
