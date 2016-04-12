class type flux_configuration = object
                             val beginTimeId : Js.js_string Js.t Js.prop
                             val endTimeId : Js.js_string Js.t Js.prop
                             val selectCorrectionId : Js.js_string Js.t Js.prop
                             val checkboxSelfInfluenceId : Js.js_string Js.t Js.prop
                             val nbEventsId : Js.js_string Js.t Js.prop
                             val svgId : Js.js_string Js.t Js.prop
                             val rulesCheckboxesId : Js.js_string Js.t Js.prop
                             val height : int Js.t Js.prop
                             val width : int Js.t Js.prop
                           end
let constructor_configuration : flux_configuration Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_configuration ~(begin_time_id : string)
                         ~(end_time_id : string)
                         ~(select_correction_id : string)
                         ~(checkbox_self_influence_id : string)
                         ~(nb_events_id : string)
                         ~(svg_id : string)
                         ~(rules_checkboxes_id : string)
                         ~(height: int)
                         ~(width: int)
    : flux_configuration Js.t  =
  let configuration : flux_configuration Js.t = jsnew constructor_configuration () in
  let () = (Js.Unsafe.coerce configuration)##beginTimeId <- Js.string begin_time_id;
           (Js.Unsafe.coerce configuration)##endTimeId <- Js.string end_time_id;
           (Js.Unsafe.coerce configuration)##selectCorrectionId <- Js.string select_correction_id;
           (Js.Unsafe.coerce configuration)##checkboxSelfInfluenceId <- Js.string checkbox_self_influence_id;
           (Js.Unsafe.coerce configuration)##nbEventsId <- Js.string nb_events_id;
           (Js.Unsafe.coerce configuration)##svgId <- Js.string svg_id;
           (Js.Unsafe.coerce configuration)##rulesCheckboxesId <- Js.string rules_checkboxes_id;
           (Js.Unsafe.coerce configuration)##height <- height;
           (Js.Unsafe.coerce configuration)##width <- width
  in configuration

class type flux_data = object
                    val bioBeginTime : float Js.t Js.prop
                    val bioEndTime : float Js.t Js.prop
                    val rules : Js.js_string Js.js_array Js.t Js.prop
                    val hits : int Js.js_array Js.t Js.prop
                    val fluxs : int Js.js_array Js.js_array Js.t Js.prop
                  end

let constructor_data : flux_data Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_data ~(flux_begin_time : float)
                ~(flux_end_time : float)
                ~(flux_rules : string list)
                ~(flux_hits: int list)
                ~(flux_fluxs : float list list)
    : flux_data Js.t  =
  let data : flux_data Js.t = jsnew constructor_data () in
  let () = (Js.Unsafe.coerce data)##bioBeginTime <- flux_begin_time;
           (Js.Unsafe.coerce data)##bioEndTime <- flux_end_time;
           (Js.Unsafe.coerce data)##rules <- Js.array (Array.of_list (List.map Js.string flux_rules));
           (Js.Unsafe.coerce data)##hits <- Js.array (Array.of_list flux_hits);
           (Js.Unsafe.coerce data)##fluxs <- Js.array (Array.of_list (List.map (fun a -> Js.array (Array.of_list a)) flux_fluxs));
           ()
  in
  data

class type flux_map = object
			method exportJSON  : Js.js_string Js.t -> unit Js.meth
			method setFlux : flux_data Js.t -> unit Js.meth
                      end;;

let create_flux_map (configuration : flux_configuration Js.t) : flux_map Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "fluxMap")
                    [| Js.Unsafe.inject configuration |]
