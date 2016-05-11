class type contact_map =
object
  method exportJSON  : Js.js_string Js.t -> unit Js.meth
  method setData : Js.js_string Js.t -> unit Js.meth
  method clearData : unit Js.meth
end;;

let create_contact_map (id : string) : contact_map Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "ContactMap")
                    [| Js.Unsafe.inject (Js.string id) |]
