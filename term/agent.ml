type t = int * int
(** agent_id * agent_type *)

let print ?sigs f (id,ty) =
  match sigs with
  | None -> Format.pp_print_int f id
  | Some sigs -> Format.fprintf f "%a_%i" (Signature.print_agent sigs) ty id

let to_json (id,ty) = `Assoc ["id", `Int id; "type", `Int ty]
let of_json = function
  | `Assoc ["id", `Int id; "type", `Int ty]
  | `Assoc ["type", `Int ty; "id", `Int id] -> (id,ty)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x))

