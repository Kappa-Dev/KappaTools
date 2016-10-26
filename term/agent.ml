type t = int * int
(** agent_id * agent_type *)

let print ?sigs ~with_id f (i,ty) =
  match sigs with
  | Some sigs ->
    Format.fprintf f "%a%t" (Signature.print_agent sigs) ty
      (fun f -> if with_id then Format.fprintf f "/*%i*/" i)
  | None -> Format.fprintf f "n%i" i

let print_site ?sigs (i,agent) f id =
  match sigs with
  | Some sigs ->
    Signature.print_site sigs agent f id
  | None -> Format.fprintf f "n%is%i" i id

let print_internal ?sigs (i,agent) site f id =
  match sigs with
  | Some sigs ->
    Signature.print_site_internal_state sigs agent site f (Some id)
  | None -> Format.fprintf f "n%is%i~%i" i site id

let rename inj (n_id,n_ty) = (Renaming.apply inj n_id,n_ty)

let sort (_,ty) = ty
let id (id,_) = id


let to_json (id,ty) = `Assoc ["id", `Int id; "type", `Int ty]
let of_json = function
  | `Assoc ["id", `Int id; "type", `Int ty]
  | `Assoc ["type", `Int ty; "id", `Int id] -> (id,ty)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x))

