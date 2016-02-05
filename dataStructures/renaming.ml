open Mods

exception Undefined
exception NotBijective
exception Clashing

type t = {sigma:int IntMap.t ; is_identity:bool}

let empty = {sigma = IntMap.empty ; is_identity=true}
let identity l =
  {sigma = List.fold_left (fun out x -> IntMap.add x x out) IntMap.empty l;
   is_identity = true}
let is_identity i = i.is_identity

let to_list i = IntMap.bindings i.sigma

let unsafe_add x y i =
  {sigma = IntMap.add x y i.sigma ; is_identity = i.is_identity && x==y}
let add x y i =
  let not_ok = !Parameter.debugModeOn && IntMap.mem x i.sigma in
  if not_ok then raise Clashing else unsafe_add x y i

let rec cyclic_permutation_from_identity max id subst pre = function
  | _ when pre = id -> unsafe_add pre max subst
  | [] -> assert false
  | h :: t ->
     cyclic_permutation_from_identity max id (unsafe_add pre h subst) h t
let cyclic_permutation_from_list ~stop_at = function
  | [] -> failwith "Renaming.cyclic_permutation_from_list"
  | h :: t ->
     cyclic_permutation_from_identity h stop_at empty h t

let mem x i = IntMap.mem x i.sigma
let fold f i = IntMap.fold f i.sigma

let apply i x =
  if not i.is_identity || !Parameter.debugModeOn then
    match IntMap.find_option x i.sigma with
    | Some x -> x
    | None -> raise Undefined
 else x

let compose extensible i i' =
  if not i.is_identity || extensible || !Parameter.debugModeOn then
    let sigma,is_id =
      IntMap.fold (fun x y (out,is_id) ->
	match IntMap.find_option y i'.sigma with
	| Some z -> (IntMap.add x z out,is_id && x==z)
	| None -> (out,is_id && x==y)
      ) i.sigma (i.sigma,true)
    in
    {sigma=sigma ; is_identity=is_id}
  else i'

let inverse i =
   if i.is_identity then i
   else
   let sigma = 
       IntMap.fold (fun x y out ->
        if IntMap.mem y out then raise NotBijective
        else IntMap.add y x out) i.sigma IntMap.empty
   in
   {sigma = sigma ; is_identity = i.is_identity}

let compare i i' = IntMap.compare int_compare i.sigma i'.sigma
let equal i i' = (compare i i') = 0

let print f i =
  ignore
    (IntMap.fold
       (fun src dst b ->
	if src <> dst then
	  let () =
	    Format.fprintf f "%t%i->%i" (if b then Pp.comma else Pp.empty)
			   src dst in
	  true
	else b
       ) i.sigma false)

let print_full f i =
  Format.fprintf
    f "@[(%a)@]"
    (Pp.set IntMap.bindings Pp.comma
	    (fun f (src,dst) -> if src<>dst then Format.fprintf f "%i->%i" src dst
				else Format.pp_print_int f src)) i.sigma
