(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let empty : Api_types_j.file_version = []
let create (client_id : Api_types_j.client_id) :
  Api_types_j.file_version =
  [{ Api_types_j.local_version_client_id = client_id ;
     Api_types_j.local_version_file_version = 0 ;
   }]

let increment
    (client_id : Api_types_j.client_id) :
  Api_types_j.file_version ->
  Api_types_j.file_version =
  List.map
    (fun local_version ->
       if local_version.Api_types_j.local_version_client_id = client_id then
         { local_version
           with Api_types_j.local_version_file_version = local_version.Api_types_j.local_version_file_version }
       else
         local_version
    )

let update
    (local_version : Api_types_j.local_version)
    (file_version : Api_types_j.file_version)
  : Api_types_j.file_version =
    let client_id = local_version.Api_types_j.local_version_client_id in
    if List.exists
        (fun v -> v.Api_types_j.local_version_client_id = client_id)
        file_version
    then
      List.map
        (fun v ->
           if v.Api_types_j.local_version_client_id = client_id then
             { v with Api_types_j.local_version_file_version =
                        max
                          local_version.Api_types_j.local_version_file_version
                          v.Api_types_j.local_version_file_version
             }
           else
             v
        )
        file_version
    else
      local_version::file_version

let rec merge
    (l : Api_types_j.file_version)
    (r : Api_types_j.file_version) :
  Api_types_j.file_version =
  match (l,r) with
  | ([],r) -> r
  | (lhead::ltail,r) -> merge ltail (update lhead r)

(* return type is
   (nothing less than,one greater than)
   make sure the client agrees on the time.
*)
type gt_state = { all_ge : bool ; one_gt : bool ; client_eq : bool ; }

let gt_local
    ?(client_id : Api_types_j.client_id option)
    (l : Api_types_j.local_version)
    (r : Api_types_j.file_version) : (gt_state * Api_types_j.file_version) =
  (* find r *)
  try let r_match =
        List.find
          (fun v -> v.Api_types_j.local_version_client_id =
                    l.Api_types_j.local_version_client_id) r in
    (* ensure our is greater or equal *)
    let all_ge = r_match.Api_types_j.local_version_file_version <=
                 l.Api_types_j.local_version_file_version in
    (* see if this current value is greater *)
    let one_gt = r_match.Api_types_j.local_version_file_version <
                 l.Api_types_j.local_version_file_version in
    (* if we are looking at the client's version
       make sure they are equal *)
    let client_eq = (Some l.Api_types_j.local_version_client_id = client_id) &&
                    (r_match.Api_types_j.local_version_client_id =
                     l.Api_types_j.local_version_client_id)
    in
    (* filter our l out of r *)
    let new_r =
      List.filter
        (fun v -> v.Api_types_j.local_version_client_id <>
                  l.Api_types_j.local_version_client_id)
        r
    in
    ({ all_ge = all_ge ; one_gt = one_gt ; client_eq = client_eq ; },new_r)
  with Not_found -> (* l not in r : this is okay as r is a subset of l *)
    ({ all_ge = true ; one_gt = false ; client_eq = false ; },r)

(* l > r
   r is a subset of l
   forall r[i] in r : r[i] =< l[i] and there is at least one r[i] < l[i]
   for client make sure r[i] = l[i] e.g. the client has synched.
*)
let rec gt_cmp
    ?(client_id : Api_types_j.client_id option)
    (l : Api_types_j.file_version)
    (r : Api_types_j.file_version) : gt_state =
  match (l,r) with
  | ([],[]) -> { all_ge = true ; one_gt = false ; client_eq = false ;}
  | ([],_::_) -> { all_ge = false ; one_gt = false ; client_eq = false ; }
  | (l_head::l_version,r) ->
    let (gt_state_1,r_version) = gt_local ?client_id l_head r in
    let gt_state_2 = gt_cmp l_version r_version in
    { all_ge    = gt_state_1.all_ge    && gt_state_2.all_ge    ;
      one_gt    = gt_state_1.one_gt    || gt_state_2.one_gt    ;
      client_eq = gt_state_1.client_eq || gt_state_2.client_eq ;
    }

let gt
    ?(client_id : Api_types_j.client_id option)
    (l : Api_types_j.file_version)
    (r : Api_types_j.file_version) : bool =
  let gt_state = gt_cmp ?client_id l r in
  (match client_id with
   | None -> true
   | Some _ -> gt_state.client_eq) &&
  (gt_state.all_ge = true) &&
  (gt_state.one_gt = true)
