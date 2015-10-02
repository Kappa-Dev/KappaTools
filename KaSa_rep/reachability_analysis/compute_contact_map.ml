 (**
  * side_effect.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 11th of September
  * Last modification: 
  * 
  * Compute the contact map 
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Int_storage
open Cckappa_sig
open Printf

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Contact Map") message exn
                 (fun () -> default)                

let trace = false

(*****************************************************************************************)
(*TYPE*)
  
module Int2Map =
  MapExt.Make (
    struct
      type t = Cckappa_sig.agent_name * Cckappa_sig.site_name * Cckappa_sig.state_index
      let compare = compare
    end
  )

type binding_dual = ((int list) * (int * int * int) list) Int2Map.t

let precise_binding_dual parameter error handler rule sol_binding =
  let sol = Int2Map.empty in
  (*add_link*)
  let add_link (a, b, s) (c, d, s') sol =
    let l, old =
      try Int2Map.find (a, b, s) sol
      with Not_found -> [],[]
    in
    Int2Map.add (a, b, s) (l, ((c, d, s') :: old)) sol
  in  
  (*return the site name of site: this of type string*)
  (*folding this solution with the information in dual*)
  let error, sol =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state') sol ->
        List.fold_left (fun (error, sol2) (add1, add2) ->
          let agent_type1 = add1.agent_type in
          let site1       = add1.site in
          (*second*)
          let agent_type2 = add2.agent_type in
          let site2       = add2.site in
          if agent_type1 = agent && site1 = site &&
            agent_type2 = agent' && site2 = site'
          then
            (*if true return the solution*)
            error, sol2
          else
            (*if not true, add those links*)
            let sol = add_link (agent_type1, site1, state) (agent_type2, site2, state') sol2
            in
            error, sol
        ) (error, sol_binding) rule.actions.bind
      ) handler.dual sol
  in
  (*Return the result of this contact map*)
  let sol = Int2Map.map (fun (l, x) -> List.rev l, x) sol
  in
  sol

let print_precise parameter error precise =
  Int2Map.iter
    (fun (x, y, s) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.Remanent_parameters_sig.log 
            "agent_type:%i@site_type:%i:state:%i" x y s in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.Remanent_parameters_sig.log ", ");
              fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z, t, s') ->
	  Printf.fprintf parameter.Remanent_parameters_sig.log
            "agent_type:%i@site_type:%i:state:%i--agent_type':%i@site_type':%i:state':%i\n"
            x y s z t s'
	) l2
    ) precise

(************************************************************************************)   
(*Dual information and binding information*)

module Int2Map_pair =
  MapExt.Make (
    struct
      type t = int * int
      let compare = compare
    end
  )

type contact_map = ((int list) * (int * int) list) Int2Map_pair.t

let compute_contact_map parameter error handler =
  (*create empty map*)
  let sol = Int2Map_pair.empty in
  (*add_link*)
  let add_link (a, b) (c, d) sol =
    let l, old =
      try Int2Map_pair.find (a, b) sol
      with Not_found -> [],[]
    in
    Int2Map_pair.add (a, b) (l, ((c, d) :: old)) sol
  in  
  (*return the site name of site: this of type string*)
  (*folding this solution with the information in dual*)
  let error, sol =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state') sol ->
	let sol = add_link (agent, site) (agent', site') sol
	in
	error, sol
      ) handler.dual sol
  in
  (*Return the result of this contact map*)
  let sol = Int2Map_pair.map (fun (l, x) -> List.rev l, x) sol
  in
  Some sol

(* - Only take the binding information of the rhs; Ex: A(x), B(x) -> A(x!1), B(x!1)
   and not the binding in the lhs; Ex: A(x!1), B(x!1) -> A(x), B(x)
   - Getting information of binding state, then search if these binding belong to dual.
*)

let collect_binding_rhs parameter error rule sol =
  let add_link (a, b) (c, d) sol =
    let l, old =
      try Int2Map_pair.find (a, b) sol
      with Not_found -> [], []
    in
    Int2Map_pair.add (a, b) (l, ((c, d) :: old)) sol
  in
  let error, sol =
    List.fold_left (fun (error, sol) (add1, add2) ->
      let agent_type1 = add1.agent_type in
      let site1       = add1.site in
      (*second*)
      let agent_type2 = add2.agent_type in
      let site2       = add2.site in
      (*new*)
      let sol = add_link (agent_type1, site1) (agent_type2, site2) sol in
      error, sol
    ) (error, sol)
      rule.actions.bind
  in
  let sol = Int2Map_pair.map (fun (l, x) -> List.rev l, x) sol in
  sol

let print_collect_binding_rhs parameter error binding =
  Int2Map_pair.iter
    (fun (x, y) (l1, l2) ->
      if l1 <> []
      then
        begin 
          let _ = fprintf parameter.Remanent_parameters_sig.log 
            "agent_type:%i@site_type:%i" x y in
          let _ = List.fold_left
            (fun bool x ->
              (if bool
               then
                  fprintf parameter.Remanent_parameters_sig.log ", ");
              fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
              true)
            false l1
          in
          fprintf stdout "\n"
        end
      else ();
      List.iter
	(fun (z,t) ->
	  Printf.fprintf parameter.Remanent_parameters_sig.log
            "agent_type:%i@site_type:%i--agent_type':%i@site_type':%i\n"
            x y z t 
	) l2
    ) binding

let print_contact_map parameter error contact_map =
  let _ = fprintf stdout "Contact map:\n" in
  match contact_map with
  | Some p ->
    Int2Map_pair.iter
      (fun (x, y) (l1, l2) ->
        if l1 <> []
        then
          begin 
            let _ = fprintf parameter.Remanent_parameters_sig.log 
              "agent_type:%i@site_type:%i" x y in
            let _ = List.fold_left
              (fun bool x ->
                (if bool
                 then
                    fprintf parameter.Remanent_parameters_sig.log ", ");
                fprintf parameter.Remanent_parameters_sig.log "agent_type:%i" x;
                true)
              false l1
            in
            fprintf stdout "\n"
          end
        else ();
        List.iter
	  (fun (z,t) ->
	    Printf.fprintf parameter.Remanent_parameters_sig.log
              "agent_type:%i@site_type:%i--agent_type':%i@site_type':%i\n"
              x y z t 
	  ) l2
      ) p
  | None -> ()
