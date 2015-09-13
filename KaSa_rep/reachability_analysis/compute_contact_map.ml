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
      type t = int * int
      let compare = compare
    end
  )

type contact_map = ((int list) * (int * int) list) Int2Map.t

let compute_contact_map parameter error handler =
  (*create empty map*)
  let sol = ref Int2Map.empty in
  (*add_link*)
  let add_link (a, b) (c, d) sol =
    let l, old =
      try Int2Map.find (a, b) sol
      with Not_found -> [],[]
    in
    Int2Map.add (a, b) (l, ((c, d) :: old)) sol
  in  
  (*return the site name of site: this of type string*)
  (*folding this solution with the information in dual*)
  let sol = !sol in
  let error, sol =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state') sol ->
	let _ = fprintf stdout "agent:%i;site:%i;state:%i\n" agent site state in
	let _ = fprintf stdout "agent':%i;site':%i;state':%i\n" agent' site' state' in
	let sol = add_link (agent, site) (agent', site') sol
	in
	error, sol
      ) handler.dual sol
  in
  (*Return the result of this contact map*)
  let sol = Int2Map.map (fun (l, x) -> List.rev l, x) sol
  in
  Some sol

let rec get_contact_map parameter error handler contact_map =
  match contact_map with
    | Some x -> x
    | None -> get_contact_map parameter error handler
	(compute_contact_map parameter error handler)
