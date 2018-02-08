open Intervalles
open Fraction
open Config_aff
open Occu1
(*open Occu2
*)
module type Tabinter =
 sig
   type var
   type intervalle
   type intervalle_tab
   val create : int -> intervalle_tab
   val set  : intervalle_tab -> var -> intervalle -> unit
   val read : intervalle_tab -> var -> intervalle
val affiche:intervalle_tab -> unit
   val copy : intervalle_tab -> intervalle_tab
   val clef : intervalle_tab -> var list
   val wide_place : intervalle_tab -> intervalle_tab -> var list
   val union : intervalle_tab -> intervalle_tab -> intervalle_tab
   val somme : intervalle_tab -> intervalle_tab -> intervalle_tab
   val int_of_var_list : var list -> intervalle_tab
   val push : intervalle_tab -> var -> fraction  -> intervalle_tab
val pushbool : intervalle_tab -> var -> intervalle_tab
   val equal : intervalle_tab -> intervalle_tab -> bool
   val merge :   intervalle_tab -> intervalle_tab -> intervalle_tab
 end

module Tabinter =
 struct
   type var = trans
   type intervalle=Intervalles.intervalle
   type intervalle_tab = {i:(var,intervalle) Hashtbl.t;
			  k:var Working_list_aff.working_list}
   let create n =
     {i=Hashtbl.create n;
      k=Working_list_aff.new_working_list n}


  let set t lambda v =
    ((t.k).Working_list_aff.push lambda;
    (try (Hashtbl.remove (t.i) lambda) with _ -> ());
    Hashtbl.add (t.i) lambda v)
  let read t lambda =
    try (Hashtbl.find (t.i) lambda)
    with _ -> (let v = {inf=Frac{num=0;den=1};sup=Frac{num=0;den=1}} in
	        (set t lambda v;v))
  let copy t =
    let (j:intervalle_tab) =create Config_aff.hashnumber in
    (List.iter
       (fun x->set j x (read t x))
       ((t.k).Working_list_aff.list ()));j

   let affiche t =
     (List.iter
	(fun x->print_newline ();print_string "<Br>";Occu1.print_trans  x;
	        print_string ":";
	        print_string (string_of_intervalle (read t x)))
	((t.k).Working_list_aff.list ());print_newline ();print_string "<Br>")



   let clef t = t.k.Working_list_aff.list ()
   let wide_place t1 t2 =
     let l=Working_list_aff.new_working_list Config_aff.hashnumber in
     let spe_push p = l.Working_list_aff.push p in
     let changed=(ref false) in
     let traite p =
       changed:=false;
       let rep={inf=(
		 if (ffinf ((read t2 p).inf) ((read t1 p).inf))
	      then
		(changed:=true;Frac {num=0;den=1})
              else ((read t1 p).inf));
             sup=(
	      if (ffinf ((read t1 p).sup) (read t2 p).sup)
	      then
                (if (ffinf (Frac (!wide_max)) (read t1 p).sup)
		then
		  (if (read t1 p).sup=Infinity
		  then Infinity
		  else (changed:=true;Infinity))

		else (changed:=true;(read t2 p).sup))
	      else ((read t1 p).sup))} in
    if (!changed) then (spe_push p;set t1 p rep) else ()
    in (List.iter traite (clef t2);
	List.iter traite (clef t1);
	l.Working_list_aff.list ())
  let somme t1 t2 =
     let l=Working_list_aff.new_working_list Config_aff.hashnumber in
     let spe_push p = l.Working_list_aff.push p in
     (List.iter spe_push (clef t1);List.iter spe_push  (clef t2);
      let rep = create hashnumber in
      List.iter (fun x->set rep x  (iiplus (read t1 x) {num=1;den=1} (read t2 x))) (l.Working_list_aff.list ());
      rep)
 let union  t1 t2 =
     let l=Working_list_aff.new_working_list Config_aff.hashnumber in
     let spe_push p = l.Working_list_aff.push p in
     (List.iter spe_push (clef t1);List.iter spe_push  (clef t2);
      let rep = create hashnumber in
      List.iter (fun x->set rep x  (union (read t1 x)  (read t2 x))) (l.Working_list_aff.list ());
      rep)
  let merge t1 t2 =
    List.iter (fun x->set t1 x (read t2 x)) (clef t2);
    t1


  let int_of_var_list l =
    let i=create Config_aff.hashnumber in
    List.iter (fun x->set i x {inf=Frac{num=1;den=1};sup=Frac{num=1;den=1}}) l;
    i
  let push i x k  =
    set i x (iiplus (read i x) k {inf=Frac{num=1;den=1};sup=Frac{num=1;den=1}});i

   let pushbool i x =
	set i x {inf=Frac{num=1;den=1};sup=Frac{num=1;den=1}};i
  let equal i1 i2 =
    List.for_all (fun x-> (read i1 x)=(read i2 x)) ((clef i1)@(clef i2))
 end
