open Tools_aff
open Hashtbl
open Fraction
open Intervalles
open Occu1
open Intertab


let trace=false ;;
let t_i i = if trace then (print_int i;print_newline ()) ;;
let t_s x = if trace then (print_string x;print_newline ());;
let affiche_frac f = if trace then (print_string "num: ";print_int f.num;print_string " den : ";print_int f.den) else ();;
let affiche_int i =
 (match i.inf with Frac a-> t_s "INF";affiche_frac a
|  Minfinity -> print_string "minf" | Unknown -> print_string "!U!"
|  Infinity -> print_string "!Inf!");
(match i.sup with Frac a-> t_s "SUP";affiche_frac a
|  Infinity -> print_string "Inf" | _  -> ());;
let affiche_inter=affiche_int;;

let affiche_cons (k,c,b) =
  List.iter (fun x->let a=(try (find c x) with _ -> {num=0;den=1}) in print_int  x;print_newline ();affiche_frac a;print_newline ()) k;
  affiche_int b;;

type point = {coord:unit -> (trans*fraction) list;
	      proj:trans -> fraction;
	     set:trans->fraction->unit;
		affiche_point:unit -> unit;
	     somme_point:point->unit};;

type line = (trans list * (trans, Fraction.fraction) Hashtbl.t);;


module type Matrice =
   sig
     type matrice
     type var
     val mat_of_var_list : var list -> matrice
     val new_matrice: int->matrice
     val affiche: matrice->unit
     val get_all_entry:matrice->var Working_list_aff.working_list
     val add_entry:matrice->var->unit
     val plonge:matrice->var list -> matrice
     val copy:matrice -> matrice
     val normalise:matrice->unit
     val push:matrice->var->Fraction.fraction->unit
     val pushbool:matrice -> var -> matrice
     val new_copy_ligne:matrice->line->unit
     val new_empty_ligne:matrice->unit
     val n_ligne:matrice->int
     val get_line:matrice->int->line
     val merge:matrice->matrice->matrice
     val pivot:matrice->int->var
     val read_val:matrice->int->var->fraction
     val addligne:matrice->int->fraction->int->unit
     val swap:matrice->int->int->unit
     val mulligne:matrice->int->fraction->unit
     val del_ligne:matrice->int->unit
     val del_last_ligne:matrice->unit
     val union:matrice->matrice->matrice
     val get_all_key:matrice->var  list
     val is_key:matrice->var->bool
     val decomp_affine:matrice->(point*matrice)
     val somme_affine:matrice->matrice->matrice
     val add_0:matrice->matrice
 end


let new_point n =
  let entry=Working_list_aff.new_working_list n in
  let content=Hashtbl.create n in
  let read k =
    try (find content k) with _ -> {num=0;den=1} in
  let set k f =
    ((try (remove content k) with _ -> ());
     Hashtbl.add content k f;
     entry.Working_list_aff.push k) in
  let somme_point ob =
    List.iter (fun (x,y)->set x (fplus y (read x))) (ob.coord ()) in

  {proj=read;
    set=set;
   coord=(fun () -> (List.map (fun x->(x,read x)) (List.sort p
 (entry.Working_list_aff.list ()))));
    affiche_point =
(fun () -> (List.iter (fun x->((*print_trans x;print_string ":";*)
affiche_frac (read x))) (List.sort p (entry.Working_list_aff.list ()))));
  somme_point=somme_point}


module Mat =
  (struct
    type var = Occu1.trans
    type matrice = {entry:(int,var list) Hashtbl.t;
		    content:(int,(var,fraction)Hashtbl.t) Hashtbl.t;
		    all_entry:var Working_list_aff.working_list;
	            nligne:int ref;
              	    sorted_entry:var list ref}
    let new_matrice n =
       {entry=Hashtbl.create n;
        content=Hashtbl.create n;
	nligne=ref 0;
        all_entry=Working_list_aff.new_working_list n;
        sorted_entry=ref []}
    let get_all_entry m =(m.all_entry)

    let is_key m i =
      m.all_entry.Working_list_aff.member i
    let get_all_key m  = (!(m.sorted_entry))
    let n_ligne m =(!(m.nligne))
    let add_entry  m j =
        if is_key m j
        then ()
	else (m.all_entry.Working_list_aff.push j;
	      m.sorted_entry:=insert_sort po  (!(m.sorted_entry)) j)
    let copy_var m n  =
       List.iter (add_entry n) (m.all_entry.Working_list_aff.list ())
    let new_empty_ligne m  =
        (m.nligne:=(!(m.nligne))+1;
         Hashtbl.add m.entry (!(m.nligne)) [];
         Hashtbl.add m.content
           (!(m.nligne)) (Hashtbl.create Config_aff.hashnumber))

    let new_copy_ligne m (ent,cont) =
        (m.nligne:=(!(m.nligne))+1;
         Hashtbl.add m.entry (!(m.nligne)) ent;
         let tmp=(Hashtbl.create Config_aff.hashnumber) in
         (List.iter (fun x->(Hashtbl.add tmp x (find cont x);
			     add_entry m  x)) ent;
          Hashtbl.add m.content (!(m.nligne)) tmp))


    let read_val m i j =
        try (find (find m.content i) j) with _ -> {num=0;den=1}
let affiche m  =
      print_string "VAR ";
      List.iter (fun i->print_trans i;print_string " ") ((get_all_key m));
      for i=1 to (!(m.nligne)) do
	print_string "---";print_string "<Br>";

        List.iter (fun x-> print_trans x;print_string " num:";print_int (read_val m i x).num;print_string" den: ";print_int (read_val m i x).den;print_string "<Br>") (find m.entry i);print_newline ();print_string "<Br>"
      done;print_string "<Br><Br>";print_newline ();print_newline ()




  let new_eps_matrice l n =
        let m=new_matrice n in

             (List.iter (fun x->if (not (Pro(-1)=x)) then (new_copy_ligne m ([x],(let h=Hashtbl.create n in
		                 			   (Hashtbl.add h x {num=1;den=1};h)))))
             (List.sort (fun x->fun y->p y x) l);(*print_string "EPS";*)
	      (*affiche m;*)m)



    let safe_assign m i (j:var) k =
	let l=find m.entry i in
        (remove m.entry i;
        (try (remove (find m.content i) j) with _ -> ());
         if k.num=0 then (Hashtbl.add m.entry i (sub_list po l j))
                    else (add_entry m j;
	                  Hashtbl.add (find m.content i) j k;
			  Hashtbl.add m.entry i (insert_sort po l j)))

    let del_ligne m i =
        (if i<(!(m.nligne)+1) then (Hashtbl.remove m.entry i;
			      Hashtbl.remove m.content i;
			      for j=(i+1) to (!(m.nligne)) do
				Hashtbl.add m.entry (j-1) (Hashtbl.find m.entry j);
				Hashtbl.add m.content (j-1) (Hashtbl.find m.content j);
				Hashtbl.remove m.entry j;
				Hashtbl.remove m.content j
			      done;
			      m.nligne:=(!(m.nligne))-1))
    let rec set_val m i j k =
        if i>(!(m.nligne)) then (new_empty_ligne m;set_val m i j k)
        else (safe_assign m i j k)
    let rec swap m i j =
        if (i=j) then () else
          (if (i>(!(m.nligne))) || (j>(!(m.nligne))) then (new_empty_ligne m;swap m i j)
					  else (let entryi=find m.entry i in
                                                let contenti=find m.content i in
						(remove m.entry i;
						 remove m.content i;
                                                 Hashtbl.add m.entry i (find m.entry j);
						 Hashtbl.add m.content i (find m.content j);
						 remove m.entry j;
                                                 remove m.content j;
                                                 Hashtbl.add m.entry j entryi;
                                                 Hashtbl.add m.content j contenti)))
    let mulligne m i alpha =
        if ((i>(!(m.nligne)))) then ()
        else
           (List.iter (fun j->(let k=read_val m i j in
                               set_val m i j (ffois alpha k))) (find m.entry i))


    let addligne m i alpha j =
	if (alpha.num=0) then () else
        (let li=find m.entry i in
        let lrep=merge po li (find m.entry j) in
        (remove m.entry i;
         if (j>(!(m.nligne))) then ()
         else
           (List.iter (fun x->(let k=fplus (ffois alpha (read_val m j
x)) (read_val m i x) in ((try (remove (find m.content i) x) with _ -> ());
                       add (find m.content i) x  k))) lrep;
            Hashtbl.add m.entry i (vide (fun x->((read_val m i x).num=0))
lrep))))

     let rec del_last_ligne m  =
        if (!(m.nligne)>0) then
        (match (find m.entry (!(m.nligne))) with [] -> (remove m.entry (!(m.nligne));
						 remove m.content (!(m.nligne));
						 m.nligne:=(!(m.nligne))-1;
					         del_last_ligne m)
                                    | _ -> ())
	else ()

    let pivot m i =
        if i>(!(m.nligne)) then failwith "compteur_pivot_1"
                       else (match (find m.entry i) with (Pro (-1))::k::q -> k
						     |   [Pro (-1)]  -> failwith "compteur_pivot_2"
						     |   k::q -> k
                                                     |   _    ->  failwith "compteur_pivot_3")


    let normalise m  =
        let rec aux k =
            (if k>(!(m.nligne)) then  (del_last_ligne m)
             else (let rec search_good_ligne l rep  wei =
		if l>(!(m.nligne)) then (rep,wei)
		else (
                try (let cur=(pivot m l) in
                     if (po cur wei) || (wei=(Pro(-1)))
		     then (search_good_ligne (l+1) l cur)
		     else (search_good_ligne (1+l) rep wei)
                     ) with _ -> search_good_ligne (1+l) rep wei) in
             let new_ligne,wei=search_good_ligne k (-1) (Pro(-1)) in
            if wei=(Pro(-1)) then aux ((!(m.nligne))+1) else
		begin
		let col=pivot m new_ligne in
                (
                 swap m k (new_ligne);
		 mulligne m k (fdiv {num=1;den=1} (read_val m k col));

                  for i=1 to (!(m.nligne)) do
		     if i=k then ()
			    else (let alpha=ffois {num=(-1);den=1}
(read_val m i col) in  addligne m i alpha  k)
		 done;
		 aux (k+1))
		end))  in aux 1



    let rec push m j k =
      if ((get_all_entry m).Working_list_aff.member j) then
        (for i=1 to (!(m.nligne)) do
            let rep=read_val m i (Pro (-1)) in
            let r=read_val m i j in
            set_val m i (Pro (-1)) (fplus rep ((let f=(ffois r k) in {num=(-f.num);
							       den=f.den})))
        done )
      else
	((*print_trans j;*)
      new_copy_ligne m ([j],(let h=Hashtbl.create Config_aff.hashnumber in
	                        Hashtbl.add h j {Fraction.num=1;Fraction.den=1};h));normalise m;push m j k)

	let get_line m k =
            try (find m.entry k,find m.content k) with _ -> ([],Hashtbl.create Config_aff.hashnumber)

	let merge m m2 =
	    let new_m = new_matrice Config_aff.hashnumber in
	    List.iter (add_entry new_m) ((get_all_entry m).Working_list_aff.list ());
	    List.iter (add_entry new_m) ((get_all_entry m2).Working_list_aff.list ());
	    let n2=n_ligne m2 in
            let n1=n_ligne m  in
	     let avant k1 k2 =
                match (try (pivot m k1) with _ -> (Pro (-1))),
		      (try (pivot m2 k2) with _ -> (Pro (-1)))
		with (Pro (-1),Pro (-1)) -> true
		   | (Pro (-1),_) -> false
		   |  (_ ,Pro (-1)) -> true
		   |  (a,b)     -> (po a b) in
            let rec aux k1 k2 =
                match (k2>n2),(k1>n1),(avant k1 k2)

			with
			   true,true,_ ->  new_m
			 | _,_,true  -> (new_copy_ligne new_m (get_line m k1);
	            		  		aux (k1+1) k2)
			 |  _   -> (new_copy_ligne new_m (get_line m2 k2);  aux k1 (k2+1))
	    in
	    let mrep=aux 1 1 in
            (

	     mrep)

        let safe_merge m m2 =
            let rep = merge m m2 in
	    (normalise rep;
            rep)
        let copy m  =
	    	let cop_line (k,c) =
                let nl=Hashtbl.create Config_aff.hashnumber  in
		(List.iter (fun x->(Hashtbl.add nl x (Hashtbl.find c x))) k;
		 (k,nl)) in
	        let nm=new_matrice Config_aff.hashnumber in
          (List.iter (fun x->add_entry nm x) (m.all_entry.Working_list_aff.list ());
            for i=1 to (!(m.nligne)) do
		new_copy_ligne nm ((cop_line:'b->'b) (get_line m i))
	    done;
	    nm)



	let decomp_affine m  =
	    let n=Config_aff.hashnumber in
	    let nm=new_matrice n in
	    copy_var m nm;
            let o=new_point n in
	    let cop_line (k,c) =
                let nl=Hashtbl.create n in
		(List.iter (fun x->(Hashtbl.add nl x (Hashtbl.find c x))) k;
		 (k,nl)) in
            let forget_affine l =
	      match l with
		(Pro(-1))::q -> q
	      |	_ -> l in
	    for i=1 to (!(m.nligne)) do

	      new_copy_ligne nm ((let (k,c)=(get_line m i) in
	                        cop_line  (forget_affine k,c)));
	      o.set (pivot m i) (let f=(read_val m i (Pro(-1))) in {num=(-f.num);den=f.den})
	    done;
	    (o,nm)

  let mat_of_var_list l =
      let m=new_eps_matrice l (Config_aff.hashnumber) in
(*      affiche m;*)
      List.iter (fun x->push m x {num=1;den=1}) l;
(*      affiche m;*)
      m






        let plonge m l =


	  (*List.iter print_trans (all_entry.Working_list_aff.list ());*)
	  let nm=new_eps_matrice (filtre (fun p-> not (m.all_entry.Working_list_aff.member p)) l) Config_aff.hashnumber in

	   merge m nm
	let unify_domain m m2 =
	  let key1=List.filter (fun x->(not (List.mem x (get_all_key m) ))) (get_all_key m2) in
	  let key2=List.filter (fun x->(not (List.mem x (get_all_key m2) ))) (get_all_key m) in
	  (plonge m key1,plonge m2 key2)

        let union ma nb =                  (* union affine *)
(*           print_string "argument";
           affiche ma;
           affiche nb;*)

    (*normalise ();
	    nb.normalise ();*)
(*	    affiche ma;affiche nb;*)
	    let ma,mb = unify_domain ma nb  in
	  (*  print_string "unified";
	    affiche ma;affiche mb;*)
            let _m=max (n_ligne ma) (n_ligne mb) in
	    let traite (s:trans) (r:int) =
	        (*print_int r;*)
                match (read_val ma r s,read_val mb r s)
		with

                   {num=1;den=1},{num=1;den=1} -> (*print_string "A";*)(r+1)
                 | {num=1;den=1},_ ->
                  (* print_string "B";*)begin
                   for i=1 to (r-1) do
                       let a=read_val mb i s in
                       addligne ma  i a  (r)
                   done;
                  (* print_string  "delete";*)
                   del_ligne ma (r);
                  (* affiche ma;*)
                   r
                   end

                 | _,{num=1;den=1} ->
                   (*print_string "C";*)begin
                   for i=1 to (r-1) do
                       let a=read_val ma i s in
                       addligne mb i a (r)

                   done;
                   del_ligne mb (r);
                   r
                   end
                 | _ ->
                 ((*print_string "D";*)
                 let (t:int)=
		   (let rec find t =
                       if t<0
		       then 0
                       else
			 (if ((read_val ma t s)=(read_val mb t s))
			 then find (t-1)
			 else t)
                   in find (r-1)) in

                    (if (t>0) then
                    (for i=1 to (t-1) do
                        let a=fdiv (fmoins (read_val ma  i s) (read_val mb i s))
                                   (fmoins (read_val mb t s) (read_val ma t s))
                        in
                        addligne ma i a t;
                        addligne mb i a t;
                     done;
                     del_ligne ma t;
                     del_ligne mb t;r-1) else
		     r)) in
            let rec algo r sliste =
	      match sliste  with
		(Pro(-1))::squeue -> algo r squeue
	      |	 s::squeue -> let rprim = (traite s r )
		                              in algo rprim squeue
	      |	[] -> ignore (traite  (Pro (-1)) r) in
	    (algo      1
	       (let l=fusion (fun x->fun y->(po x y))
		       (get_all_key ma) (get_all_key mb) in
                (*List.iter print_trans l;*)l);
	     (*normalise ma;affiche ma;*)ma)
    let recomp_affine ((o:point),m) =
        let nm=(copy m) in
        (List.iter (fun (x,y)->(push nm x y)) (o.coord ());
        nm)



	let somme_affine ma mb =
	  let ma=copy ma in
	  let mb=copy mb in
	  let ma,mb = unify_domain ma mb in
          (*print_string "SOMME";
	  affiche ma;
	  affiche mb;*)
          let (oa,ma)=decomp_affine ma in
	  let (ob,mb)=decomp_affine mb in
	  (
	  (*print_string "ma";
          affiche ma;
          print_string "mb";
          affiche mb;*)
          oa.somme_point ob;
	  let mc=union ma mb in
	  ((*affiche mc;*)
	  let (rep)=recomp_affine (oa,mc)in ((*affiche rep;*)rep)))





let add_0 m =
 let l=((get_all_entry m).Working_list_aff.list ()) in
  let nm=new_eps_matrice l Config_aff.hashnumber in
  ((*print_string "000<BR>";
  nm.affiche ();
  m.affiche ();*)
  union nm m)

let pushbool m a =
  let m2 = copy m in
  let m2 = (new_copy_ligne m2 ([a],let h = create 1 in
  (add h a {Fraction.num=0;Fraction.den=1};h));m2) in
  let m2 = (push m2 a {Fraction.num=1;Fraction.den=1};m2) in
  let m2 = (normalise m2;m2) in
  let m3 = copy m in
  let m3 = (new_copy_ligne m3 ([a],let h = create 1 in
  (add h a {Fraction.num=1;Fraction.den=1};h));m3) in
  let m3 = (normalise m3;m3) in
  let m =  union m2 m3 in
  (normalise m;m)


end:Matrice with type var=Occu1.trans)





(*let aff_continuation2 pro choix cont =
  (*print_string "AFF";
  print_trans pro;*)
  let nm=new_eps_matrice (if (pro=(Pro (-1))) then cont else (pro::cont)) Config.hashnumber in
  let init=ref true in
  let rep=ref (nm.copy ()) in
  ((if (pro=(Pro (-1))) then () else (nm.push pro {num=(-1);den=1});
   List.iter
     (fun c ->
       let newm=nm.copy () in
       (List.iter
	 (fun p->newm.push p {num=1;den=1})
	 c;
	 if (!init)
	 then
	   (rep:=newm;(*newm.affiche ();*)
	    init:=false)
	 else
	   (rep:=(!rep).union newm
	    (*;(!rep).affiche ()*))))
     choix);(!rep)) ;;
(*let m=aff_continuation2 (-1) [[1];[2]] [1;2];;
m.affiche ();;*)


let aff_continuation =aff_continuation2 ;;


*)
