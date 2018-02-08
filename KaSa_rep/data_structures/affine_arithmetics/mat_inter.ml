open Fraction
open Intervalles
open Matrices
open Intertab
open Occu1
open Tools
module type Mat_inter =
    sig
     (* type matrice
      type intertab*)
      type prod
      type var
      val addzero : bool
      val list_var : prod -> var list
      val solve_inf: prod -> var list->unit
      val create : int -> prod
      val plonge : prod -> var list -> prod
      val copy : prod->prod
      val exclusion: prod -> var list -> bool
(*      val do_trans: prod -> var list -> prod ->var list*)
      val all_here : prod -> var list -> prod
      val solve_all : prod->unit
      val compt_of_var_list : var list -> prod
	val affiche_mat : prod -> unit
(*      val n_ligne : prod -> int*)
      val merge:prod->prod->prod
      val is_vide: prod -> var->bool
      val string_of_pro:prod -> var  -> string
      val is_infinite:prod->var->bool
      val union:prod->prod->prod
(*      val maybe:var->unit*)
(*      val affiche_mat:trans->unit*)
      val plus:prod -> prod -> prod

      val widen:prod->prod->(prod*var list)
      val push:prod->var -> Fraction.fraction->prod
      val pushbool:prod -> var -> prod
    end

module Mat_inter =
  functor (M:Matrice with type var=Occu1.trans) ->
    functor (I:Tabinter with type var=Occu1.trans and type intervalle=Intervalles.intervalle) ->
      (struct

      type matrice = M.matrice
      type intertab = I.intervalle_tab
      type prod = {mat:matrice;i:intertab}
      type var = Occu1.trans
      let addzero=true
     let n_ligne p = M.n_ligne p.mat
      let compt_of_var_list l = {mat=M.mat_of_var_list l;
				 i=I.int_of_var_list l}
  let affiche_mat x =
    I.affiche x.i;
    M.affiche x.mat
      let is_vide prod x =
	(I.read (prod.i) x)={inf=Frac({num=0;den=1});sup=Frac({num=0;den=1})}

       let is_infinite m x =
     (I.read (m.i) x).sup=Fraction.Infinity


      let solve_inf prod  l =
	    let m = prod.mat in
	   	    let inter =prod.i in
	    let m=M.copy m  in
	    let _x=m in

	    let li=ref (List.filter (fun x->(is_infinite prod x)) l) in
	    let nli=ref (1) in
	    while (!nli)<>0 do
	      let l=(!li) in
	    let posm=M.copy m  in
	    let rec aux liste =
                match liste with [] -> ()
		|  j::q when (not(is_infinite prod j)) -> aux q
		| j::q ->
				 (
				  let rec aux2 i pos neg lneg refpos =

                                      match ((i>(M.n_ligne m)),
						pos,
						neg,
						((M.read_val posm i j).num))
				      with true,true,true,_  -> (true,refpos,lneg)
					| true,_,_,_  -> (false,refpos,lneg)
				        | _,_,_,a when a<0 -> aux2 (i+1) pos true (i::lneg) refpos
					| _,true,_,_ -> aux2 (i+1) pos neg lneg refpos
					| _,false,_,a when a>0 -> aux2 (i+1) true neg lneg i
					| _ -> aux2 (i+1) pos neg lneg refpos in


				 match  (aux2 1 false false [] (-1))
with
				   (false,_,[]) -> aux q |
		(false,_,_) ->  aux q
    	      | (_,posref,l) ->
					let rec aux3 l =
                                            match l with t::q ->
	let k=(fmoins {num=0;den=1} (fdiv  (M.read_val posm  t j) (M.read_val posm
posref j))) in

		       (M.addligne posm t k  posref;aux3 q)
						| [] -> aux q
					in aux3 l
				)
	 in (aux l);


         let lprob = List.filter
                         (fun j -> (let rec aux q =
			   if (M.read_val posm q j).num<0 then true
                           else if q=1 then false
                                 else aux (q-1)
                          in aux (M.n_ligne posm)))
                         (l)  in


	     List.iter (fun j -> (try
	       (let size = M.n_ligne posm in
                for i1=1 to size do
		 for i2=i1+1 to size do

		   let a=M.read_val posm i1 j in
		   let b=M.read_val posm i2 j in
     if (a={num=0;den=1} || b={num=0;den=1})
		   then ()
		   else
		     ( let f x y = (fmoins x (ffois a (fdiv y b))) in
		     if (List.for_all (fun x->(f (M.read_val posm i1 x)  (M.read_val posm i2 x)).num >= 0) l)
		     then (
			   M.new_empty_ligne posm;
                           let  size = M.n_ligne posm in
                           M.addligne posm size {num=1;den=1} i1;
                           M.addligne posm size (ffois {num=(-1);den=1} (fdiv a b)) i2;

               raise Exit)
	 else
             let g x y = (fmoins x (ffois  b (fdiv y a))) in
       if (List.for_all (fun x->(g (M.read_val posm i2 x)  (M.read_val posm i1 x)).num>=0) l)
          then (
		           M.new_empty_ligne posm;
                           let  size = M.n_ligne posm in
                           M.addligne posm size {num=1;den=1} i2;
                           M.addligne posm size (ffois {num=(-1);den=1} (fdiv a b)) i1;

               raise Exit))
        done;done) with Exit -> ())) lprob;



         let n=Config_aff.hashnumber in
	 let pos=Hashtbl.create n in                     (* variable -> contraintes o� il apparait positivement*)
         let neg=Hashtbl.create n in                     (* variable -> contraintes o� il apparait n�gativement*)
	 let nb_inf=Array.make ((M.n_ligne posm)+1) 0 in     (* contrainte -> nb de monomes  non major�e *)
	 let nb_minf=Array.make ((M.n_ligne posm)+1) 0 in    (* contrainte -> nb de monome non minor�e *)
	 let _inf =Hashtbl.create n in                    (* contrainte -> monomes non majores*)
	 let _minf = Hashtbl.create n in                  (* contrainte -> monomes non minores*)
         let good_line=Working_list_aff.new_working_list n in (*contraintes � r�duire*)
	 let solved=Working_list_aff.new_working_list n in (*variable trouv�e*)
	 let visited_line=Working_list_aff.new_working_list n in (*contraintes r�duites ou en cours*)
	 let read_t t x =
	     try ((Hashtbl.find t x).Working_list_aff.list ())  with _ -> [] in

	 let update t x y =
              let l=(try (Hashtbl.find t x)
	             with _ -> (let l=(Working_list_aff.new_working_list n) in
	                        Hashtbl.add t x l;l)) in
	      l.Working_list_aff.push y in

	 let view (k,v) =                                        (*met en attente une contrainte*)
             if (not (visited_line.Working_list_aff.member k))
	     then
	        (visited_line.Working_list_aff.push k;
		 good_line.Working_list_aff.push (k,v)) in
	 let solve k =                                        (*d�duit les nouvelles contraintes lors de la r�duction d'une variable*)
	     if (not (solved.Working_list_aff.member k)) then
	     (
	     List.iter (fun x->
	       begin
		 nb_inf.(x)<-(nb_inf.(x)-1);
		 if (nb_inf.(x)=0)
		 then
		   (view (x,k))
	       end) (read_t pos k);
	     List.iter (fun x->
	       begin
		 nb_minf.(x)<-(nb_minf.(x)-1);
		 if (nb_minf.(x)=0)
		 then
		   (if (not (visited_line.Working_list_aff.member x))
		    then
		       (view (x,k)))
	       end) (read_t neg k)) in
	 let rec vide () =                                    (*traite les contraintes en attentes*)
	      try
		(let (k,pb)=good_line.Working_list_aff.pop () in
		 begin
		         (if nb_inf.(k)=0
			   then
			     (
			      let rec vide list somme  =
			       match list
			       with
				 (Pro (-1))::q -> vide q (ffplus  (Frac({num=0;den=1}))
						          {num=(-1);den=1} (Frac((M.read_val posm k (Pro(-1))))))

			       |  t::q       -> let delta=(M.read_val posm k t) in
				 (match delta.num
				 with
				   0 -> vide q somme
				 | a when a<0 -> vide q (ffplus somme {num=(-(delta.num));den=delta.den} ((I.read inter (t)).inf))
				 | a when a>0 -> vide q (ffplus somme {num=(-(delta.num));den=delta.den} ((I.read inter (t)).sup))
				 |  _ -> vide q somme)
			      |   []        -> somme in
			     let sup=(vide (match (M.get_line posm k) with (k,c) -> k) (Frac{num=0;den=1})) in
			     let rec vide2 l =
			         match l
				 with
				   (Pro(-1))::q -> vide2 q
				 | t::q -> let delta=(M.read_val posm k t) in
				           (match delta.num with
					     0 -> vide2 q
					   | a when a>0 -> (vide2 q)
					   | a  -> (let s2=(ffplus sup delta (I.read inter t).inf) in
					            (

					             (I.set inter t
(cap_inter (I.read inter t) {inf=(Frac {num=0;den=1});
                         sup=(ffdiv s2 (Frac(delta)))})
							      );
						    (solve t);vide2 q)))
				 |  [] -> () in
			     vide2 (match (M.get_line posm k) with (k,c) -> k)));
			  (if nb_minf.(k)=0
			   then
			     (let rec vide list somme  =
			       match list
			       with
				 (Pro (-1))::q -> vide q (ffplus  (Frac({num=0;den=1}))
						          {num=(-1);den=1} (Frac((M.read_val posm k (Pro (-1))))))

			       |  t::q       -> let delta=(M.read_val posm k t) in
				 (match delta.num
				 with
				   0 -> vide q somme
				 | a when a<0 -> vide q (ffplus somme {num=(-(delta.num));den=delta.den} ((I.read inter t).sup))
				 | a when a>0 -> vide q (ffplus somme {num=(-(delta.num));den=delta.den} ((I.read inter t).inf))
				 |  _ -> vide q somme)
			      |   []        -> somme in
			     let inf=(vide (match (M.get_line posm k) with (k,c) -> k) (Frac{num=0;den=1})) in
			     let rec vide2 l =
			         match l
				 with
				   (Pro(-1))::q -> vide2 q
				 | t::q -> let delta=(M.read_val posm k t) in
				           (match delta.num with
					     0 -> vide2 q
					   | a when a<0 -> (vide2 q)
					   | a  -> (let s2=(ffplus inf delta ((
I.read inter t).inf)) in
					            ((I.set inter t)
			     (cap_inter (I.read inter (t))
							{inf=(Frac {num=0;den=1});
							 sup=(ffdiv s2 (Frac(delta)))}
							      ));
							       	              solve t;vide2 q))
				 |  [] -> () in
			     vide2 (match (M.get_line posm k) with (k,c) -> k)))

			 end;vide ())
	      with _ -> () in



	  for i=1 to (M.n_ligne posm) do
	    let rep=ref (Pro(-1)) in
	         let (k,c)=M.get_line posm i in
                     (List.iter (fun j->if p j (Pro(-1))>0 then
		                           (match
            (((I.read inter j).sup),
             ((M.read_val posm i j).num))
	                                    with
					     Infinity,a when a>0 -> (update pos j i;rep:=j;
					                                 nb_inf.(i)<-1+nb_inf.(i))
					   | Infinity,a when a<0 -> (update neg j i;rep:=j;
					                                 nb_minf.(i)<-1+nb_minf.(i))
					   |            _        -> ())) k;
                      if (nb_inf.(i)=0 || nb_minf.(i)=0) then (view (i,!rep);vide ()))
	  done;
           nli:=List.length (!li) ;
(*           print_string "RED";
           print_int (!nli);*)
 li:= (List.filter (fun x->(is_infinite prod x))) l ;
	    nli:=(!nli) - (List.length (!li))

	 done;
	  (**************************************************************************************)
	 let transcribe_constraint k =
	      (*t_i k;   t_s "\n";*)
              let (k,c)=M.get_line m k in
	   t_s "\n";
	   let rec cop_line (k,c) =
	     match k with (Pro (-1))::q -> cop_line (q,c)
	     |  _ -> let nl=Hashtbl.create Config_aff.hashnumber  in
	     (List.iter (fun x->(Hashtbl.add nl x (try (Hashtbl.find c x) with _ -> {num=0;den=1}))) k;
	     (k,nl,let a=(try (let a=(Hashtbl.find c (Pro (-1))) in
				 (Frac{num=(-(a.num));den=a.den}))

			  with _ -> Frac{num=0;den=1}) in {inf=a;sup=a})) in
	   cop_line (k,c) in
	 let n=Config_aff.hashnumber in
	 let nm=M.new_matrice n in
	 let aff=Hashtbl.create n in

	 let read_aff i =
	   try (Hashtbl.find aff i) with _ -> {inf=Frac{num=0;den=1};
				       sup=Frac{num=0;den=1}} in
	 let change_aff i i2 =
	   ((try (Hashtbl.remove aff i) with _ -> ());
	   Hashtbl.add aff i i2) in
	 let n_copy_line (k,c,b) =
	   M.new_copy_ligne nm (k,c);
	   let n=M.n_ligne nm in
	   change_aff n b in
		 for l=1 to (M.n_ligne m) do

	   let k=(transcribe_constraint l) in

           (n_copy_line k)
	 done;
	 let simplify_pivot ligne =
	   let ((k,c),b)=(M.get_line nm ligne,read_aff ligne) in
	   let rec vide l sol =
	     match l with t::q -> (let delta=(try (Hashtbl.find c t) with _ -> {num=0;den=1}) in
	                           vide q
{inf=(ffplus (sol.inf)
	{num=(-delta.num);den=delta.den}
	(if (delta.num<0)
	then ((I.read inter (t)).inf)
        else ((I.read inter (t)).sup)));
 sup=(ffplus (sol.sup)
	{num=(-delta.num);den=delta.den}
	(if (delta.num<0)
	 then ((I.read inter (t)).sup)
	 else ((I.read inter (t)).inf)))})
	     |  [] -> sol
	  in match k with [] -> ()
	  |  t::q ->
	    let rep=vide q b in
	    let i={inf=(match (rep.inf)
	               with
	      Frac(a) -> Frac(a)
	    |  _ -> Minfinity) ;
		sup=match rep.sup with
		  Frac(a) -> Frac(a)
		|  _ -> Infinity}  in
	    match (try (Hashtbl.find c t) with _ -> {num=0;den=1}) with
	      delta when delta.num>0 -> let new_i=cap_inter (I.read inter t) {inf=ffdiv (i.inf) (Frac delta);sup=ffdiv (i.sup) (Frac delta)} in
	      (I.set inter t new_i)
	    | delta when delta.num<0 -> let new_i=cap_inter (I.read inter t) {inf=ffdiv (i.sup) (Frac delta);sup=ffdiv (i.inf) (Frac delta)} in
	      (I.set inter t new_i)
	    |  _                     -> () in
         let reduit deb fin  =
	   let rec aux k =
	    (if k>fin then  (M.del_last_ligne nm)
             else (let rec search_good_ligne l rep  wei =
		if (l>fin) then (rep,wei)
		else (
                try (let cur=(M.pivot m l) in
                     if ((cur<wei) || (wei=(Pro(-1))))
		     then (search_good_ligne (l+1) l cur)
		     else (search_good_ligne (1+l) rep wei)
                     ) with _ -> search_good_ligne (1+l) rep wei) in
             let new_ligne,wei=search_good_ligne deb (-1) (Pro(-1)) in
             if wei=(Pro (-1)) then aux (fin+1) else
		begin
		let col=M.pivot m new_ligne in
                (
                 M.swap nm k (new_ligne);let tmp=(Hashtbl.find aff (k)) in (Hashtbl.remove aff k;
								  Hashtbl.add aff k (Hashtbl.find aff (new_ligne));
								  Hashtbl.remove aff new_ligne;
								  Hashtbl.add aff new_ligne tmp);
		 M.mulligne nm k (fdiv {num=1;den=1} (M.read_val m k col));(let tmp=(Hashtbl.find aff k) in
		                                                      (Hashtbl.remove aff k;
								       Hashtbl.add aff k (iiplus {inf=Frac {num=0;den=1};
											 sup=Frac{num=0;den=1}}
		     (fdiv {num=1;den=1} (M.read_val nm k col)) tmp)));

                  for i=deb  to fin do
		     if i=k then ()
			    else (let alpha=ffois {num=(-1);den=1}
(M.read_val m i col) in  M.addligne m i alpha  k;let tmp=Hashtbl.find aff i in
                                         (Hashtbl.remove aff i;
					  Hashtbl.add aff i (iiplus tmp alpha (Hashtbl.find aff k))))
		 done;
		 aux (k+1))
		end))  in aux deb in
	 let reduce_pivot ligne =
	   let ((k,c),b)=(M.get_line nm ligne,Hashtbl.find aff ligne) in
	   ((*affiche_cons (k,c,b);*)
	   match k
	   with
	     t::q -> (let cop_line (k,c,b) =
	                  let nl=Hashtbl.create n in
			  (List.iter (fun x->(if (x>(Pro (-1))) then (Hashtbl.add nl x (try (Hashtbl.find c x) with _ -> {num=0;den=1})))) k;
			   (k,nl,b)) in

	              let (k,c,i)=cop_line (k,c,b) in
		      let delta=try (Hashtbl.find c t) with _ -> {num=0;den=1} in
		      (
		      (try (Hashtbl.remove c t) with _ -> ());
		      let i=(iiplus i {num=(-(delta.num));den=delta.den} (I.read inter t)) in
	              (Hashtbl.add aff ((M.n_ligne nm) +1) i;
	             		      M.new_copy_ligne nm (q,c))))
	   |  [] -> ()) in

	 let deb=ref 1 in
	 let fin=ref (M.n_ligne nm) in
	 while (!deb)<((!fin)+1) do
	   for i=(!deb) to (!fin) do
	     simplify_pivot i;

	     reduce_pivot i;

	   done;


	     reduit ((!fin)+1) (M.n_ligne nm);
	     deb:=(!fin+1);
	     fin:=(M.n_ligne nm)(*;*)
(*	     t_i (!deb);
	     t_i (!fin);
	     print_newline ()*)

	done;
        for k=1 to (M.n_ligne nm) do
	  simplify_pivot ((M.n_ligne nm)+1-k)
	done
   let classe p l =
     M.get_all_key (p.mat)

	let create n = {mat= (M.new_matrice  n);
		    i = (I.create n)}

	let f_un = {num=1;den=1}
        let f_zero = {num=0;den=1}
	let un ={inf = Frac f_un;sup=Frac f_un}
	let zero = {inf = Frac f_zero;sup=Frac f_zero}
   let list_var p =
     let rep = Working_list_aff.new_working_list Config_aff.hashnumber in
     (List.iter (rep.Working_list_aff.push) ((M.get_all_key (p.mat)));
      List.iter (rep.Working_list_aff.push) ((I.clef (p.i)));
      List.filter (fun x-> not(x=Occu1.Pro (-1))) (rep.Working_list_aff.list ()))

	let red2 mi = mi
(*  (* try*) (  List.iter (fun (x:var) ->
       match x with Occu1.Trans(a,b,c) as y ->
         if ffinf (Frac{num=0;den=1}) (I.read mi.i y).inf
	     then
	    I.set
	      (mi.i)  ((Occu1.Transb(a,b,c)):var)
	      (cap_inter (I.read mi.i (Occu1.Transb(a,b,c))) {inf=Frac{num=1;den=1};
				       sup=Infinity})
	     else if (I.read mi.i y).sup = Frac{num=0;den=1}
		 then I.set mi.i (Occu1.Transb(a,b,c)) zero

       | Occu1.Transb(a,b,c) as y ->
	   let i = I.read mi.i y in
	   if i=un
	       then  I.set mi.i (Occu1.Trans(a,b,c))
	       (cap_inter (I.read mi.i (Occu1.Trans(a,b,c))) {inf=Frac{num=1;den=1};
				       sup=Infinity})
	     else if i = zero
		 then (I.set mi.i (Occu1.Trans(a,b,c))  zero)
       | Pro _ -> ())
	            [Occu1.Transb(21,12,0);Occu1.Trans(21,12,0);Occu1.Transb(12,19,13);Occu1.Trans(12,19,13)])
	*)



   let solve_inf mi c =
     let changed=ref true in
     let k = ref 0 in
     while (!changed) || (!k)>5 do
      let tmp=I.copy (mi.i) in
       solve_inf mi c;
       let _ = red2 mi in
       (if I.equal tmp (mi.i) then changed:=false
       else ());
       k:=(!k)+1
     done



   let exclusion p l  =
    begin
	let _mat=p.mat in
	let classe=classe p l  in
	let i2=I.copy (p.i) in
	try
	  (List.iter (fun j -> I.set i2 j (cap_inter (I.read i2 j) {inf=Frac{num=1;den=1};
				       sup=Infinity})) l;
	   solve_inf {mat=p.mat;i=i2} classe;false)

					    with _ -> true
     end

   let all_here p l  =
    begin
	let _mat=p.mat in
	let classe=classe p l  in
	  let h = Hashtbl.create Config_aff.hashnumber in
	  let get x = try (let rep = Hashtbl.find h x in (Hashtbl.remove h x;rep)) with _ -> 0 in
	  let inc x = let n=get x in
	              Hashtbl.add h x (n+1) in
	  List.iter (fun x->inc x) l;
	let i2=I.copy (p.i) in
	(List.iter (fun j -> I.set i2 j (cap_inter (I.read i2 j) {inf=Frac{num=(get j);den=1};
				       sup=Infinity})) l;
	   solve_inf {mat=p.mat;i=i2} classe);
	{mat=p.mat;i=i2}


     end
	let double_here p l  =
	  begin
	    let _mat=p.mat in
	    let classe=classe p l  in
	    let h = Hashtbl.create Config_aff.hashnumber in
	    let get x = try (let rep = Hashtbl.find h x in (Hashtbl.remove h x;rep)) with _ -> 0 in
	    let inc x = let n=get x in
	    Hashtbl.add h x (n+1) in
	    List.iter (fun x->inc x) l;
	let i2=I.copy (p.i) in
	(List.iter (fun j -> I.set i2 j (cap_inter (I.read i2 j) {inf=Frac{num=2;den=1};
								   sup=Infinity})) l;
	 solve_inf {mat=p.mat;i=i2} classe);
	{mat=p.mat;i=i2}


     end
   let not_here p l  =
    begin
	let _mat=p.mat in
	let classe=classe p l  in
	  let h = Hashtbl.create Config_aff.hashnumber in
	  let get x = try (let rep = Hashtbl.find h x in (Hashtbl.remove h x;rep)) with _ -> 0 in
	  let inc x = let n=get x in
	              Hashtbl.add h x (n+1) in
	  List.iter (fun x->inc x) l;
	let i2=I.copy (p.i) in
	(List.iter (fun j -> I.set i2 j (cap_inter (I.read i2 j) {inf=Frac{num=0;den=1};sup=Frac{num=0;den=1}})) l;
	   solve_inf {mat=p.mat;i=i2} classe);
	{mat=p.mat;i=i2}
end
(*      let not_here p l =
	match l with
	  [Transb(12,19,13)] ->
	    (print_string "NOT HERE";
	    print_newline ();
	    affiche_mat p;
	    let p = not_here p l in
	    (affiche_mat p;p))
	| _ -> not_here p l *)


   let plus p q =
     {mat=M.somme_affine (p.mat) (q.mat);
      i= I.somme (p.i) (q.i)}
    let union p q =
     {mat=M.union (p.mat) (q.mat);
      i= I.union (p.i) (q.i)}
    let plonge m l =
      {mat=M.plonge (m.mat) l ;i=m.i}

   let widen p q =
     let n=(M.n_ligne (p.mat)) in
     let newm=M.union (p.mat) (q.mat) in
     let i=I.wide_place (p.i) (q.i) in
     if ((n=(M.n_ligne (newm))) && i=[])  then
     ({mat=newm;i=p.i},[])
	 else ({mat=newm;i=p.i},((list_var p)))

   let solve_all m =
       solve_inf m (list_var m)
   let string_of_pro m x =
     Intervalles.string_of_intervalle (I.read (m.i) x)

   let push m x f  =
     let _a=I.push (m.i) x f  in
     (M.push (m.mat) x f ;
     m)

  let copy m =
     {mat=M.copy (m.mat);
       i=I.copy (m.i)}

   let pushbool m x  =
(*     if x = Occu1.Transb(12,19,13) or x = Occu1.Transb(21,12,0) then *)
(     let mc = copy m in
     let m1,b1 = try (let m1 = all_here mc [x]  in  m1,true) with _ -> (copy m),false in
     let mc = copy m in
     let m2,b2 = try (push (not_here mc [x]) x {Fraction.num=1;Fraction.den=1},true) with _ -> (copy m),false in
     if b1 then
       if b2 then
	 union m1 m2
	   else m1
     else if b2 then m2 else (m))
(*     match x with Occu1.Transb(x,y,z)->
       print_string "WARN:BOT";
      print_int x;print_int y;print_int z;
       print_newline ();
       affiche_mat m;m | _ -> m)) else m*)

   let copy m =
     {mat=M.copy (m.mat);
       i=I.copy (m.i)}

   let merge m1 m2 =
     {mat=M.merge (m1.mat) (m2.mat);
      i=I.merge (m1.i) (m2.i)}

  end:Mat_inter with type var=Occu1.trans)


module Mat_int= (Mat_inter(Mat)(Tabinter))
