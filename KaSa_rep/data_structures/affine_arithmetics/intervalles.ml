open Fraction;;
open Hashtbl;;
open Header;;
open Working_list_aff;;




type intervalle = {inf:ffraction;sup:ffraction} ;;

exception Intervalle_vide ;;

let wide_max = ref {num=2;den=1} ;;

let get_wide_max () = !wide_max ;;
let set_wide_max f  = wide_max:=f ;;


let sub_convexe a b =
    not ((ffinf (a.inf) (b.inf)) || (ffinf  (b.sup) (a.sup))) ;;

let trans_convexe i t =
    let inff = (match i.inf with Frac(a)->ffmax (Frac {num=0;den=1})
                                                  (Frac (fplus a t))
                            | a -> a) in
    let supf = (match i.sup with Frac(a)->(Frac (fplus a t))
                            | a -> a) in
    {inf=ffmax inff (Frac {num=0;den=1});sup=supf};;


let union_convexe  t1 t2 =
   let n=Array.length t1 in
   let ts=Array.make n  {inf=Frac {num=0;den=1} ;sup=Frac{num=0;den=1}} in
   for i=0 to (n-1) do
       ts.(i)<-{inf=ffmin (t1.(i).inf) (t2.(i).inf);sup=ffmax (t1.(i).sup) (t2.(i).sup)}
   done;
   ts;;

let union i1 i2 = {inf=ffmin (i1.inf) (i2.inf);sup=ffmax (i1.sup) (i2.sup)};;

let wide_union_convexe t1 t2 =
    let n=Array.length t1 in
    let ts=Array.make  n   {inf=Frac {num=0;den=1} ;sup=Frac{num=0;den=1}} in
    for i=0 to (n-1) do
       ts.(i)<-{inf=(if (ffinf (t2.(i).inf) (t1.(i).inf)) then (Frac {num=0;den=1})
                                                            else (t1.(i).inf));
               sup=(if (ffinf (t1.(i).sup) (t2.(i).sup)) then
                            (if (ffinf (Frac (!wide_max)) t2.(i).sup) then Infinity
                                                                 else t2.(i).sup)
                    else (t1.(i).sup))}
    done;
    ts;;

let wide_en_place t1 t2 =
  let n=Array.length t1 in
  let l=Working_list_aff.new_working_list hashnumber in
  let changed=(ref false) in
  for i=0 to (n-1) do
    changed:=false;
    let rep={inf=(
	      if (ffinf (t2.(i).inf) (t1.(i).inf))
	      then
		(changed:=true;Frac {num=0;den=1})
              else (t1.(i).inf));
             sup=(
	      if (ffinf (t1.(i).sup) (t2.(i).sup))
	      then
                (if (ffinf (Frac (!wide_max)) t2.(i).sup)
		then
		  (if t1.(i).sup=Infinity
		  then Infinity
		  else (changed:=true;Infinity))

		else (changed:=true;t2.(i).sup))
	      else (t1.(i).sup))} in
    if (!changed) then (l.Working_list_aff.push i;t1.(i)<-rep) else ()
  done;l.list () ;;


let cap_inter i1 i2 =
     let is={inf=ffmax (i1.inf) (i2.inf);sup=ffmin (i1.sup) (i2.sup)} in
        if ffinf (is.sup) (is.inf) then (raise Intervalle_vide)
	    else is ;;

let inter_convexe t1 t2 =
    let n=Array.length t1 in
    let ts=Array.make n  {inf=Frac{num=0;den=1} ;sup=Frac{num=0;den=1}} in
    for i=0 to (n-1) do
        ts.(i)<-{inf=ffmax (t1.(i).inf) (t2.(i).inf);sup=ffmin (t1.(i).sup) (t2.(i).sup)};
        if ffinf (ts.(i).sup) (ts.(i).inf) then (raise Intervalle_vide)
     done;
    ts;;

let iiplus i1 alpha i2 =
  if alpha.num=0 then i1
  else (if (alpha.num<0) then {inf=ffplus i1.inf alpha i2.sup;
			       sup=ffplus i1.sup alpha i2.inf}
      else {inf=ffplus i1.inf alpha i2.inf;
			       sup=ffplus i1.sup alpha i2.sup}) ;;

let combinaison_lineaire_convexe ((l,resultat),s) =
    let resultat={num=(-1)*resultat.num;den=resultat.den} in
    let rec aux l sol =
        match l with ((i,k)::q) when (k.num<0) ->
                (match (ffplus sol k (s.(i).sup) ) with Unknown ->Minfinity
                              | k -> aux q k )
                | ((i,k)::q) when (k.num>0) ->
                (match (ffplus sol k (s.(i).inf)) with Unknown ->Unknown
                              | k -> aux q k )
                | (i,k)::q -> aux q sol
                | [] -> sol

    in
    let inff=(aux l (Frac resultat)) in
    let rec aux l sol =
        match l with ((i,k)::q) when (k.num<0) ->
                 (match (ffplus sol k (s.(i).inf) ) with Unknown ->Infinity
                              | k -> aux q k )
                | ((i,k)::q) (*when (k.num>0)*) ->
                (match (ffplus sol k (s.(i).sup) ) with Unknown ->Unknown
                              | k -> aux q k )
                | [] -> sol
    in
    let supf=(aux l (Frac resultat)) in
    if (ffinf supf inff) then (raise Intervalle_vide)  else {inf=inff;sup=supf};;



let contient_zero i =
    let aux1 () = (match i.inf with Infinity -> false
                                | Frac a when a.num>0 -> false
                                | _ -> true) in
    match i.sup with Minfinity -> false
                             | Frac a when a.num<0 -> false
                             | _ -> aux1 ();;



let string_of_intervalle i =
    match i with
      _ when i.sup=Infinity -> ("[|"^(string_of_int(trunc (i.inf)))^";"^
(Config_aff.st_inf)^"|[")
   |  _ when trunc(i.inf)=trunc(i.sup)    -> string_of_int(trunc(i.inf))
   |  _ -> "[|"^(string_of_int(trunc
(i.inf)))^";"^(string_of_int(trunc(i.sup)))^"|]"
    ;;
