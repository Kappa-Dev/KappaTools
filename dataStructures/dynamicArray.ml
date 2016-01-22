(**
  * dynamicarray.ml 
  *
  * Array with dynamic size: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 01/03/2012
  * Last modification: 01/03/2012
  * * 
  *  
  * It uses imperative styles to ensure compatibility with other modules   
  *
  * Copyright 2011,2012  Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module DynArray =
  (functor (G:GenArray.GenArray) ->
  (struct
      type 'a t =
        {
           array: 'a G.t ref;
           current_size: int ref;
           default: 'a
          }
            
      let create n a =
	{
          array=ref(G.create n a);
          current_size = ref n;
          default=a
        }

      let length a = !(a.current_size)

      let expand t = 
        let n = length t in 
        let n' = max (n+1) (n*2) in 
        let array' = G.create n' t.default in 
        let _ = G.blit !(t.array) 0 array' 0 n in 
        let _ = t.array:=array' in 
        let _ = t.current_size := n' in 
        () 

      let get a i = 
        let n = length a in 
        if n>i 
        then 
          G.get !(a.array) i 
        else 
          a.default

      let rec set a i v = 
        let n = length a in 
        if n>i 
        then 
          G.set !(a.array) i v 
        else
          let _ = expand a in 
          set a i v 
		
      let make = create
		
      let init n f =
	{
          array=ref (G.init n f) ;
          current_size = ref n ; 
          default = f 0 
        }

      let make_matrix m n a = init m (fun _ -> create n a)
		
      let append a b =
	let lb = length b in
	let la = length a in
	let c = la + lb in
	init
	  c
	  (fun x -> if x < la then get a x else get b (x - la))
		
      let concat l =
	let l =
	  List.filter
	    (fun x -> length x > 0)
	    l in
	match l
	with [] -> raise (Invalid_argument "DynamicArray.concat")
	  |	t:: _ ->
	    let elt = get t 0 in
	    let c =
	      List.fold_left
		(fun sol a -> sol + length a)
		0 l in
	    let m = create c elt in
	    let rec aux k l =
	      match l with [] -> ()
		| t:: q ->
		  let s = length t in
		  let rec aux2 offset k =
		    if offset = s
		    then aux k q
		    else
		      (set
			 m
			 k
			 (get
			    t
			    offset);
		       aux2 (offset + 1) (k + 1))
		  in
		  aux2 0 k in
	    let _ = aux 0 l in
	    m
	      
      let sub a start len =
	let size = length a in
	if start < 0 || len < 0 || start + len > size
	then raise (Invalid_argument "Dynamic_array.sub")
	else
	  let c = init len (fun x -> get a (x + start)) in
				c
		
      let copy a =
        { a with 
          array = ref (G.copy !(a.array)) ;
          current_size = ref (!(a.current_size)) ; 
        }

		
      let fill a start len x =
	let size = length a in
	if start < 0 || len < 0 || start + len > size
	then raise (Invalid_argument "Dynamic_array.fill")
	else
	  let rec aux k i =
	    if k = len then ()
	    else
	      (set a i x; aux (k + 1) (i + 1))
	  in
	  aux 0 start
	    
      let of_list l =
        match l 
        with 
          | [] -> raise (Invalid_argument "DynamicArray.of_list")
          | t::_ -> 
	    {
              current_size = ref (List.length l);
              array = ref (G.of_list l);
              default = t
            }
      let iter f a = G.iter f !(a.array)
      let iteri f a = G.iteri f !(a.array)
      let fold_lefti f b a = G.fold_lefti f b !(a.array)
      let fold_righti f a b = G.fold_righti f !(a.array) b

      let blit a1 ofs1 a2 ofs2 len =
	if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
          || ofs2 < 0 || ofs2 > length a2 - len
  	then invalid_arg "DynamicArray.blit"
  	else 
	  if ofs1 < ofs2 then
			    (* Top-down copy *)
	    for i = len - 1 downto 0 do
	      G.set !(a2.array) (ofs2 + i) (G.get !(a1.array) (ofs1 + i))
	    done
	  else
			    (* Bottom-up copy *)
	    for i = 0 to len - 1 do
	      G.set !(a2.array) (ofs2 + i) (G.get !(a1.array) (ofs1 + i))
	    done

      let print ?trailing pr_s pr_a f a =
	G.print ?trailing pr_s pr_a f !(a.array)

	end:GenArray.GenArray))
