(**
   * int_storage.ml
   *
   * Creation:                      <2010-07-27 feret>
   * Last modification: Time-stamp: <2016-01-22 15:47:16 feret>
   *
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   *
   * This library provides primitives to deal with storage functions
   *
   * Copyright 2010,2011,2012,2013,2014,2015 Institut National
   * de Recherche en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

module type Storage =
sig
  type 'a t
  type key
  type dimension

  val create: Remanent_parameters_sig.parameters -> Exception.method_handler -> dimension -> Exception.method_handler * 'a t
  val init: Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a t -> dimension -> Exception.method_handler * 'a t
  val set: Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a -> 'a t -> Exception.method_handler * 'a t
  val get: Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a t -> Exception.method_handler * 'a option
  val unsafe_get: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a t -> Exception.method_handler * 'a option
  val dimension: Exception.method_handler -> 'a t -> Exception.method_handler * dimension
  val print: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler
  val print_var_f: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler
  val print_site_f: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler

  val key_list: Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a t -> (Exception.method_handler * key list)
  val iter:Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  ->  Exception.method_handler ) -> 'a t ->  Exception.method_handler
  val fold_with_interruption: Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  -> 'b-> Exception.method_handler  * 'b ) -> 'a t -> 'b ->  Exception.method_handler * 'b
  val fold: Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  -> 'b-> Exception.method_handler  * 'b ) -> 'a t -> 'b ->  Exception.method_handler * 'b
  val fold2_common:Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a -> 'b -> 'c ->  Exception.method_handler  * 'c ) -> 'a t -> 'b t ->  'c ->  Exception.method_handler * 'c

end

let invalid_arg parameters mh message exn value  =
  Exception.warn parameters mh (Some "Int_storage") message exn (fun () -> value )

module Int_storage_imperatif =
  (struct
    type key = int
    type dimension = int
    type 'a t =
        {
          array:('a option) array ;
          size:int ;
        }

    let dimension error a = error, a.size

    let imperatif = true
    let ordered _ = true
    let key_list paremeters error t =
      let size = t.size in
      let array = t.array in
      let rec aux k sol =
        if k<0 then error,sol
        else
          match array.(k) with
            | None -> aux (k-1) sol
            | Some _ -> aux (k-1) (k::sol)
      in aux size []

    let rec create parameters error size  =
      if size < 0
      then
        let error,array = create parameters error 0 in
        invalid_arg parameters error (Some "create, line 60") Exit array
      else
        error,
        {
          array = Array.make (size+1) None;
          size = size;
        }

    let init parameters error array size =
      let error,dimension = dimension error array in
      if dimension < size
      then
        let error,array' = create parameters error size in
        let _ = Array.blit array.array 0 array'.array 0 dimension in
        error,array'
      else
        error,{array = Array.sub array.array 0 size ; size = size}

    let set parameters error key value array =
      if key>array.size || key<0
      then
        invalid_arg parameters error (Some "set, line 81") Exit array
      else
        let _ = array.array.(key)<-Some value in
        error,array

    let get parameters error key array =
      if key>array.size || key<0 then
        invalid_arg parameters error (Some "get, line 88") Exit None
      else
        match array.array.(key) with
          | None -> invalid_arg parameters error (Some "get, line 92") Exit None
          | a -> error,a

    let unsafe_get parameters error key array =
      if key>array.size || key<0 then
        error,None
      else
        error,array.array.(key)

    let print error print_elt parameters array =
      let rec aux i error =
        if i>array.size then error
        else
          let error =
            match array.array.(i) with
              | None -> error
              | Some elt ->
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%s%d:" (Remanent_parameters.get_prefix parameters) i in
		let () =
		  Loggers.print_newline (Remanent_parameters.get_logger parameters)
		in
                let parameters = Remanent_parameters.update_prefix parameters
                  ((string_of_int i)^":") in
                let error = print_elt error parameters elt in
                error
          in aux (i+1) error
      in aux 0 error

    let print_var_f error print_elt parameters array =
      let rec aux i error =
        if i>array.size then error
        else
          let error =
            match array.array.(i) with
              | None -> error
              | Some elt ->
                let _ =
                  Loggers.print_newline (Remanent_parameters.get_logger parameters) in
                let parameters = Remanent_parameters.update_prefix parameters
                  ((string_of_int i)^":") in
                let error = print_elt error parameters elt in
                error
          in aux (i+1) error
      in aux 0 error

    let print_site_f error print_elt parameters array =
      let rec aux i error =
        if i>array.size then error
        else
          let error =
            match array.array.(i) with
              | None -> error
              | Some elt ->
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%sagent_type:%d:" (Remanent_parameters.get_prefix parameters)
                    i in
                let () =
		  Loggers.print_newline (Remanent_parameters.get_logger parameters)
		in
		let parameters =
                  Remanent_parameters.update_prefix parameters
                    ("agent_type:"^(string_of_int i)^":") in
                let error = print_elt error parameters elt in
                error
          in aux (i+1) error
      in aux 0 error

    let equal parameters error p a b =
      if a==b
      then error,true
      else
        begin
          let error,size_a = dimension error a in
          let error,size_b = dimension error b in
          let size_min = min size_a size_b in
          let array_a = a.array in
          let array_b = b.array in
          let rec aux k error =
            if k<0
            then error,true
            else
              match array_a.(k),array_b.(k) with
                | None,None -> aux (k-1) error
                | Some a,Some b ->
                  let error,bool = p parameters error a b in
                  if bool
                  then
                    aux (k-1) error
                  else
                    error,false
                | None, Some _ | Some _,None -> error,false
          in
          let error,b = aux size_min error in
          error,
          b
          &&
            begin
              let rec aux array max k =
                if k>max then true
                else match array.(k) with
                  | None -> aux array max (k+1)
                  | _ -> false
              in
              if size_a = size_b
              then true
              else if size_a < size_b
              then aux array_b size_b (size_a+1)
              else aux array_a size_a (size_b+1)
            end
        end

    let iter parameter error f t =
      let size = t.size in
      let array = t.array in
      let rec aux k error =
        if k>size then error
        else
          match array.(k) with
            | None ->
              aux (k+1) error
            | Some x ->
              aux (k+1) (f parameter error k x)
      in
      aux 0 error

    let fold parameter error f t init =
      let size = t.size in
      let array = t.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array.(k) with
            | None ->
              aux (k+1) remanent
            | Some x ->
              let error,sol  = remanent in
              aux (k+1) (f parameter error k x  sol)
      in
      aux 0  (error,init)

    let fold_with_interruption parameter error f t init =
      let size = t.size in
      let array = t.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array.(k) with
            | None ->
              aux (k+1) remanent
            | Some x ->
              let error,sol  = remanent in
              let output_opt =
		try
		  Some (f parameter error k x sol)
		with
		  ExceptionDefn.UserInterrupted _ -> None
	      in
	      match
		output_opt
	      with
	      | None -> remanent
	      | Some a -> aux (k+1) a
      in
      aux 0  (error,init)

	

    let fold2_common  parameter error f t1 t2 init =
      let size = min t1.size t2.size in
      let array1 = t1.array in
      let array2 = t2.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array1.(k),array2.(k) with
            | None,_ | _,None -> aux (k+1) remanent
            | Some x1,Some x2 ->
              let error,sol = remanent in
              aux (k+1) (f parameter error k x1 x2 sol)
      in
      aux 0  (error,init)

   end:Storage with type key = int and type dimension = int)

module Nearly_infinite_arrays =
  functor (Basic:Storage with type dimension = int and type key = int) ->
    (struct
      type dimension = Basic.dimension
      type key = Basic.key
      type 'a t = 'a Basic.t

      let create = Basic.create
      let dimension = Basic.dimension
      let key_list = Basic.key_list

      let expand parameters error array =
        let error,old_dimension = dimension error array in
        if old_dimension = Sys.max_array_length
        then
          invalid_arg parameters error (Some "expand, line 170") Exit array
        else
          Basic.init parameters error array
            (max 1 (min Sys.max_array_length (2*old_dimension)))

      let get = Basic.get
      let unsafe_get = Basic.unsafe_get
      let init = Basic.init

      let rec set parameters error key value array =
        let error,dimension = dimension error array in
        if key>=dimension
        then
          let error,array' = expand parameters error array in
          if array == array'
          then
            invalid_arg parameters error (Some "set, line 185") Exit array
          else
            set parameters error key value array'
        else
          Basic.set parameters error key value array

      let print = Basic.print
      let print_var_f = Basic.print_var_f
      let print_site_f = Basic.print_site_f
      let iter = Basic.iter
      let fold = Basic.fold
      let fold_with_interruption = Basic.fold_with_interruption
      let fold2_common = Basic.fold2_common

     end:Storage with type key = int and type dimension = int)

module Extend =
  functor (Extension:Storage) ->
    functor (Underlying:Storage) ->
      (struct

        type dimension = Extension.dimension * Underlying.dimension

        type key = Extension.key * Underlying.key

        type 'a t =
            {
              matrix : 'a Underlying.t Extension.t ;
              dimension : dimension;
            }

        let create parameters error dimension =
          let error,matrix = Extension.create parameters error (fst dimension) in
          error,
          {
            matrix = matrix;
            dimension = dimension ;
          }

        let key_list parameters error t =
          let error,ext_list = Extension.key_list parameters error t.matrix in
          List.fold_left
            (fun (error,list) key ->
              let error,t2 = Extension.get parameters error key t.matrix in
              match t2 with
                | None -> invalid_arg parameters error (Some "key_list, line 184") Exit list
                | Some t2 ->
                  let error,l2 = Underlying.key_list parameters error t2 in
                  error,
                  List.fold_left
                    (fun list key2  -> (key,key2)::list)
                    list
                    (List.rev l2))
            (error,[])
            (List.rev ext_list)

        let init parameters error array dimension =
          invalid_arg parameters error (Some "init, line 196") Exit array

        let set parameters error (i,j) value array =
          let error,old_underlying = Extension.unsafe_get parameters error i array.matrix in
          let error,old_underlying =
            match old_underlying with
              | Some old_underlying -> error,old_underlying
              | None -> Underlying.create parameters error (snd array.dimension)
          in
          let error,new_underlying = Underlying.set parameters error j value old_underlying in
          let error,new_matrix = Extension.set parameters error i new_underlying array.matrix in
          (* let ordered = ordered && Extension.ordered new_matrix in*)
          error,{array with matrix = new_matrix}

        let get parameters error (i,j) array =
          let error,underlying = Extension.get parameters error i array.matrix in
          match underlying with
            | Some underlying -> Underlying.get parameters error j underlying
            | None ->  invalid_arg parameters error (Some "get, line 298") Exit None

        let unsafe_get parameters error (i,j) array =
          let error,underlying = Extension.unsafe_get parameters error i array.matrix in
          match underlying with
            | Some underlying -> Underlying.unsafe_get parameters error j underlying
            | _ -> error,None

        let dimension error a = error,a.dimension

        let print error print_of parameters a =
          Extension.print error
            (fun error -> Underlying.print error print_of)
            parameters
            a.matrix

        let print_var_f error print_of parameters a =
          Extension.print error
            (fun error -> Underlying.print error print_of)
            parameters
            a.matrix

        let print_site_f error print_of parameters a =
          Extension.print error
            (fun error -> Underlying.print error print_of)
            parameters
            a.matrix

        let iter parameter error f a =
          Extension.iter
            parameter
            error
            (fun parameter error k a ->
              Underlying.iter
                parameter
                error
                (fun parameter error k' a' -> f parameter error (k,k') a')
                a
            )
            a.matrix

        let fold_gen fold1 fold2  parameter error f a b =
          fold1
            parameter
            error
            (fun parameter error k a b ->
              fold2
                parameter
                error
                (fun parameter error k' a' b -> f parameter error (k,k') a' b)
                a
                b
            )
            a.matrix
            b

	let fold parameter error f a b = fold_gen Extension.fold Underlying.fold parameter error f a b
	let fold_with_interruption parameter error f a b = fold_gen Extension.fold_with_interruption Underlying.fold_with_interruption parameter error f a b

        let fold2_common parameter error f a b c =
          fold
            parameter
            error
            (fun parameter error k a c ->
              let error,get = unsafe_get parameter error k b in
              match get with
                | None -> (error,c)
                | Some b -> f parameter error k a b c)
            a
            c


       end:Storage with type key = Extension.key * Underlying.key and type dimension = Extension.dimension * Underlying.dimension )


module Quick_key_list =
  functor (Basic:Storage) ->
    (struct
      type dimension = Basic.dimension
      type key = Basic.key
      type 'a t =
          {
            basic: 'a Basic.t ;
            keys: key list
          }

      let create parameters error i =
        let error,basic = Basic.create parameters error i in
        error,{basic = basic ; keys = []}

      let key_list parameters error t = error,t.keys

      let init parameters error array j =
        let error,basic = Basic.init parameters error array.basic j in
        error,{basic = basic ; keys = array.keys}

      let set parameters error key value array =
        let error,old = Basic.unsafe_get parameters error key array.basic in
        let new_array =
          match old with
            | Some _ -> array
            | None -> {array with keys = key::array.keys}
        in
        let error,new_basic = Basic.set parameters error key value new_array.basic in
        error, {new_array with basic = new_basic}

      let get parameters error key array =
        Basic.get parameters error key array.basic

      let unsafe_get parameters error key array =
        Basic.unsafe_get parameters error key array.basic

      let dimension error a = Basic.dimension error a.basic

      let print error f parameters a = Basic.print error f parameters a.basic

      let print_var_f error f parameters a =
        Basic.print_var_f error f parameters a.basic

      let print_site_f error f parameters a =
        Basic.print_site_f error f parameters a.basic

      let iter parameters error f a =
        let error,list = key_list parameters error a in
        List.fold_left
          (fun error k ->
            let error,im = get parameters error k a in
            match im with
              | None ->
                let error,_ = invalid_arg parameters error (Some "fold, line 391")
                  Exit () in error
              | Some im -> f parameters error k im)
          error (List.rev list)

      let fold parameters error f a b =
        let error,list = key_list parameters error a in
        List.fold_left
          (fun (error,b) k ->
            let error,im = get parameters error k a in
            match im with
              | None -> invalid_arg parameters error (Some "fold, line 391") Exit b
              | Some im -> f parameters error k im b)
          (error,b)
          (List.rev list)

      let fold_with_interruption parameters error f a b =
	let error,list = key_list parameters error a in
	let rec aux list output =
	  match
	    list
	  with
	  | [] -> output
	  | head::tail ->
	    let output_opt =
	      try
		let error,im = get parameters error head a in
		let b = snd output in
		match im with
		| None ->
                  Some (invalid_arg parameters error (Some "fold, line 391") Exit b)
		| Some im -> Some (f parameters error head im b)
	      with ExceptionDefn.UserInterrupted _ -> None
	    in
	    match
	      output_opt
	    with
	    | None -> output
	    | Some output -> aux tail output
	in aux list (error,b)

      let fold2_common parameter error f a b c =
        fold
          parameter
          error
          (fun parameter error k a c ->
            let error,get = unsafe_get parameter error k b in
            match get with
              | None -> (error,c)
              | Some b -> f parameter error k a b c)
          a
          c

     end:Storage with type key = Basic.key and type dimension = Basic.dimension)

module Nearly_inf_Imperatif = Nearly_infinite_arrays (Int_storage_imperatif)

module Quick_Nearly_inf_Imperatif = Quick_key_list (Nearly_inf_Imperatif)

module Int_Int_storage_Imperatif_Imperatif =
  Extend (Int_storage_imperatif)(Int_storage_imperatif)

module Nearly_Inf_Int_Int_storage_Imperatif_Imperatif =
  Extend (Quick_Nearly_inf_Imperatif)(Quick_Nearly_inf_Imperatif)

module Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif =
  Extend (Quick_Nearly_inf_Imperatif)
    (Extend (Quick_Nearly_inf_Imperatif)(Quick_Nearly_inf_Imperatif))

