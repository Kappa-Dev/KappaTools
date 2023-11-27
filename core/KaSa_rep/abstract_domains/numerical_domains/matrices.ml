open Tools_aff
open Hashtbl
open Fraction
open Intervalles
open Occu1

module type Matrice = sig
  type point
  type line = Occu1.trans list * (Occu1.trans, Fraction.fraction) Hashtbl.t
  type matrice
  type var

  val mat_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * matrice

  val make : Remanent_parameters_sig.parameters -> int -> matrice

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val affiche_cons :
    Remanent_parameters_sig.parameters ->
    int list * (int, Fraction.fraction) Hashtbl.t * Intervalles.intervalle ->
    unit

  val get_all_entry : matrice -> var Working_list_imperative.working_list
  val add_entry : matrice -> var -> unit

  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var list ->
    Exception.method_handler * matrice

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * matrice

  val normalise :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var ->
    Fraction.fraction ->
    Exception.method_handler

  val pushbool :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var ->
    Exception.method_handler * matrice

  val new_copy_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    line ->
    Exception.method_handler

  val new_empty_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val n_ligne : matrice -> int

  val get_line :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    Exception.method_handler * line

  val get_trans_list : line -> Occu1.trans list

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val pivot : matrice -> int -> var
  val read_val : matrice -> int -> var -> Fraction.fraction
  val addligne : matrice -> int -> Fraction.fraction -> int -> unit

  val swap :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    int ->
    Exception.method_handler

  val mulligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    Fraction.fraction ->
    Exception.method_handler

  val del_ligne : matrice -> int -> unit
  val del_last_ligne : matrice -> unit

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val get_all_key : matrice -> var list
  val is_key : matrice -> var -> bool

  val decomp_affine :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * (point * matrice)

  val somme_affine :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val insert_0 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * matrice

  val equal :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * bool

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var list ->
    Exception.method_handler * matrice
end

module Matrice = struct
  let trace = false

  let _t_i parameters i =
    if trace || Remanent_parameters.get_trace parameters then (
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%i" i
      in
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    )

  let _t_s parameters _error x =
    if trace || Remanent_parameters.get_trace parameters then (
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" x
      in
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    )

  let print_frac parameters f =
    let () =
      if f.den = 1 then
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%i" f.num
      else
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "(%i/%i)" f.num f.den
    in
    ()

  let affiche_frac parameters f =
    if trace || Remanent_parameters.get_trace parameters then
      print_frac parameters f

  let affiche_int parameters i =
    if trace || Remanent_parameters.get_trace parameters then (
      let () =
        match i.inf with
        | Frac a ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s"
            (Remanent_parameters.get_open_int_interval_inclusive_symbol
               parameters);
          affiche_frac parameters a
        | Minfinity ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s%s"
            (Remanent_parameters.get_open_int_interval_infinity_symbol
               parameters)
            (Remanent_parameters.get_minus_infinity_symbol parameters)
        | Unknown | Infinity ->
          Loggers.fprintf (Remanent_parameters.get_logger parameters) "!U!"
      in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s"
          (Remanent_parameters.get_int_interval_separator_symbol parameters)
      in
      let () =
        match i.sup with
        | Frac a ->
          affiche_frac parameters a;
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s"
            (Remanent_parameters.get_close_int_interval_inclusive_symbol
               parameters)
        | Infinity ->
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s%s"
            (Remanent_parameters.get_plus_infinity_symbol parameters)
            (Remanent_parameters.get_close_int_interval_infinity_symbol
               parameters)
        | Unknown | Minfinity ->
          Loggers.fprintf (Remanent_parameters.get_logger parameters) "!U!"
      in
      ()
    )

  let _affiche_inter = affiche_int

  let affiche_cons parameters (k, c, b) =
    let () =
      List.iter
        (fun x ->
          let a = try find c x with _ -> { num = 0; den = 1 } in
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters) "%i" x
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          let () = affiche_frac parameters a in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          ())
        k
    in
    affiche_int parameters b

  type point = {
    coord: unit -> (trans * fraction) list;
    (*proj:trans -> fraction;*)
    set: trans -> fraction -> unit;
    (*affiche_point:unit -> unit;*)
    somme_point: point -> unit;
  }

  type line = trans list * (trans, Fraction.fraction) Hashtbl.t

  let get_trans_list line = fst line

  let new_point parameters n =
    let entry =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let content = Hashtbl.create n in
    let read k = try find content k with _ -> { num = 0; den = 1 } in
    let set k f =
      (try remove content k with _ -> ());
      Hashtbl.add content k f;
      Working_list_imperative.push k entry
    in

    let somme_point ob =
      List.iter (fun (x, y) -> set x (fplus y (read x))) (ob.coord ())
    in
    {
      (*proj=read;*)
      set;
      coord =
        (fun () ->
          List.map
            (fun x -> x, read x)
            (List.sort p (Working_list_imperative.list entry)));
      (* affiche_point =
         (fun () ->
            List.iter
              (fun x->
                 affiche_frac parameters (read x))
              (List.sort p (Working_list_imperative.list entry)));*)
      somme_point;
    }

  type var = Occu1.trans

  type matrice = {
    entry: (int, var list) Hashtbl.t;
    content: (int, (var, fraction) Hashtbl.t) Hashtbl.t;
    all_entry: var Working_list_imperative.working_list;
    nligne: int ref;
    sorted_entry: var list ref;
  }

  let make parameters n =
    {
      entry = Hashtbl.create n;
      content = Hashtbl.create n;
      nligne = ref 0;
      all_entry =
        Working_list_imperative.make
          (Remanent_parameters.get_empty_hashtbl_size parameters);
      sorted_entry = ref [];
    }

  let get_all_entry m = m.all_entry
  let is_key m i = Working_list_imperative.member i m.all_entry
  let get_all_key m = !(m.sorted_entry)
  let n_ligne m = !(m.nligne)

  let add_entry m j =
    if is_key m j then
      ()
    else (
      Working_list_imperative.push j m.all_entry;
      m.sorted_entry := insert_sort po !(m.sorted_entry) j
    )

  let copy_var m n =
    List.iter (add_entry n) (Working_list_imperative.list m.all_entry)

  let new_empty_ligne parameters error m =
    let () =
      m.nligne := !(m.nligne) + 1;
      Hashtbl.add m.entry !(m.nligne) [];
      Hashtbl.add m.content !(m.nligne)
        (Hashtbl.create (Remanent_parameters.get_empty_hashtbl_size parameters))
    in
    error

  let new_copy_ligne parameters error m (ent, cont) =
    let () = m.nligne := !(m.nligne) + 1 in
    let () = Hashtbl.add m.entry !(m.nligne) ent in
    let tmp =
      Hashtbl.create (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let () =
      List.iter
        (fun x ->
          try
            Hashtbl.add tmp x (find cont x);
            add_entry m x
          with Not_found -> ())
        ent
    in
    let () = Hashtbl.add m.content !(m.nligne) tmp in
    error

  let read_val m i j =
    try find (find m.content i) j with _ -> { num = 0; den = 1 }

  let affiche parameters error m =
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "Variables:"
    in
    let () =
      List.iter
        (fun i ->
          let () = print_trans parameters i in
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters) ", "
          in
          ())
        (get_all_key m)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    for i = 1 to !(m.nligne) do
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "---"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      let _ =
        List.fold_left
          (fun b x ->
            let () =
              if b then
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  " + "
            in
            let () =
              let f = read_val m i x in
              if f.num = 1 && f.den = 1 && not (Occu1.Affine_cst = x) then
                ()
              else (
                let () = print_frac parameters f in
                if not (Occu1.Affine_cst = x) then
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "."
              )
            in
            let () = print_trans parameters x in
            true)
          false (find m.entry i)
      in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) " = 0"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ()
    done;
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error

  let new_eps_matrice parameters error l n =
    let m = make parameters n in
    let error =
      List.fold_left
        (fun error x ->
          if not (Affine_cst = x) then
            new_copy_ligne parameters error m
              ( [ x ],
                let h = Hashtbl.create n in
                Hashtbl.add h x { num = 1; den = 1 };
                h )
          else
            error)
        error
        (List.sort (fun x y -> p y x) l)
    in
    error, m

  let safe_assign m i (j : var) k =
    let l = find m.entry i in
    remove m.entry i;
    (try remove (find m.content i) j with _ -> ());
    if k.num = 0 then
      Hashtbl.add m.entry i (sub_list po l j)
    else (
      add_entry m j;
      Hashtbl.add (find m.content i) j k;
      Hashtbl.add m.entry i (insert_sort po l j)
    )

  let del_ligne m i =
    if i < !(m.nligne) + 1 then (
      Hashtbl.remove m.entry i;
      Hashtbl.remove m.content i;
      for j = i + 1 to !(m.nligne) do
        Hashtbl.add m.entry (j - 1) (Hashtbl.find m.entry j);
        Hashtbl.add m.content (j - 1) (Hashtbl.find m.content j);
        Hashtbl.remove m.entry j;
        Hashtbl.remove m.content j
      done;
      m.nligne := !(m.nligne) - 1
    )

  let rec set_val parameters error m i j k =
    if i > !(m.nligne) then (
      let error = new_empty_ligne parameters error m in
      set_val parameters error m i j k
    ) else (
      let () = safe_assign m i j k in
      error
    )

  let rec swap parameters error m i j =
    if i = j then
      error
    else if i > !(m.nligne) || j > !(m.nligne) then (
      let error = new_empty_ligne parameters error m in
      let error = swap parameters error m i j in
      error
    ) else (
      let entryi = find m.entry i in
      let contenti = find m.content i in
      let () = remove m.entry i in
      let () = remove m.content i in
      let () = Hashtbl.add m.entry i (find m.entry j) in
      let () = Hashtbl.add m.content i (find m.content j) in
      let () = remove m.entry j in
      let () = remove m.content j in
      let () = Hashtbl.add m.entry j entryi in
      let () = Hashtbl.add m.content j contenti in
      error
    )

  let mulligne parameters error m i alpha =
    if i > !(m.nligne) then
      error
    else
      List.fold_left
        (fun error j ->
          let k = read_val m i j in
          set_val parameters error m i j (ffois alpha k))
        error (find m.entry i)

  let addligne m i alpha j =
    if alpha.num = 0 then
      ()
    else (
      let li = find m.entry i in
      let lrep = merge po li (find m.entry j) in
      remove m.entry i;
      if j > !(m.nligne) then
        ()
      else (
        List.iter
          (fun x ->
            let k = fplus (ffois alpha (read_val m j x)) (read_val m i x) in
            (try remove (find m.content i) x with _ -> ());
            add (find m.content i) x k)
          lrep;
        Hashtbl.add m.entry i (vide (fun x -> (read_val m i x).num = 0) lrep)
      )
    )

  let rec del_last_ligne m =
    if !(m.nligne) > 0 then (
      match find m.entry !(m.nligne) with
      | [] ->
        remove m.entry !(m.nligne);
        remove m.content !(m.nligne);
        m.nligne := !(m.nligne) - 1;
        del_last_ligne m
      | _ -> ()
    ) else
      ()

  let pivot m i =
    if i > !(m.nligne) then
      failwith "compteur_pivot_1"
    else (
      match find m.entry i with
      | Affine_cst :: k :: _ -> k
      | [ Affine_cst ] -> failwith "compteur_pivot_2"
      | ((Bool _ | Counter _ | Site _) as k) :: _ -> k
      | [] -> failwith "compteur_pivot_3"
    )

  let normalise parameters (error : Exception.method_handler) m =
    let rec aux (error : Exception.method_handler) k =
      if k > !(m.nligne) then (
        del_last_ligne m;
        error
      ) else (
        let rec search_good_ligne l rep wei =
          if l > !(m.nligne) then
            rep, wei
          else (
            try
              let cur = pivot m l in
              if po cur wei || wei = Affine_cst then
                search_good_ligne (l + 1) l cur
              else
                search_good_ligne (1 + l) rep wei
            with _ -> search_good_ligne (1 + l) rep wei
          )
        in
        let new_ligne, wei = search_good_ligne k (-1) Affine_cst in
        let error =
          if wei = Affine_cst then
            aux error (!(m.nligne) + 1)
          else (
            let col = pivot m new_ligne in
            let error = swap parameters error m k new_ligne in
            let error =
              mulligne parameters error m k
                (fdiv { num = 1; den = 1 } (read_val m k col))
            in
            for i = 1 to !(m.nligne) do
              if i = k then
                ()
              else (
                let alpha = ffois { num = -1; den = 1 } (read_val m i col) in
                addligne m i alpha k
              )
            done;
            aux error (k + 1)
          )
        in
        error
      )
    in
    aux error 1

  let rec push parameters (error : Exception.method_handler) m j k =
    if Working_list_imperative.member j (get_all_entry m) then (
      let rec aux error i =
        if i > !(m.nligne) then
          (error : Exception.method_handler)
        else (
          let rep = read_val m i Affine_cst in
          let r = read_val m i j in
          let error =
            set_val parameters error m i Affine_cst
              (fplus rep
                 (let f = ffois r k in
                  { num = -f.num; den = f.den }))
          in
          aux error (i + 1)
        )
      in
      aux error 1
    ) else (
      (*print_trans j;*)
      let error =
        new_copy_ligne parameters error m
          ( [ j ],
            let h =
              Hashtbl.create
                (Remanent_parameters.get_empty_hashtbl_size parameters)
            in
            Hashtbl.add h j { Fraction.num = 1; Fraction.den = 1 };
            h )
      in
      let error = normalise parameters error m in
      let error = push parameters error m j k in
      error
    )

  let get_line parameters error m k =
    try error, (find m.entry k, find m.content k)
    with _ ->
      ( error,
        ( [],
          Hashtbl.create (Remanent_parameters.get_empty_hashtbl_size parameters)
        ) )

  let merge parameters error m m2 =
    let new_m =
      make parameters (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let () =
      List.iter (add_entry new_m)
        (Working_list_imperative.list (get_all_entry m))
    in
    let () =
      List.iter (add_entry new_m)
        (Working_list_imperative.list (get_all_entry m2))
    in
    let n2 = n_ligne m2 in
    let n1 = n_ligne m in
    let avant k1 k2 =
      match
        ( (try pivot m k1 with _ -> Affine_cst),
          try pivot m2 k2 with _ -> Affine_cst )
      with
      | Affine_cst, Affine_cst -> true
      | Affine_cst, _ -> false
      | _, Affine_cst -> true
      | ( ((Bool _ | Counter _ | Site _) as a),
          ((Bool _ | Counter _ | Site _) as b) ) ->
        po a b
    in
    let rec aux k1 k2 error =
      match k2 > n2, k1 > n1, avant k1 k2 with
      | true, true, _ -> error, new_m
      | _, _, true ->
        let error, (la, lb) = get_line parameters error m k1 in
        let error = new_copy_ligne parameters error new_m (la, lb) in
        aux (k1 + 1) k2 error
      | _ ->
        let error, (la, lb) = get_line parameters error m2 k2 in
        let error = new_copy_ligne parameters error new_m (la, lb) in
        aux k1 (k2 + 1) error
    in
    aux 1 1 error

  let _safe_merge parameters error m m2 =
    let error, rep = merge parameters error m m2 in
    let error = normalise parameters error rep in
    error, rep

  let copy parameters error m =
    let cop_line (k, c) =
      let nl =
        Hashtbl.create (Remanent_parameters.get_empty_hashtbl_size parameters)
      in
      List.iter (fun x -> Hashtbl.add nl x (Hashtbl.find c x)) k;
      k, nl
    in
    let nm =
      make parameters (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    List.iter
      (fun x -> add_entry nm x)
      (Working_list_imperative.list m.all_entry);
    let rec aux error i =
      if i > !(m.nligne) then
        error
      else (
        let error, (la, lb) = get_line parameters error m i in
        let error =
          new_copy_ligne parameters error nm ((cop_line : 'b -> 'b) (la, lb))
        in
        aux error (i + 1)
      )
    in
    let error = aux error 1 in
    error, nm

  let decomp_affine parameters error m =
    let n = Remanent_parameters.get_empty_hashtbl_size parameters in
    let nm = make parameters n in
    let () = copy_var m nm in
    let o = new_point parameters n in
    let cop_line (k, c) =
      let nl = Hashtbl.create n in
      List.iter (fun x -> Hashtbl.add nl x (Hashtbl.find c x)) k;
      k, nl
    in
    let forget_affine l =
      match l with
      | Affine_cst :: q -> q
      | [] | (Bool _ | Counter _ | Site _) :: _ -> l
    in
    let rec aux i error =
      if i > !(m.nligne) then
        error
      else (
        let error, (k, c) = get_line parameters error m i in
        let error =
          new_copy_ligne parameters error nm (cop_line (forget_affine k, c))
        in
        let () =
          o.set (pivot m i)
            (let f = read_val m i Affine_cst in
             { num = -f.num; den = f.den })
        in
        aux (i + 1) error
      )
    in
    let error = aux 1 error in
    error, (o, nm)

  let mat_of_var_list parameters error l =
    let error, m =
      new_eps_matrice parameters error l
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let error =
      List.fold_left
        (fun error x -> push parameters error m x { num = 1; den = 1 })
        error l
    in
    error, m

  let plonge parameters error m l =
    let error, nm =
      new_eps_matrice parameters error
        (filtre (fun p -> not (Working_list_imperative.member p m.all_entry)) l)
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    merge parameters error m nm

  let unify_domain parameters error m m2 =
    let key1 =
      List.filter (fun x -> not (List.mem x (get_all_key m))) (get_all_key m2)
    in
    let key2 =
      List.filter (fun x -> not (List.mem x (get_all_key m2))) (get_all_key m)
    in
    let error, m1 = plonge parameters error m key1 in
    let error, m2 = plonge parameters error m2 key2 in
    error, m1, m2

  let union parameters error ma nb =
    let error, ma, mb = unify_domain parameters error ma nb in
    let _m = max (n_ligne ma) (n_ligne mb) in
    let traite (s : trans) (r : int) =
      match read_val ma r s, read_val mb r s with
      | { num = 1; den = 1 }, { num = 1; den = 1 } -> r + 1
      | { num = 1; den = 1 }, _ ->
        for i = 1 to r - 1 do
          let a = read_val mb i s in
          addligne ma i a r
        done;
        del_ligne ma r;
        r
      | _, { num = 1; den = 1 } ->
        for i = 1 to r - 1 do
          let a = read_val ma i s in
          addligne mb i a r
        done;
        del_ligne mb r;
        r
      | _ ->
        let (t : int) =
          let rec find t =
            if t < 0 then
              0
            else if read_val ma t s = read_val mb t s then
              find (t - 1)
            else
              t
          in
          find (r - 1)
        in
        if t > 0 then (
          for i = 1 to t - 1 do
            let a =
              fdiv
                (fmoins (read_val ma i s) (read_val mb i s))
                (fmoins (read_val mb t s) (read_val ma t s))
            in
            addligne ma i a t;
            addligne mb i a t
          done;
          del_ligne ma t;
          del_ligne mb t;
          r - 1
        ) else
          r
    in
    let rec algo r sliste =
      match sliste with
      | Affine_cst :: squeue -> algo r squeue
      | ((Bool _ | Counter _ | Site _) as s) :: squeue ->
        let rprim = traite s r in
        algo rprim squeue
      | [] -> ignore (traite Affine_cst r)
    in
    ( error,
      (algo 1
         (let l =
            fusion (fun x y -> po x y) (get_all_key ma) (get_all_key mb)
          in
          l);
       ma) )

  let recomp_affine parameters error ((o : point), m) =
    let error, nm = copy parameters error m in
    let error =
      List.fold_left
        (fun error (x, y) -> push parameters error nm x y)
        error (o.coord ())
    in
    error, nm

  let somme_affine parameters error ma mb =
    let error, ma = copy parameters error ma in
    let error, mb = copy parameters error mb in
    let error, ma, mb = unify_domain parameters error ma mb in
    let error, (oa, ma) = decomp_affine parameters error ma in
    let error, (ob, mb) = decomp_affine parameters error mb in
    let () = oa.somme_point ob in
    let error, mc = union parameters error ma mb in
    let error, rep = recomp_affine parameters error (oa, mc) in
    error, rep

  let insert_0 parameters error m =
    let l = Working_list_imperative.list (get_all_entry m) in
    let error, nm =
      new_eps_matrice parameters error l
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    union parameters error nm m

  let pushbool parameters (error : Exception.method_handler) m a =
    let error, m2 = copy parameters error m in
    let error =
      new_copy_ligne parameters error m2
        ( [ a ],
          let h = create 1 in
          add h a { Fraction.num = 0; Fraction.den = 1 };
          h )
    in
    let error =
      push parameters error m2 a { Fraction.num = 1; Fraction.den = 1 }
    in
    let error = normalise parameters error m2 in
    let error, m3 = copy parameters error m in
    let error =
      new_copy_ligne parameters error m3
        ( [ a ],
          let h = create 1 in
          add h a { Fraction.num = 1; Fraction.den = 1 };
          h )
    in
    let error = normalise parameters error m3 in
    let error, m = union parameters error m2 m3 in
    let error = normalise parameters error m in
    error, m

  let equal parameters error m1 m2 =
    let error, m1 = copy parameters error m1 in
    let error, m2 = copy parameters error m2 in
    let error, union = union parameters error m1 m2 in
    error, m2.nligne = union.nligne && m1.nligne = union.nligne

  let abstract_away parameters (error, matrice) var =
    let error, matrice_copy = copy parameters error matrice in
    let error =
      push parameters error matrice_copy var
        { Fraction.num = 1; Fraction.den = 1 }
    in
    let error, m = union parameters error matrice matrice_copy in
    let error = normalise parameters error m in
    error, m

  let abstract_away parameters error matrice list =
    List.fold_left (abstract_away parameters) (error, matrice) list
end
