open Intervalles
open Fraction
open Occu1

module type Tabinter = sig
  type var
  type intervalle
  type intervalle_tab

  val make : int -> intervalle_tab
  val set : intervalle_tab -> var -> intervalle -> unit
  val read : intervalle_tab -> var -> intervalle

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    Exception.method_handler

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val clef : intervalle_tab -> var list

  val wide_place :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * var list

  val union_place :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * var list

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val inter :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val somme :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val int_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * intervalle_tab

  val push : intervalle_tab -> var -> Fraction.fraction -> intervalle_tab
  val pushbool : intervalle_tab -> var -> intervalle_tab
  val equal : intervalle_tab -> intervalle_tab -> bool

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    var list ->
    Exception.method_handler * intervalle_tab
end

module Tabinter = struct
  type var = trans
  type intervalle = Intervalles.intervalle

  type intervalle_tab = {
    i: (var, intervalle) Hashtbl.t;
    k: var Working_list_imperative.working_list;
  }

  let make n = { i = Hashtbl.create n; k = Working_list_imperative.make n }

  let set t lambda v =
    Working_list_imperative.push lambda t.k;
    (try Hashtbl.remove t.i lambda with _ -> ());
    Hashtbl.add t.i lambda v

  let read t lambda =
    try Hashtbl.find t.i lambda
    with _ ->
      let v = Intervalles.zero in
      set t lambda v;
      v

  let copy parameter error t =
    let j = make (Remanent_parameters.get_empty_hashtbl_size parameter) in
    let () =
      List.iter (fun x -> set j x (read t x)) (Working_list_imperative.list t.k)
    in
    error, j

  let affiche parameters error t =
    let error =
      List.fold_left
        (fun error x ->
          if not (x = Occu1.Affine_cst) then (
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let () = Occu1.print_trans parameters x in
            let error, inter_string =
              Intervalles.string_of_intervalle parameters error (read t x)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                ": %s" inter_string
            in
            error
          ) else
            error)
        error
        (Working_list_imperative.list t.k)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error

  let clef t = Working_list_imperative.list t.k

  let wide_place parameters error t1 t2 =
    let l =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let spe_push p = Working_list_imperative.push p l in
    let changed = ref false in
    let traite p =
      let () = changed := false in
      let rep =
        {
          inf =
            (if ffinf (read t2 p).inf (read t1 p).inf then
               if ffinf (read t1 p).inf (Fraction.ffneg (Frac !wide_max)) then
                 if (read t1 p).inf = Minfinity then
                   Minfinity
                 else (
                   changed := true;
                   Minfinity
                 )
               else (
                 changed := true;
                 (read t2 p).inf
               )
             else
               (read t1 p).inf);
          sup =
            (if ffinf (read t1 p).sup (read t2 p).sup then
               if ffinf (Frac !wide_max) (read t1 p).sup then
                 if (read t1 p).sup = Infinity then
                   Infinity
                 else (
                   changed := true;
                   Infinity
                 )
               else (
                 changed := true;
                 (read t2 p).sup
               )
             else
               (read t1 p).sup);
        }
      in
      if !changed then (
        spe_push p;
        set t1 p rep
      ) else
        ()
    in
    List.iter traite (clef t2);
    List.iter traite (clef t1);
    error, Working_list_imperative.list l

  let union_place parameters error t1 t2 =
    let l =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let spe_push p = Working_list_imperative.push p l in
    let changed = ref false in
    let traite p =
      let () = changed := false in
      let rep =
        {
          inf =
            (if ffinf (read t2 p).inf (read t1 p).inf then (
               changed := true;
               (read t2 p).inf
             ) else
               (read t1 p).inf);
          sup =
            (if ffinf (read t1 p).sup (read t2 p).sup then (
               changed := true;
               (read t2 p).sup
             ) else
               (read t1 p).sup);
        }
      in
      if !changed then (
        spe_push p;
        set t1 p rep
      ) else
        ()
    in
    List.iter traite (clef t2);
    List.iter traite (clef t1);
    error, Working_list_imperative.list l

  let somme parameters error t1 t2 =
    let l =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let spe_push p = Working_list_imperative.push p l in
    let () =
      List.iter spe_push (clef t1);
      List.iter spe_push (clef t2)
    in
    let rep = make (Remanent_parameters.get_empty_hashtbl_size parameters) in
    let () =
      List.iter
        (fun x ->
          set rep x (iiplus (read t1 x) { num = 1; den = 1 } (read t2 x)))
        (Working_list_imperative.list l)
    in
    error, rep

  let inter parameters error t1 t2 =
    let l =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let spe_push p = Working_list_imperative.push p l in
    let () =
      List.iter spe_push (clef t1);
      List.iter spe_push (clef t2)
    in
    let rep = make (Remanent_parameters.get_empty_hashtbl_size parameters) in
    let () =
      List.iter
        (fun x -> set rep x (cap_inter (read t1 x) (read t2 x)))
        (Working_list_imperative.list l)
    in
    error, rep

  let union parameters error t1 t2 =
    let l =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let spe_push p = Working_list_imperative.push p l in
    let () = List.iter spe_push (clef t1) in
    let () = List.iter spe_push (clef t2) in
    let rep = make (Remanent_parameters.get_empty_hashtbl_size parameters) in
    let () =
      List.iter
        (fun x -> set rep x (union (read t1 x) (read t2 x)))
        (Working_list_imperative.list l)
    in
    error, rep

  let merge _parameters error t1 t2 =
    List.iter (fun x -> set t1 x (read t2 x)) (clef t2);
    error, t1

  let int_of_var_list parameters error l =
    let i = make (Remanent_parameters.get_empty_hashtbl_size parameters) in
    List.iter
      (fun x ->
        set i x
          { inf = Frac { num = 1; den = 1 }; sup = Frac { num = 1; den = 1 } })
      l;
    error, i

  let push i x k =
    set i x
      (iiplus (read i x) k
         { inf = Frac { num = 1; den = 1 }; sup = Frac { num = 1; den = 1 } });
    i

  let pushbool i x =
    set i x { inf = Frac { num = 1; den = 1 }; sup = Frac { num = 1; den = 1 } };
    i

  let equal i1 i2 =
    List.for_all (fun x -> read i1 x = read i2 x) (clef i1 @ clef i2)

  let abstract_away _parameters (error, mat) key =
    let () = set mat key { inf = Minfinity; sup = Infinity } in
    error, mat

  let abstract_away parameters error mat key =
    List.fold_left (abstract_away parameters) (error, mat) key
end
