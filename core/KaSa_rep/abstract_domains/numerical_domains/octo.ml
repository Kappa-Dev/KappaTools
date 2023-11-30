open Fraction
open Integer
open Mat_inter

exception Exit

type unbounded = Bound of int | Infinity

module Octo : Mat_inter with type var = Occu1.trans = struct
  type var = Occu1.trans
  type var_symb = Plus of var | Moins of var (*| ZERO *)

  type prod = {
    key: (var_symb * var_symb) Working_list_imperative.working_list;
    map: (var_symb * var_symb, Integer.unbounded) Hashtbl.t;
    var: var Working_list_imperative.working_list;
  }

  let addzero = false

  let _p i j =
    match i, j with
    (* ZERO,_ -> true
       | _,ZERO -> false*)
    | Plus _, Moins _ -> true
    | Plus x, Plus y -> x < y
    | Moins _, Plus _ -> false
    | Moins x, Moins y -> x < y

  let rec add_var m (x : var_symb) =
    match x with
    | Plus t | Moins t ->
      if Working_list_imperative.member t m.var then
        ()
      else (
        Working_list_imperative.push t m.var;
        let l = Working_list_imperative.list m.var in
        List.iter
          (fun z ->
            set_cons m (Plus z) (Plus t) (div2 (get_cons m (Plus z) (Moins z)));
            set_cons m (Moins z) (Plus t) (div2 (get_cons m (Moins z) (Plus z)));
            set_cons m (Plus t) (Plus z) (div2 (get_cons m (Moins z) (Plus z)));
            set_cons m (Plus t) (Moins z) (div2 (get_cons m (Plus z) (Moins z)));
            set_cons m (Plus z) (Moins t) (div2 (get_cons m (Plus z) (Moins z)));
            set_cons m (Moins z) (Moins t)
              (div2 (get_cons m (Moins z) (Plus z)));
            set_cons m (Moins t) (Plus z) (div2 (get_cons m (Moins z) (Plus z)));
            set_cons m (Moins t) (Moins z)
              (div2 (get_cons m (Plus z) (Moins z))))
          l;
        Working_list_imperative.push t m.var
      )

  and get_cons m i j =
    try Hashtbl.find m.map (i, j) with _ -> Integer.Bounded 0

  and new_cons m i j k =
    Working_list_imperative.push (i, j) m.key;
    add_var m i;
    add_var m j;
    try
      let a = Hashtbl.find m.map (i, j) in
      Hashtbl.remove m.map (i, j);
      Hashtbl.add m.map (i, j) (Integer.min a k)
    with _ ->
      (*(m.key.Working_list_imperative.push) (i,j);
        add_var m i;add_var m j;*)
      Hashtbl.add m.map (i, j) k

  and set_cons m i j k =
    Working_list_imperative.push (i, j) m.key;
    add_var m i;
    add_var m j;
    try
      let _ = Hashtbl.find m.map (i, j) in
      Hashtbl.remove m.map (i, j);
      Hashtbl.add m.map (i, j) k
    with _ ->
      (*(m.key.Working_list_imperative.push) (i,j);
        add_var m i;add_var m j;*)
      Hashtbl.add m.map (i, j) k

  let print parameters x =
    match x with
    | Integer.Bounded x -> string_of_int x
    | Integer.Infinity ->
      Remanent_parameters.get_plus_infinity_symbol parameters

  let minus x =
    match x with
    | Integer.Bounded x -> Integer.Bounded (-x)
    | Integer.Infinity -> Integer.Infinity

  let op x =
    match x with
    | Plus x -> Moins x
    | Moins x -> Plus x

  let _test_and_set m i j k =
    if Integer.p (minus (get_cons m (op i) (op j))) k then
      raise Exit
    else
      set_cons m i j k

  let interval_of_pro _parameters _error m x =
    ( minus (div2 (get_cons m (Moins x) (Plus x))),
      div2 (get_cons m (Plus x) (Moins x)) )

  let string_of_pro parameters error m x =
    let inf, sup = interval_of_pro parameters error m x in
    error, "[|" ^ print parameters inf ^ ";" ^ print parameters sup ^ "|]"

  let push _parameters error m x f =
    add_var m (Plus x);
    let k = f.num in
    List.iter
      (fun y ->
        List.iter
          (fun s ->
            let y =
              if s then
                Plus y
              else
                Moins y
            in
            set_cons m (Plus x) y
              (Integer.plus (get_cons m (Plus x) y) (Bounded k));
            set_cons m y (Moins x)
              (Integer.plus (get_cons m y (Moins x)) (Bounded k));

            set_cons m (Moins x) y
              (Integer.plus (get_cons m (Moins x) y) (Bounded (-k)));
            set_cons m y (Plus x)
              (Integer.plus (get_cons m y (Plus x)) (Bounded (-k))))
          [ true; false ])
      (Working_list_imperative.list m.var);
    (*print_string (string_of_pro m x);print_string (print (get_cons m (Plus(x)) (Moins(x))));*)
    error, m

  let affiche parameters error prod =
    print_string "<HR>";
    let error' =
      List.fold_left
        (fun error x ->
          let error, s = string_of_pro parameters error prod x in
          let () = print_string s in
          error)
        error
        (Working_list_imperative.list prod.var)
    in
    print_newline ();
    print_string "<BR>";
    error'
  (*List.iter
       (fun x->
         (List.iter
          (fun y ->
         print_newline ();
    	print_trans x;
         print_trans y;
                print_newline ();
         print_string (print (get_cons prod (Plus(x)) (Plus(y)))^
         print (get_cons prod (Plus(x)) (Moins(y)))^
         print (get_cons prod (Moins(x)) (Plus(y)))^
         print (get_cons prod (Moins(x)) (Moins(y))));print_string "<BR>")
    (prod.var.Working_list_imperative.list ())))
    (prod.var.Working_list_imperative.list  ());
     print_newline () *)

  let create _parameters n =
    {
      key = Working_list_imperative.make n;
      map = Hashtbl.create n;
      var = Working_list_imperative.make n;
    }

  let copy parameters error m =
    let rep =
      create parameters (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let () =
      List.iter
        (fun (i, j) -> set_cons rep i j (get_cons m i j))
        (Working_list_imperative.list m.key)
    in
    error, rep

  let compt_of_var_list parameters error l =
    let tmp =
      create parameters (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let error =
      List.fold_left
        (fun error x ->
          let () = add_var tmp (Plus x) in
          let error, _ = push parameters error tmp x { num = 1; den = 1 } in
          error)
        error l
    in
    error, tmp

  let affiche_mat x = affiche x

  let is_vide prod x =
    not (Integer.p (get_cons prod (Plus x) (Moins x)) (Bounded 0))

  let _is_empty prod =
    List.exists
      (fun x -> is_vide prod x)
      (Working_list_imperative.list prod.var)

  let solve_all _parameters error prod =
    let signe s x =
      if s then
        Plus x
      else
        Moins x
    in
    let () =
      List.iter
        (fun x ->
          new_cons prod (Plus x) (Plus x) (Bounded 0);
          new_cons prod (Moins x) (Moins x) (Bounded 0))
        (Working_list_imperative.list prod.var)
    in
    let () =
      List.iter
        (fun i ->
          List.iter
            (fun x ->
              List.iter
                (fun xsigne ->
                  List.iter
                    (fun y ->
                      List.iter
                        (fun ysigne ->
                          let x = signe xsigne x in
                          let y = signe ysigne y in
                          set_cons prod x y
                            (Integer.minl
                               [
                                 get_cons prod x y;
                                 Integer.plus
                                   (get_cons prod x (Moins i))
                                   (get_cons prod (Moins i) y);
                                 Integer.plus (get_cons prod x (Plus i))
                                   (get_cons prod (Plus i) y);
                                 Integer.plus (get_cons prod x (Plus i))
                                   (Integer.plus
                                      (get_cons prod (Plus i) (Moins i))
                                      (get_cons prod (Moins i) y));
                                 Integer.plus
                                   (get_cons prod x (Moins i))
                                   (Integer.plus
                                      (get_cons prod (Moins i) (Plus i))
                                      (get_cons prod (Plus i) y));
                               ]))
                        [ true; false ])
                    (Working_list_imperative.list prod.var))
                [ true; false ])
            (Working_list_imperative.list prod.var);
          List.iter
            (fun x ->
              List.iter
                (fun y ->
                  set_cons prod (Moins x) (Moins y)
                    (Integer.min
                       (get_cons prod (Moins x) (Moins y))
                       (Integer.div2
                          (Integer.plus
                             (get_cons prod (Moins x) (Plus x))
                             (get_cons prod (Plus y) (Moins y)))));
                  set_cons prod (Plus x) (Moins y)
                    (Integer.min
                       (get_cons prod (Plus x) (Moins y))
                       (Integer.div2
                          (Integer.plus
                             (get_cons prod (Plus x) (Moins x))
                             (get_cons prod (Plus y) (Moins y)))));
                  set_cons prod (Plus x) (Plus y)
                    (Integer.min
                       (get_cons prod (Plus x) (Plus y))
                       (Integer.div2
                          (Integer.plus
                             (get_cons prod (Plus x) (Moins x))
                             (get_cons prod (Moins y) (Plus y)))));
                  set_cons prod (Moins x) (Plus y)
                    (Integer.min
                       (get_cons prod (Moins x) (Plus y))
                       (Integer.div2
                          (Integer.plus
                             (get_cons prod (Moins x) (Plus x))
                             (get_cons prod (Moins y) (Plus y))))))
                (Working_list_imperative.list prod.var))
            (Working_list_imperative.list prod.var))
        (Working_list_imperative.list prod.var)
    in
    let rep =
      if
        List.exists
          (fun x ->
            List.exists
              (fun v ->
                let a = get_cons prod v v in
                match a with
                | Integer.Infinity -> false
                | Integer.Bounded a -> a < 0)
              [ Plus x; Moins x ])
          (Working_list_imperative.list prod.var)
      then
        None
      else
        Some prod
    in
    error, rep

  (* List.iter (fun (i,j) -> set_cons t1 i j (get_cons t2 i j))
             (t1.key.Working_list_imperative.list ())*)

  let solve_inf parameters error prod _l = solve_all parameters error prod

  let merge p q =
    let a = Working_list_imperative.list p.var in
    let b = Working_list_imperative.list q.var in
    List.iter (fun x -> add_var p (Plus x)) b;
    List.iter (fun x -> add_var q (Plus x)) a

  let list_var _parameters p = Working_list_imperative.list p.var

  let _equal m n =
    List.for_all
      (fun (i, j) -> get_cons m i j = get_cons n i j)
      (Working_list_imperative.list m.key)
    && List.for_all
         (fun (i, j) -> get_cons m i j = get_cons n i j)
         (Working_list_imperative.list n.key)

  let widen parameters error m n =
    merge m n;
    let changed = ref false in
    let pro =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let a =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let var_pro x =
      match x with
      | Plus x | Moins x -> x
    in
    List.iter
      (fun (i, j) -> Working_list_imperative.push (i, j) a)
      (Working_list_imperative.list n.key @ Working_list_imperative.list m.key);
    List.iter
      (fun (i, j) ->
        let a, b = get_cons m i j, get_cons n i j in
        if Integer.p b a then (
          changed := true;
          Working_list_imperative.push (var_pro i) pro;
          Working_list_imperative.push (var_pro j) pro;
          if Integer.p (Integer.Bounded 1) a then
            set_cons m i j b
          else
            set_cons m i j Integer.Infinity
        ))
      (Working_list_imperative.list a);
    error, (m, !changed)

  let union_incr parameters error m n =
    merge m n;
    let changed = ref false in
    let pro =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let a =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    let var_pro x =
      match x with
      | Plus x | Moins x -> x
    in
    List.iter
      (fun (i, j) -> Working_list_imperative.push (i, j) a)
      (Working_list_imperative.list n.key @ Working_list_imperative.list m.key);
    List.iter
      (fun (i, j) ->
        let a, b = get_cons m i j, get_cons n i j in
        if Integer.p b a then (
          changed := true;
          Working_list_imperative.push (var_pro i) pro;
          Working_list_imperative.push (var_pro j) pro;
          set_cons m i j b
        ))
      (Working_list_imperative.list a);
    let error, _ = solve_all parameters error m in
    error, (m, !changed)

  let union parameters error m n =
    merge m n;
    let a =
      Working_list_imperative.make
        (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    List.iter
      (fun (i, j) -> Working_list_imperative.push (i, j) a)
      (Working_list_imperative.list n.key @ Working_list_imperative.list m.key);
    List.iter
      (fun (i, j) ->
        let a, b = get_cons m i j, get_cons n i j in
        if Integer.p b a then set_cons m i j b)
      (Working_list_imperative.list a);
    let error, _ = solve_all parameters error m in
    error, m

  let plonge _parameters error m l =
    List.iter (fun x -> add_var m (Plus x)) l;
    error, m

  let is_infinite m x = get_cons m (Plus x) (Moins x) = Integer.Infinity

  let intersection parameters error m n =
    let r =
      create parameters (Remanent_parameters.get_empty_hashtbl_size parameters)
    in
    List.iter
      (fun (i, j) -> set_cons r i j (get_cons m i j))
      (Working_list_imperative.list m.key);
    List.iter
      (fun (i, j) -> set_cons r i j (get_cons n i j))
      (Working_list_imperative.list n.key);
    List.iter
      (fun i ->
        List.iter
          (fun j ->
            set_cons r (Plus i) (Moins j) Integer.Infinity;
            set_cons r (Moins i) (Plus j) Integer.Infinity;
            set_cons r (Plus i) (Plus j) Integer.Infinity;
            set_cons r (Moins i) (Moins j) Integer.Infinity;
            set_cons r (Plus j) (Moins i) Integer.Infinity;
            set_cons r (Moins j) (Plus i) Integer.Infinity;
            set_cons r (Plus j) (Plus i) Integer.Infinity;
            set_cons r (Moins j) (Moins i) Integer.Infinity)
          (Working_list_imperative.list n.var))
      (Working_list_imperative.list m.var);
    List.iter
      (fun i ->
        List.iter
          (fun j ->
            set_cons r (Plus i) (Moins j) Integer.Infinity;
            set_cons r (Moins i) (Plus j) Integer.Infinity;
            set_cons r (Plus i) (Plus j) Integer.Infinity;
            set_cons r (Moins i) (Moins j) Integer.Infinity;
            set_cons r (Plus j) (Moins i) Integer.Infinity;
            set_cons r (Moins j) (Plus i) Integer.Infinity;
            set_cons r (Plus j) (Plus i) Integer.Infinity;
            set_cons r (Moins j) (Moins i) Integer.Infinity)
          (Working_list_imperative.list m.var))
      (Working_list_imperative.list n.var);
    solve_all parameters error r

  let guard parameters error prod l =
    let () =
      List.iter
        (fun (x, cmp, i) ->
          match cmp with
          | Counters_domain_type.GTEQ ->
            set_cons prod (Moins x) (Plus x) (Bounded (-2 * i))
          | Counters_domain_type.GT ->
            set_cons prod (Moins x) (Plus x) (Bounded (-2 * (i + 1)))
          | Counters_domain_type.LTEQ ->
            set_cons prod (Plus x) (Moins x) (Bounded (2 * i))
          | Counters_domain_type.LT ->
            set_cons prod (Plus x) (Moins x) (Bounded (2 * (i - 1)))
          | Counters_domain_type.EQ ->
            set_cons prod (Plus x) (Moins x) (Bounded (2 * i));
            set_cons prod (Moins x) (Plus x) (Bounded (-2 * i)))
        l
    in
    solve_all parameters error prod

  let merge = intersection

  let abstract_away _parameters error prod x =
    let a = Working_list_imperative.list prod.var in
    let () =
      List.iter
        (fun x ->
          List.iter
            (fun var ->
              List.iter
                (fun c1 ->
                  List.iter
                    (fun c2 ->
                      set_cons prod c1 c2 Integer.Infinity;
                      set_cons prod c2 c1 Integer.Infinity)
                    [ Plus var; Moins var ])
                [ Plus x; Moins x ])
            a)
        x
    in
    error, prod

  let interval_of_pro parameters error pro x =
    let inf, sup = interval_of_pro parameters error pro x in
    ( error,
      Some
        ( (match inf with
          | Integer.Infinity -> Fraction.Minfinity
          | Integer.Bounded x ->
            Fraction.Frac { Fraction.num = x; Fraction.den = 1 }),
          match sup with
          | Integer.Infinity -> Fraction.Infinity
          | Integer.Bounded x ->
            Fraction.Frac { Fraction.num = x; Fraction.den = 1 } ) )
end
