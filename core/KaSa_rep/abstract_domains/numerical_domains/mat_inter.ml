open Fraction
open Intervalles
open Matrices
open Intertab
open Occu1

module type Mat_inter = sig
  type prod
  type var

  val addzero : bool
  val list_var : Remanent_parameters_sig.parameters -> prod -> var list

  val solve_inf :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var list ->
    Exception.method_handler * prod option

  val create : Remanent_parameters_sig.parameters -> int -> prod

  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var list ->
    Exception.method_handler * prod

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    Exception.method_handler * prod

  (* val exclusion:
       Remanent_parameters_sig.parameters ->
       Exception.method_handler ->
       prod -> var list ->
       Exception.method_handler * bool

     val all_here :
       Remanent_parameters_sig.parameters ->
       Exception.method_handler ->
             prod -> var list -> Exception.method_handler * prod option*)

  val guard :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    (var * Counters_domain_type.comparison_op * int) list ->
    Exception.method_handler * prod option

  val solve_all :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    Exception.method_handler * prod option

  val compt_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * prod

  val affiche_mat :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    Exception.method_handler

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    prod ->
    Exception.method_handler * prod option

  val is_vide : prod -> var -> bool

  val string_of_pro :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var ->
    Exception.method_handler * string

  val interval_of_pro :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var ->
    Exception.method_handler * (Fraction.ffraction * Fraction.ffraction) option

  val is_infinite : prod -> var -> bool

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    prod ->
    Exception.method_handler * prod

  (*val plus:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->prod->
    Exception.method_handler * prod*)
  val widen :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    prod ->
    Exception.method_handler * (prod * bool)

  val union_incr :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    prod ->
    Exception.method_handler * (prod * bool)

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var ->
    Fraction.fraction ->
    Exception.method_handler * prod

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod ->
    var list ->
    Exception.method_handler * prod
end

module Mat_inter =
functor
  (M : Matrice with type var = Occu1.trans)
  (I : Tabinter
         with type var = Occu1.trans
          and type intervalle = Intervalles.intervalle)
  ->
  (
    struct
      type matrice = M.matrice
      type intertab = I.intervalle_tab
      type prod = { mat: matrice; i: intertab }
      type var = Occu1.trans

      let addzero = true
      let _n_ligne p = M.n_ligne p.mat

      let compt_of_var_list parameters error l =
        let error, mat = M.mat_of_var_list parameters error l in
        let error, i = I.int_of_var_list parameters error l in
        error, { mat; i }

      let affiche_mat parameters error x =
        let error = I.affiche parameters error x.i in
        let error = M.affiche parameters error x.mat in
        error

      let is_vide prod x =
        I.read prod.i x
        = { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }

      let is_infinite m x = (I.read m.i x).sup = Fraction.Infinity
      let is_minfinite m x = (I.read m.i x).inf = Fraction.Minfinity
      let is_both_infinite m x = is_infinite m x && is_minfinite m x
      let is_either_infinite m x = is_infinite m x || is_minfinite m x

      type side = Upper | Lower

      let reduce_lower _parameters error inter t b =
        let () =
          I.set inter t (cap_inter (I.read inter t) { inf = b; sup = Infinity })
        in
        error

      let reduce_upper _parameters error inter t b =
        let () =
          I.set inter t
            (cap_inter (I.read inter t) { inf = Minfinity; sup = b })
        in
        error

      (* improve this algorithm in the presence of -oo  ?? *)
      let solve_inf parameters error prod l =
        let m = prod.mat in
        let inter = prod.i in
        let error, m = M.copy parameters error m in
        let li = ref (List.filter (fun x -> is_either_infinite prod x) l) in
        let nli = ref 1 in
        let error_ref = ref error in
        while !nli <> 0 do
          let l = !li in
          let error, posm = M.copy parameters !error_ref m in
          let () = error_ref := error in
          let rec aux_174 liste =
            match liste with
            | [] -> ()
            | j :: q when not (is_either_infinite prod j) -> aux_174 q
            | j :: q when is_both_infinite prod j -> aux_174 q
            | j :: q ->
              let rec aux2 i lneg lpos =
                if i > M.n_ligne m then
                  lneg, lpos
                else (
                  let s = (M.read_val posm i j).num in
                  if s = 0 then
                    aux2 (i + 1) lneg lpos
                  else if s > 0 then
                    aux2 (i + 1) lneg (i :: lpos)
                  else
                    aux2 (i + 1) (i :: lneg) (i :: lpos)
                )
              in
              let rec aux3 i l =
                match l with
                | t :: q ->
                  let k =
                    fmoins { num = 0; den = 1 }
                      (fdiv (M.read_val posm t j) (M.read_val posm i j))
                  in
                  M.addligne posm t k i;
                  aux3 i q
                | [] -> ()
              in
              let lneg, lpos = aux2 1 [] [] in
              (match lneg, lpos, is_infinite prod j with
              | l, posref :: _, true | posref :: _, l, false ->
                let () = aux3 posref l in
                aux_174 q
              | _, [], true | [], _, false -> aux_174 q)
          in
          let () = aux_174 l in

          let lprob =
            List.filter
              (fun j ->
                let rec aux_214 q =
                  if (M.read_val posm q j).num < 0 && is_infinite prod j then
                    true
                  else if (M.read_val posm q j).num > 0 && is_minfinite prod j
                  then
                    true
                  else if q <= 2 then
                    false
                  else
                    aux_214 (q - 1)
                in
                aux_214 (M.n_ligne posm))
              l
          in
          List.iter
            (fun j ->
              try
                let size = M.n_ligne posm in
                for i1 = 1 to size do
                  for i2 = i1 + 1 to size do
                    let a = M.read_val posm i1 j in
                    let b = M.read_val posm i2 j in
                    if a = { num = 0; den = 1 } || b = { num = 0; den = 1 } then
                      ()
                    else (
                      let f x y = fmoins x (ffois a (fdiv y b)) in
                      if
                        List.for_all
                          (fun x ->
                            (f (M.read_val posm i1 x) (M.read_val posm i2 x))
                              .num >= 0)
                          l
                      then (
                        let error =
                          M.new_empty_ligne parameters !error_ref posm
                        in
                        let () = error_ref := error in
                        let size = M.n_ligne posm in
                        M.addligne posm size { num = 1; den = 1 } i1;
                        M.addligne posm size
                          (ffois { num = -1; den = 1 } (fdiv a b))
                          i2;
                        raise Exit
                      ) else (
                        let g x y = fmoins x (ffois b (fdiv y a)) in
                        if
                          List.for_all
                            (fun x ->
                              (g (M.read_val posm i2 x) (M.read_val posm i1 x))
                                .num >= 0)
                            l
                        then (
                          let error =
                            M.new_empty_ligne parameters !error_ref posm
                          in
                          let () = error_ref := error in
                          let size = M.n_ligne posm in
                          M.addligne posm size { num = 1; den = 1 } i2;
                          M.addligne posm size
                            (ffois { num = -1; den = 1 } (fdiv a b))
                            i1;
                          raise Exit
                        )
                      )
                    )
                  done
                done
              with Exit -> ())
            lprob;

          let n = Remanent_parameters.get_empty_hashtbl_size parameters in
          let pos = Hashtbl.create n in
          (* variable -> contraintes o� il apparait positivement*)
          let neg = Hashtbl.create n in
          (* variable -> contraintes o� il apparait n�gativement*)
          let nb_inf = Array.make (M.n_ligne posm + 1) 0 in
          (* contrainte -> nb de monomes  non major�e *)
          let nb_minf = Array.make (M.n_ligne posm + 1) 0 in
          (* contrainte -> nb de monome non minor�e *)
          let good_line = Working_list_imperative.make n in
          (*contraintes � r�duire*)
          (*let solved=Working_list_imperative.make n in (*variable trouv�e*)*)
          let visited_line = Working_list_imperative.make n in

          (*contraintes r�duites ou en cours*)
          (* let read_t t x =
             try (Working_list_imperative.list (Hashtbl.find t x))
                 with _ -> [] in*)
          let update t x y =
            let l =
              try Hashtbl.find t x
              with _ ->
                let l = Working_list_imperative.make n in
                Hashtbl.add t x l;
                l
            in
            Working_list_imperative.push y l
          in
          let view (k, v) =
            (*met en attente une contrainte*)
            if not (Working_list_imperative.member k visited_line) then (
              Working_list_imperative.push k visited_line;
              Working_list_imperative.push (k, v) good_line
            )
          in
          let rec vide () =
            (*traite les contraintes en attentes*)
            try
              let k =
                match Working_list_imperative.pop good_line with
                | Some (k, _) -> k
                | None -> raise Exit
              in
              (let rec check_double_infinity error list ~has_seen_minus_infinity
                   ~has_seen_plus_infinity
                   ~has_seen_unsimplifiable_plus_infinity
                   ~has_seen_unsimplifiable_minus_infinity =
                 match list with
                 | [] ->
                   ( error,
                     ( has_seen_unsimplifiable_minus_infinity,
                       has_seen_unsimplifiable_plus_infinity ) )
                 | Affine_cst :: q ->
                   check_double_infinity error q ~has_seen_minus_infinity
                     ~has_seen_plus_infinity
                     ~has_seen_unsimplifiable_plus_infinity
                     ~has_seen_unsimplifiable_minus_infinity
                 | ((Bool _ | Counter _ | Site _) as t) :: q ->
                   let delta = M.read_val posm k t in
                   (match compare delta.num 0 with
                   | 0 ->
                     check_double_infinity error q ~has_seen_minus_infinity
                       ~has_seen_plus_infinity
                       ~has_seen_unsimplifiable_plus_infinity
                       ~has_seen_unsimplifiable_minus_infinity
                   | -1 ->
                     let error, new_plus_infinity =
                       match (I.read inter t).inf with
                       | Minfinity -> error, true
                       | Frac _ -> error, false
                       | Infinity | Unknown ->
                         Exception.warn parameters error __POS__ Exit false
                     in
                     let error, new_minus_infinity =
                       match (I.read inter t).sup with
                       | Infinity -> error, true
                       | Frac _ -> error, false
                       | Minfinity | Unknown ->
                         Exception.warn parameters error __POS__ Exit false
                     in
                     let has_seen_unsimplifiable_plus_infinity =
                       has_seen_unsimplifiable_plus_infinity
                       || (new_plus_infinity && has_seen_minus_infinity)
                     in
                     let has_seen_unsimplifiable_minus_infinity =
                       has_seen_unsimplifiable_minus_infinity
                       || (new_minus_infinity && has_seen_plus_infinity)
                     in
                     let has_seen_plus_infinity =
                       has_seen_plus_infinity || new_plus_infinity
                     in
                     let has_seen_minus_infinity =
                       has_seen_minus_infinity || new_minus_infinity
                     in
                     if
                       has_seen_unsimplifiable_minus_infinity
                       && has_seen_unsimplifiable_plus_infinity
                     then
                       error, (true, true)
                     else
                       check_double_infinity error q ~has_seen_minus_infinity
                         ~has_seen_plus_infinity
                         ~has_seen_unsimplifiable_plus_infinity
                         ~has_seen_unsimplifiable_minus_infinity
                   | 1 ->
                     let error, new_minus_infinity =
                       match (I.read inter t).inf with
                       | Minfinity -> error, true
                       | Frac _ -> error, false
                       | Infinity | Unknown ->
                         Exception.warn parameters error __POS__ Exit false
                     in
                     let error, new_plus_infinity =
                       match (I.read inter t).sup with
                       | Infinity -> error, true
                       | Frac _ -> error, false
                       | Minfinity | Unknown ->
                         Exception.warn parameters error __POS__ Exit false
                     in
                     let has_seen_unsimplifiable_plus_infinity =
                       has_seen_unsimplifiable_plus_infinity
                       || (new_plus_infinity && has_seen_minus_infinity)
                     in
                     let has_seen_unsimplifiable_minus_infinity =
                       has_seen_unsimplifiable_minus_infinity
                       || (new_minus_infinity && has_seen_plus_infinity)
                     in
                     let has_seen_plus_infinity =
                       has_seen_plus_infinity || new_plus_infinity
                     in
                     let has_seen_minus_infinity =
                       has_seen_minus_infinity || new_minus_infinity
                     in
                     if
                       has_seen_unsimplifiable_minus_infinity
                       && has_seen_unsimplifiable_plus_infinity
                     then
                       error, (true, true)
                     else
                       check_double_infinity error q ~has_seen_minus_infinity
                         ~has_seen_plus_infinity
                         ~has_seen_unsimplifiable_plus_infinity
                         ~has_seen_unsimplifiable_minus_infinity
                   | _ ->
                     Exception.warn parameters error __POS__ Exit (true, true))
               in
               let rec vide error side list somme waiting =
                 match list with
                 | [] -> vide_waiting error side somme waiting
                 | Affine_cst :: q ->
                   vide error side q
                     (match side with
                     | Lower ->
                       ffplus somme { num = -1; den = 1 }
                         (Frac (M.read_val posm k Affine_cst))
                     | Upper ->
                       ffplus somme { num = -1; den = 1 }
                         (Frac (M.read_val posm k Affine_cst)))
                     waiting
                 | ((Bool _ | Counter _ | Site _) as t) :: q ->
                   let delta = M.read_val posm k t in
                   (match compare delta.num 0, side with
                   | 0, _ -> vide error side q somme waiting
                   | -1, Lower | 1, Upper ->
                     (match (I.read inter t).inf with
                     | Minfinity ->
                       let error = vide2 error side (t, Upper) delta q somme in
                       (match (I.read inter t).inf with
                       | Minfinity | Infinity | Unknown ->
                         (*let error, () =
                             Exception.warn parameters error __POS__ Exit ()
                           in*)
                         error
                       | Frac _ as f ->
                         vide error side q
                           (ffplus somme
                              { num = -delta.num; den = delta.den }
                              f)
                           waiting)
                     | Frac _ as f ->
                       vide error side q
                         (ffplus somme { num = -delta.num; den = delta.den } f)
                         ((delta, f, t, Upper) :: waiting)
                     | Infinity | Unknown ->
                       (*let error, () =
                           Exception.warn parameters error __POS__ Exit ()
                         in*)
                       error)
                   | 1, Lower | -1, Upper ->
                     (match (I.read inter t).sup with
                     | Infinity ->
                       let error = vide2 error side (t, Lower) delta q somme in
                       (match (I.read inter t).sup with
                       | Minfinity | Infinity | Unknown ->
                         (*let error, () =
                             Exception.warn parameters error __POS__ Exit ()
                           in*)
                         error
                       | Frac _ as f ->
                         vide error side q
                           (ffplus somme
                              { num = -delta.num; den = delta.den }
                              f)
                           waiting)
                     | Frac _ as f ->
                       vide error side q
                         (ffplus somme { num = -delta.num; den = delta.den } f)
                         ((delta, f, t, Lower) :: waiting)
                     | Minfinity | Unknown ->
                       (*let error, () =
                           Exception.warn parameters error __POS__ Exit ()
                         in*)
                       error)
                   | _, (Lower | Upper) ->
                     (*let error, () =
                         Exception.warn parameters error __POS__ Exit ()
                       in*)
                     error)
               and vide2 error side t t_delta q somme =
                 match q with
                 | [] ->
                   let t, side_t = t in
                   let new_bound = ffdiv somme (Frac t_delta) in
                   (match side_t with
                   | Lower -> reduce_lower parameters error inter t new_bound
                   | Upper -> reduce_upper parameters error inter t new_bound)
                 | head :: tail ->
                   let delta = M.read_val posm k head in
                   (match compare delta.num 0, side with
                   | 0, _ -> vide2 error side t t_delta tail somme
                   | -1, Lower | 1, Upper ->
                     vide2 error side t t_delta tail
                       (ffplus somme
                          { num = -delta.num; den = delta.den }
                          (I.read inter head).inf)
                   | 1, Lower | -1, Upper ->
                     vide2 error side t t_delta tail
                       (ffplus somme
                          { num = -delta.num; den = delta.den }
                          (I.read inter head).sup)
                   | _, (Lower | Upper) ->
                     (*let error, () =
                       Exception.warn parameters error __POS__ Exit ()
                       in*)
                     error)
               and vide_waiting error _side somme waiting =
                 List.fold_left
                   (fun error (delta, f, t, side) ->
                     let somme = ffplus somme delta f in
                     let somme = ffdiv somme (Frac delta) in
                     match side with
                     | Lower -> reduce_lower parameters error inter t somme
                     | Upper -> reduce_upper parameters error inter t somme)
                   error waiting
               in
               let error, line = M.get_line parameters !error_ref posm k in
               let error, (blocked_minus_infinity, blocked_plus_infinity) =
                 check_double_infinity error (M.get_trans_list line)
                   ~has_seen_plus_infinity:false ~has_seen_minus_infinity:false
                   ~has_seen_unsimplifiable_plus_infinity:false
                   ~has_seen_unsimplifiable_minus_infinity:false
               in
               let error =
                 if not blocked_minus_infinity then
                   vide error Lower (M.get_trans_list line)
                     (Frac { num = 0; den = 1 })
                     []
                 else
                   error
               in
               let error =
                 if not blocked_plus_infinity then
                   vide error Upper (M.get_trans_list line)
                     (Frac { num = 0; den = 1 })
                     []
                 else
                   error
               in
               let () = error_ref := error in
               ());
              (* begin
                 (if nb_inf.(k)=0
                  then
                    (
                    let rec vide list somme  =
                        match list
                        with
                        | Affine_cst::q ->
                          vide q
                            (ffplus
                               (Frac({num=0;den=1}))
                               {num=(-1);den=1}
                               (Frac((M.read_val posm k Affine_cst))))
                               | (Bool _ | Counter _ as t)::q       ->
                                 let delta=(M.read_val posm k t) in
                                 (match delta.num
                                  with
                                  | 0 -> vide q somme
                                  | a when a<0 ->
                                    vide q
                                      (ffplus somme
                                         {num=(-(delta.num));
                                          den=delta.den}
                                         ((I.read inter (t)).inf))
                                  | a when a>0 ->
                                    vide q
                                      (ffplus somme
                                         {num=(-(delta.num));den=delta.den}
                                         ((I.read inter (t)).sup))
                                  |  _ -> vide q somme)

                        | (Bool _ | Counter _ as t)::q       ->
                          let delta=(M.read_val posm k t) in
                          (match delta.num
                           with
                           | 0 -> vide q somme
                           | a when a<0 ->
                             vide q
                               (ffplus somme
                                  {num=(-(delta.num));
                                   den=delta.den}
                                  ((I.read inter (t)).inf))
                           | a when a>0 ->
                             vide q
                               (ffplus somme
                                  {num=(-(delta.num));den=delta.den}
                                  ((I.read inter (t)).sup))
                           |  _ -> vide q somme)
                        |   []        -> somme
                      in
                      let error, line  = M.get_line parameters (!error_ref) posm k in
                      let () = error_ref:=error in
                      let sup=
                        vide (M.get_trans_list line)  (Frac{num=0;den=1})
                      in
                      let rec vide2 l =
                        match l
                        with
                        | Affine_cst::q -> vide2 q
                        | (Bool _ | Counter _ as t)::q ->
                          let delta=(M.read_val posm k t) in
                          (match delta.num with
                           | 0 -> vide2 q
                           | a when a>0 -> (vide2 q)
                           | _  ->
                             (
                              let s2=(ffplus sup delta (I.read inter t).inf) in
                              (
                                (I.set inter t
                                   (cap_inter
                                      (I.read inter t)
                                      {inf=Minfinity;
                                       sup=(ffdiv s2 (Frac(delta)))})
                                );
                                (solve t);vide2 q)))
                        |  [] -> ()
                      in
                      let error, line  =
                        M.get_line parameters (!error_ref) posm k in
                      let () = error_ref:=error in
                      vide2
                        (M.get_trans_list line)
                    ));
                 (if nb_minf.(k)=0
                  then
                    (let rec vide list somme  =
                       match list
                       with
                         Affine_cst::q ->
                         vide q
                           (ffplus
                              (Frac({num=0;den=1}))
                              {num=(-1);den=1} (Frac(M.read_val posm k Affine_cst)))

                       |  (Bool _ | Counter _ as t)::q       ->
                         let delta=(M.read_val posm k t) in
                         (match delta.num
                          with
                          | 0 -> vide q somme
                          | a when a<0 ->
                            vide q
                              (ffplus
                                 somme
                                 {num=(-(delta.num));
                                  den=delta.den}
                                 ((I.read inter t).sup))
                          | a when a>0 ->
                            vide q
                              (ffplus
                                 somme
                                 {num=(-(delta.num));den=delta.den}
                                 ((I.read inter t).inf))
                          |  _ -> vide q somme)
                       |   []        -> somme
                     in
                     let error, line  =
                       M.get_line parameters (!error_ref) posm k in
                     let () = error_ref:=error in

                     let inf=(vide
                                (M.get_trans_list line)
                                (Frac{num=0;den=1}))
                     in
                     let rec vide2 l =
                       match l
                       with
                       | Affine_cst::q -> vide2 q
                       | (Bool _ | Counter _ as t)::q ->
                         let delta=(M.read_val posm k t) in
                         (match delta.num with
                          | 0 -> vide2 q
                          | a when a<0 -> (vide2 q)
                          | _  ->
                            (let s2=
                               ffplus
                                 inf
                                 delta
                                 (I.read inter t).inf
                             in
                             ((I.set inter t)
                                (cap_inter (I.read inter (t))
                                   {inf=Minfinity;
                                    sup=(ffdiv s2 (Frac(delta)))}
                                ));
                             solve t;
                             vide2 q))
                       |  [] -> () in
                       let error, line  =
                         M.get_line parameters (!error_ref) posm k in
                       let () = error_ref:=error in

                     vide2 (M.get_trans_list line)))
                  end;*)
              vide ()
            with _ -> ()
          in
          for i = 1 to M.n_ligne posm do
            let rep = ref Affine_cst in
            let error, line = M.get_line parameters error posm i in
            let k = M.get_trans_list line in
            let () = error_ref := error in
            List.iter
              (fun j ->
                if p j Affine_cst > 0 then (
                  match (I.read inter j).sup, (M.read_val posm i j).num with
                  | Infinity, a when a > 0 ->
                    update pos j i;
                    rep := j;
                    nb_inf.(i) <- 1 + nb_inf.(i)
                  | Infinity, a when a < 0 ->
                    update neg j i;
                    rep := j;
                    nb_minf.(i) <- 1 + nb_minf.(i)
                  | (Minfinity | Infinity | Unknown | Frac _), _ -> ()
                ))
              k;
            if nb_inf.(i) = 0 || nb_minf.(i) = 0 then (
              view (i, !rep);
              vide ()
            )
          done;
          nli := List.length !li;
          li := (List.filter (fun x -> is_infinite prod x)) l;
          nli := !nli - List.length !li
        done;

        (**************************************************************************************)
        let transcribe_constraint k =
          (*t_i k;   t_s "\n";*)
          let error, (k, c) = M.get_line parameters !error_ref m k in
          let () = error_ref := error in
          let rec cop_line (k, c) =
            match k with
            | Affine_cst :: q -> cop_line (q, c)
            | [] | (Bool _ | Counter _ | Site _) :: _ ->
              let nl =
                Hashtbl.create
                  (Remanent_parameters.get_empty_hashtbl_size parameters)
              in
              List.iter
                (fun x ->
                  Hashtbl.add nl x
                    (try Hashtbl.find c x with _ -> { num = 0; den = 1 }))
                k;
              ( k,
                nl,
                let a =
                  try
                    let a = Hashtbl.find c Affine_cst in
                    Frac { num = -a.num; den = a.den }
                  with _ -> Frac { num = 0; den = 1 }
                in
                { inf = a; sup = a } )
          in
          cop_line (k, c)
        in
        let n = Remanent_parameters.get_empty_hashtbl_size parameters in
        let nm = M.make parameters n in
        let aff = Hashtbl.create n in
        let read_aff i =
          try Hashtbl.find aff i
          with _ ->
            { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }
        in
        let change_aff i i2 =
          (try Hashtbl.remove aff i with _ -> ());
          Hashtbl.add aff i i2
        in
        let n_copy_line (k, c, b) =
          let error = M.new_copy_ligne parameters !error_ref nm (k, c) in
          let () = error_ref := error in
          let n = M.n_ligne nm in
          change_aff n b
        in
        for l = 1 to M.n_ligne m do
          let k = transcribe_constraint l in
          n_copy_line k
        done;
        let simplify_pivot ligne =
          let (error, (k, c)), b =
            M.get_line parameters !error_ref nm ligne, read_aff ligne
          in
          let () = error_ref := error in
          let rec vide l sol =
            match l with
            | t :: q ->
              let delta =
                try Hashtbl.find c t with _ -> { num = 0; den = 1 }
              in
              vide q
                {
                  inf =
                    ffplus sol.inf
                      { num = -delta.num; den = delta.den }
                      (if delta.num < 0 then
                         (I.read inter t).inf
                       else
                         (I.read inter t).sup);
                  sup =
                    ffplus sol.sup
                      { num = -delta.num; den = delta.den }
                      (if delta.num < 0 then
                         (I.read inter t).sup
                       else
                         (I.read inter t).inf);
                }
            | [] -> sol
          in
          match k with
          | [] -> ()
          | t :: q ->
            let rep = vide q b in
            let i =
              {
                inf =
                  (match rep.inf with
                  | Frac a -> Frac a
                  | Infinity | Minfinity | Unknown -> Minfinity);
                sup =
                  (match rep.sup with
                  | Frac a -> Frac a
                  | Infinity | Minfinity | Unknown -> Infinity);
              }
            in
            (match try Hashtbl.find c t with _ -> { num = 0; den = 1 } with
            | delta when delta.num > 0 ->
              let new_i =
                cap_inter (I.read inter t)
                  {
                    inf = ffdiv i.inf (Frac delta);
                    sup = ffdiv i.sup (Frac delta);
                  }
              in
              I.set inter t new_i
            | delta when delta.num < 0 ->
              let new_i =
                cap_inter (I.read inter t)
                  {
                    inf = ffdiv i.sup (Frac delta);
                    sup = ffdiv i.inf (Frac delta);
                  }
              in
              I.set inter t new_i
            | _ -> ())
        in
        let reduit deb fin =
          let rec aux k =
            if k > fin then
              M.del_last_ligne nm
            else (
              let rec search_good_ligne l rep wei =
                if l > fin then
                  rep, wei
                else (
                  try
                    let cur = M.pivot m l in
                    if p cur wei < 0 || wei = Affine_cst then
                      search_good_ligne (l + 1) l cur
                    else
                      search_good_ligne (1 + l) rep wei
                  with _ -> search_good_ligne (1 + l) rep wei
                )
              in
              let new_ligne, wei = search_good_ligne deb (-1) Affine_cst in
              if wei = Affine_cst then
                aux (fin + 1)
              else (
                let col = M.pivot m new_ligne in
                let error = M.swap parameters !error_ref nm k new_ligne in
                error_ref := error;
                let tmp =
                  try Hashtbl.find aff k
                  with _ ->
                    let error = !error_ref in
                    let error, a =
                      Exception.warn parameters error __POS__ Exit
                        {
                          inf = Fraction.Frac Fraction.zero;
                          sup = Fraction.Frac Fraction.zero;
                        }
                    in
                    let () = error_ref := error in
                    a
                in

                Hashtbl.remove aff k;
                Hashtbl.add aff k
                  (try Hashtbl.find aff new_ligne
                   with _ ->
                     let error = !error_ref in
                     let error, a =
                       Exception.warn parameters error __POS__ Exit
                         {
                           inf = Fraction.Frac Fraction.zero;
                           sup = Fraction.Frac Fraction.zero;
                         }
                     in
                     let () = error_ref := error in
                     a);
                Hashtbl.remove aff new_ligne;
                Hashtbl.add aff new_ligne tmp;
                let error =
                  M.mulligne parameters !error_ref nm k
                    (fdiv { num = 1; den = 1 } (M.read_val m k col))
                in
                error_ref := error;
                (let tmp =
                   try Hashtbl.find aff k
                   with _ ->
                     let error = !error_ref in
                     let error, a =
                       Exception.warn parameters error __POS__ Exit
                         {
                           inf = Fraction.Frac Fraction.zero;
                           sup = Fraction.Frac Fraction.zero;
                         }
                     in
                     let () = error_ref := error in
                     a
                 in
                 Hashtbl.remove aff k;
                 Hashtbl.add aff k
                   (iiplus
                      {
                        inf = Frac { num = 0; den = 1 };
                        sup = Frac { num = 0; den = 1 };
                      }
                      (fdiv { num = 1; den = 1 } (M.read_val nm k col))
                      tmp));

                for i = deb to fin do
                  if i = k then
                    ()
                  else (
                    let alpha =
                      ffois { num = -1; den = 1 } (M.read_val m i col)
                    in
                    M.addligne m i alpha k;
                    let tmp = Hashtbl.find aff i in
                    Hashtbl.remove aff i;
                    Hashtbl.add aff i
                      (iiplus tmp alpha
                         (try Hashtbl.find aff k
                          with _ ->
                            let error = !error_ref in
                            let error, a =
                              Exception.warn parameters error __POS__ Exit
                                {
                                  inf = Fraction.Frac Fraction.zero;
                                  sup = Fraction.Frac Fraction.zero;
                                }
                            in
                            let () = error_ref := error in
                            a))
                  )
                done;
                aux (k + 1)
              )
            )
          in
          aux deb
        in
        let reduce_pivot ligne =
          let b =
            try Hashtbl.find aff ligne
            with _ ->
              let error = !error_ref in
              let error, a =
                Exception.warn parameters error __POS__ Exit
                  {
                    inf = Fraction.Frac Fraction.zero;
                    sup = Fraction.Frac Fraction.zero;
                  }
              in
              let () = error_ref := error in
              a
          in
          let error, (k, c) = M.get_line parameters !error_ref nm ligne in
          let () = error_ref := error in
          (*affiche_cons (k,c,b);*)
          match k with
          | t :: q ->
            let cop_line (k, c, b) =
              let nl = Hashtbl.create n in
              List.iter
                (fun x ->
                  if p x Affine_cst > 0 then
                    Hashtbl.add nl x
                      (try Hashtbl.find c x with _ -> { num = 0; den = 1 }))
                k;
              k, nl, b
            in
            let _k, c, i = cop_line (k, c, b) in
            let delta = try Hashtbl.find c t with _ -> { num = 0; den = 1 } in
            (try Hashtbl.remove c t with _ -> ());
            let i =
              iiplus i { num = -delta.num; den = delta.den } (I.read inter t)
            in
            Hashtbl.add aff (M.n_ligne nm + 1) i;
            let error = M.new_copy_ligne parameters !error_ref nm (q, c) in

            let () = error_ref := error in
            ()
          | [] -> ()
        in
        let deb = ref 1 in
        let fin = ref (M.n_ligne nm) in
        while !deb < !fin + 1 do
          for i = !deb to !fin do
            let () = simplify_pivot i in
            let () = reduce_pivot i in
            ()
          done;
          reduit (!fin + 1) (M.n_ligne nm);
          deb := !fin + 1;
          fin := M.n_ligne nm (*;*)
        done;
        for k = 1 to M.n_ligne nm do
          simplify_pivot (M.n_ligne nm + 1 - k)
        done;
        !error_ref

      let classe p _l = M.get_all_key p.mat
      let create parameters n = { mat = M.make parameters n; i = I.make n }
      let f_un = { num = 1; den = 1 }
      let f_zero = { num = 0; den = 1 }
      let _un = { inf = Frac f_un; sup = Frac f_un }
      let _zero = { inf = Frac f_zero; sup = Frac f_zero }

      let list_var parameters p =
        let rep =
          Working_list_imperative.make
            (Remanent_parameters.get_empty_hashtbl_size parameters)
        in
        List.iter
          (fun x -> Working_list_imperative.push x rep)
          (M.get_all_key p.mat);
        List.iter (fun x -> Working_list_imperative.push x rep) (I.clef p.i);
        List.filter
          (fun x -> not (x = Affine_cst))
          (Working_list_imperative.list rep)

      let red2 mi = mi

      let solve_inf parameters error mi c =
        let rec aux k error =
          (* let error = affiche_mat parameters error mi in*)
          if k > 5 then
            error, mi
          else (
            let error, tmp = I.copy parameters error mi.i in
            let error = solve_inf parameters error mi c in
            let _ = red2 mi in
            if I.equal tmp mi.i then
              error, mi
            else
              aux (k + 1) error
          )
        in
        aux 0 error

      let solve_inf parameters error mi c =
        try
          let error, mi = solve_inf parameters error mi c in
          error, Some mi
        with Intervalle_vide -> error, None (*to do: propagate error *)

      let guard parameters error p l =
        let classe =
          classe p (List.rev_map (fun (a, _, _) -> a) (List.rev l))
        in
        let error, m2 = M.copy parameters error p.mat in
        let error, i2 = I.copy parameters error p.i in
        try
          let () =
            List.iter
              (fun (j, cmp, i) ->
                I.set i2 j
                  (cap_inter (I.read i2 j)
                     (match cmp with
                     | Counters_domain_type.EQ ->
                       {
                         inf = Frac { num = i; den = 1 };
                         sup = Frac { num = i; den = 1 };
                       }
                     | Counters_domain_type.GT ->
                       { inf = Frac { num = i + 1; den = 1 }; sup = Infinity }
                     | Counters_domain_type.GTEQ ->
                       { inf = Frac { num = i; den = 1 }; sup = Infinity }
                     | Counters_domain_type.LT ->
                       { inf = Minfinity; sup = Frac { num = i - 1; den = 1 } }
                     | Counters_domain_type.LTEQ ->
                       { sup = Frac { num = i; den = 1 }; inf = Minfinity })))
              l
          in
          solve_inf parameters error { mat = m2; i = i2 } classe
        with Intervalle_vide -> error, None

      let gen_bin f_m f_i parameters error p q =
        let error, mat = f_m parameters error p.mat q.mat in
        let error, i = f_i parameters error p.i q.i in
        error, { mat; i }

      let union parameters error p q =
        gen_bin M.union I.union parameters error p q

      let merge parameters error p q =
        try
          let error, a = gen_bin M.merge I.merge parameters error p q in
          error, Some a
        with Intervalle_vide -> error, None

      let plonge parameters error m l =
        let error, mat = M.plonge parameters error m.mat l in
        error, { m with mat }

      let bin_incr gen parameters error p q =
        let n = M.n_ligne p.mat in
        let error, newm = M.union parameters error p.mat q.mat in
        (* to do, test if newm <> p.mat *)
        let error, i = gen parameters error p.i q.i in
        if n = M.n_ligne newm && i = [] then
          error, ({ mat = newm; i = p.i }, false)
        else
          error, ({ mat = newm; i = p.i }, true)

      let widen parameters error p q =
        bin_incr I.wide_place parameters error p q

      let union_incr parameters error p q =
        bin_incr I.union_place parameters error p q

      let solve_all parameters error m =
        solve_inf parameters error m (list_var parameters m)

      let interval_of_pro _parameters error m x = error, I.read m.i x

      let string_of_pro parameters error m x =
        let error, interv = interval_of_pro parameters error m x in
        Intervalles.string_of_intervalle parameters error interv

      let interval_of_pro parameters error m x =
        let error, interv = interval_of_pro parameters error m x in
        error, Some (interv.inf, interv.sup)

      let push parameters error m x f =
        let _ = I.push m.i x f in
        let error = M.push parameters error m.mat x f in
        error, m

      let _translate parameters error m l =
        List.fold_left
          (fun (error, m) (x, i) ->
            push parameters error m x { num = i; den = 1 })
          (error, m) l (* TO DO -> do more efficiently *)

      let copy parameters error m =
        let error, mat = M.copy parameters error m.mat in
        let error, i = I.copy parameters error m.i in
        error, { mat; i }

      let abstract_away parameters error m l =
        let error, mat = M.abstract_away parameters error m.mat l in
        let error, i = I.abstract_away parameters error m.i l in
        error, { mat; i }
    end :
      Mat_inter with type var = Occu1.trans)

module Mat_int = Mat_inter (Matrices.Matrice) (Intertab.Tabinter)
