open Fraction
open Intervalles
open Intertab

module NR =
functor
  (I : Tabinter
         with type var = Occu1.trans
          and type intervalle = Intervalles.intervalle)
  ->
  (
    struct
      type intertab = I.intervalle_tab
      type prod = intertab
      type var = Occu1.trans

      let addzero = true

      let compt_of_var_list parameters error l =
        I.int_of_var_list parameters error l

      let affiche_mat parameters error x = I.affiche parameters error x

      let is_vide prod x =
        I.read prod x
        = { inf = Frac { num = 0; den = 1 }; sup = Frac { num = 0; den = 1 } }

      let is_infinite m x = (I.read m x).sup = Fraction.Infinity
      let is_minfinite m x = (I.read m x).inf = Fraction.Minfinity
      let _is_both_infinite m x = is_infinite m x && is_minfinite m x
      let _is_either_infinite m x = is_infinite m x || is_minfinite m x
      let create _parameters n = I.make n
      let f_un = { num = 1; den = 1 }
      let f_zero = { num = 0; den = 1 }
      let _un = { inf = Frac f_un; sup = Frac f_un }
      let _zero = { inf = Frac f_zero; sup = Frac f_zero }
      let list_var _parameters p = I.clef p

      let guard parameters error p l =
        (*  let classe=classe p (List.rev_map (fun (a,_,_) -> a) (List.rev l))  in*)
        let error, i2 = I.copy parameters error p in
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
          error, Some i2
        with Intervalle_vide -> error, None

      let union parameters error p q = I.union parameters error p q

      let merge parameters error p q =
        try
          let error, a = I.merge parameters error p q in
          error, Some a
        with Intervalle_vide -> error, None

      let plonge _parameters error m _l = error, m

      let bin_incr gen parameters error p q =
        let error, i = gen parameters error p q in
        if i = [] then
          error, (p, false)
        else
          error, (p, true)

      let widen parameters error p q =
        bin_incr I.wide_place parameters error p q

      let union_incr parameters error p q =
        bin_incr I.union_place parameters error p q

      let interval_of_pro _parameters error m x = error, I.read m x

      let string_of_pro parameters error m x =
        let error, interv = interval_of_pro parameters error m x in
        Intervalles.string_of_intervalle parameters error interv

      let interval_of_pro parameters error m x =
        let error, interv = interval_of_pro parameters error m x in
        error, Some (interv.inf, interv.sup)

      let push _parameters error m x f =
        let _ = I.push m x f in
        error, m

      let _translate parameters error m l =
        List.fold_left
          (fun (error, m) (x, i) ->
            push parameters error m x { num = i; den = 1 })
          (error, m) l (* TO DO -> do more efficiently *)

      let solve_all _parameters error m = error, Some m
      let solve_inf _parameters error m _l = error, Some m
      let copy parameters error m = I.copy parameters error m

      let abstract_away parameters error m l =
        let error, i = I.abstract_away parameters error m l in
        error, i
    end :
      Mat_inter.Mat_inter with type var = Occu1.trans)

module Non_rel = NR (Intertab.Tabinter)
