module type Expr_sig = sig
  type mix
  type id
end

type ('mix, 'id) var = Token of 'id | Instances of 'mix

module type Lin_comb = sig
  type mix
  type id
  type elt
  type t

  val of_expr :
    (id -> (mix, id) Alg_expr.e Locality.annot option) ->
    (mix, id) Alg_expr.e Locality.annot ->
    t option

  val print :
    sep:string ->
    product:string ->
    (Ode_loggers_sig.t -> mix -> unit) ->
    (Ode_loggers_sig.t -> id -> unit) ->
    Ode_loggers_sig.t ->
    t ->
    unit
end

module Make =
functor
  (Expr_sig : Expr_sig)
  ->
  (
    struct
      type mix = Expr_sig.mix
      type id = Expr_sig.id
      type elt = (mix, id) var

      module O = struct
        type t = elt

        let compare = compare
        let print _ _ = ()
      end

      module SetMap = SetMap.Make (O)
      module Map = SetMap.Map

      type t = Nbr.t Map.t

      let liftunit1 f () () a b c = (), f a b c
      let liftunit2 f () () a b c d = (), f a b c d
      let empty = Some Map.empty

      let fold2 f g h t1 t2 acc =
        snd
          (Map.monadic_fold2 () () (liftunit2 h) (liftunit1 f) (liftunit1 g) t1
             t2 acc)

      let add t1 t2 =
        fold2 Map.add Map.add
          (fun k x y t ->
            let z = Nbr.add x y in
            if Nbr.is_zero z then
              t
            else
              Map.add k z t)
          t1 t2 Map.empty

      let singleton elt = Some (Map.add elt Nbr.one Map.empty)

      let scale alpha t =
        if Nbr.is_zero alpha then
          Map.empty
        else if Nbr.is_equal alpha Nbr.one then
          t
        else
          Map.map (Nbr.mult alpha) t

      let minus_one = Nbr.neg Nbr.one

      let lift1 f a =
        match a with
        | None -> None
        | Some a -> Some (f a)

      let lift2 f a b =
        match a, b with
        | None, _ | _, None -> None
        | Some a, Some b -> Some (f a b)

      let add = lift2 add
      let scale a = lift1 (scale a)

      let rec of_expr env expr =
        match expr with
        | Alg_expr.BIN_ALG_OP (op, a, b), _ ->
          (match op with
          | Operator.SUM -> add (of_expr env a) (of_expr env b)
          | Operator.MINUS ->
            add (of_expr env a) (scale minus_one (of_expr env b))
          | Operator.MULT ->
            (match a, b with
            | (Alg_expr.CONST a, _), b | b, (Alg_expr.CONST a, _) ->
              scale a (of_expr env b)
            | ( ( Alg_expr.DIFF_KAPPA_INSTANCE _, _
                | Alg_expr.DIFF_TOKEN _, _
                | Alg_expr.STATE_ALG_OP _, _
                | Alg_expr.ALG_VAR _, _
                | Alg_expr.KAPPA_INSTANCE _, _
                | Alg_expr.TOKEN_ID _, _
                | Alg_expr.IF _, _
                | Alg_expr.BIN_ALG_OP _, _
                | Alg_expr.UN_ALG_OP _, _ ),
                ( Alg_expr.DIFF_KAPPA_INSTANCE _, _
                | Alg_expr.DIFF_TOKEN _, _
                | Alg_expr.STATE_ALG_OP _, _
                | Alg_expr.ALG_VAR _, _
                | Alg_expr.KAPPA_INSTANCE _, _
                | Alg_expr.TOKEN_ID _, _
                | Alg_expr.IF _, _
                | Alg_expr.BIN_ALG_OP _, _
                | Alg_expr.UN_ALG_OP _, _ ) ) ->
              None)
          | Operator.DIV ->
            (match b with
            | Alg_expr.CONST b, _ when Nbr.is_zero b -> None
            | Alg_expr.CONST b, _ ->
              scale (Nbr.internal_div Nbr.one b) (of_expr env a)
            | ( ( Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
                | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
                | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
                | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
                | Alg_expr.DIFF_TOKEN _ ),
                _ ) ->
              None)
          | Operator.POW | Operator.MODULO | Operator.MIN | Operator.MAX -> None)
        | Alg_expr.UN_ALG_OP (op, a), _ ->
          (match op with
          | Operator.UMINUS -> scale minus_one (of_expr env a)
          | Operator.COSINUS | Operator.EXP | Operator.SINUS | Operator.TAN
          | Operator.SQRT | Operator.LOG | Operator.INT ->
            None)
        | Alg_expr.CONST a, _ when Nbr.is_zero a -> empty
        | Alg_expr.ALG_VAR x, _ ->
          (match env x with
          | None -> None
          | Some a -> of_expr env (Alg_expr_extra.simplify a))
        | Alg_expr.KAPPA_INSTANCE mix, _ -> singleton (Instances mix)
        | Alg_expr.TOKEN_ID id, _ -> singleton (Token id)
        | Alg_expr.DIFF_KAPPA_INSTANCE _, _
        | Alg_expr.DIFF_TOKEN _, _
        | Alg_expr.STATE_ALG_OP _, _
        | Alg_expr.CONST _, _
        | Alg_expr.IF _, _ ->
          None

      let to_list t =
        List.rev_map (fun (a, b) -> b, a) (List.rev (Map.bindings t))

      let print_first logger sep first =
        if first then
          ()
        else
          Ode_loggers_sig.fprintf logger "%s" sep

      let print_v pr_mix pr_token logger var =
        match var with
        | Token id -> pr_token logger id
        | Instances mix -> pr_mix logger mix

      let print ~sep ~product pr_mix pr_token logger t =
        let l = to_list t in
        let _ =
          List.fold_left
            (fun first (coef, var) ->
              if Nbr.is_zero coef then
                first
              else (
                let () = print_first logger sep first in
                if Nbr.is_equal Nbr.one coef then (
                  let () = print_v pr_mix pr_token logger var in
                  false
                ) else (
                  let () =
                    Ode_loggers_sig.fprintf logger "%s%s" (Nbr.to_string coef)
                      product
                  in
                  let () = print_v pr_mix pr_token logger var in
                  false
                )
              ))
            true l
        in
        ()
    end :
      Lin_comb with type mix = Expr_sig.mix and type id = Expr_sig.id)

module Lin = Make (struct
  type id = int
  type mix = int
end)
