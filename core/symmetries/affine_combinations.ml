type ('mix, 'id) anonamised_expr = ('mix, 'id) Alg_expr.e Loc.annoted

let rec anonamise (expr : ('mix, 'id) Alg_expr.e Loc.annoted) :
    ('mix, 'id) anonamised_expr =
  Loc.annot_with_dummy
    (match fst expr with
    | Alg_expr.BIN_ALG_OP (op, e1, e2) ->
      Alg_expr.BIN_ALG_OP (op, anonamise e1, anonamise e2)
    | Alg_expr.UN_ALG_OP (op, e) -> Alg_expr.UN_ALG_OP (op, anonamise e)
    | ( Alg_expr.STATE_ALG_OP _ | Alg_expr.ALG_VAR _ | Alg_expr.KAPPA_INSTANCE _
      | Alg_expr.TOKEN_ID _ | Alg_expr.CONST _ ) as e ->
      e
    | Alg_expr.IF (b, e1, e2) ->
      Alg_expr.IF (anonamise_bool b, anonamise e1, anonamise e2)
    | Alg_expr.DIFF_TOKEN (e, id) -> Alg_expr.DIFF_TOKEN (anonamise e, id)
    | Alg_expr.DIFF_KAPPA_INSTANCE (e, id) ->
      Alg_expr.DIFF_KAPPA_INSTANCE (anonamise e, id))

and anonamise_bool bool =
  Loc.annot_with_dummy
    (match fst bool with
    | (Alg_expr.TRUE | Alg_expr.FALSE) as e -> e
    | Alg_expr.BIN_BOOL_OP (op, b1, b2) ->
      Alg_expr.BIN_BOOL_OP (op, anonamise_bool b1, anonamise_bool b2)
    | Alg_expr.UN_BOOL_OP (op, b1) -> Alg_expr.UN_BOOL_OP (op, anonamise_bool b1)
    | Alg_expr.COMPARE_OP (op, e1, e2) ->
      Alg_expr.COMPARE_OP (op, anonamise e1, anonamise e2))

module AnonamisedExprSetMap = SetMap.Make (struct
  type t = (Pattern.id array list, int) anonamised_expr

  let compare = compare
  let print _ _ = ()
end)

module AnonamisedExprMap = AnonamisedExprSetMap.Map

type aff_combination = {
  linear: Fractions.t AnonamisedExprMap.t;
  affine: Fractions.t;
}

let of_int i = { linear = AnonamisedExprMap.empty; affine = Fractions.of_int i }

let apply_1 op aff1 =
  {
    linear =
      AnonamisedExprMap.fold
        (fun k a map ->
          let result = op a in
          if Fractions.is_zero result then
            map
          else
            AnonamisedExprMap.add k (op a) map)
        aff1.linear AnonamisedExprMap.empty;
    affine = op aff1.affine;
  }

let apply_2 op aff1 aff2 =
  {
    linear =
      snd
        (AnonamisedExprMap.monadic_fold2 () ()
           (fun _ err k a b map ->
             let rep = op a b in
             ( err,
               if Fractions.is_zero rep then
                 AnonamisedExprMap.remove k map
               else
                 AnonamisedExprMap.add k rep map ))
           (fun _ err _ _ map -> err, map)
           (fun _ err k a map -> err, AnonamisedExprMap.add k a map)
           aff1.linear aff2.linear aff1.linear);
    affine = op aff1.affine aff2.affine;
  }

let sum aff1 aff2 = apply_2 Fractions.add aff1 aff2
let minus aff1 aff2 = apply_2 Fractions.sub aff1 aff2

let mul_scal i aff =
  let f = Fractions.of_int i in
  apply_1 (Fractions.mult f) aff

let div_scal aff i =
  let f = Fractions.of_int i in
  let f_inv_opt = Fractions.inv f in
  match f_inv_opt with
  | None -> None
  | Some f_inv -> Some (apply_1 (Fractions.mult f_inv) aff)

let necessarily_equal aff1 aff2 =
  Fractions.is_equal aff1.affine aff2.affine
  && AnonamisedExprMap.equal Fractions.is_equal aff1.linear aff2.linear

let is_int nbr =
  match nbr with
  | Nbr.I _ -> true
  | Nbr.F f -> float_of_int (Nbr.to_int nbr) = f
  | Nbr.I64 i -> Int64.of_int (Nbr.to_int nbr) = i

let atom expr =
  {
    linear = AnonamisedExprMap.add expr Fractions.one AnonamisedExprMap.empty;
    affine = Fractions.zero;
  }

let linearise expr =
  let expr = anonamise expr in
  let rec aux expr =
    match fst expr with
    | Alg_expr.BIN_ALG_OP (Operator.SUM, e1, e2) -> sum (aux e1) (aux e2)
    | Alg_expr.BIN_ALG_OP (Operator.MINUS, e1, e2) -> minus (aux e1) (aux e2)
    | Alg_expr.BIN_ALG_OP (Operator.MULT, (Alg_expr.CONST nbr, _), e2)
      when is_int nbr ->
      mul_scal (Nbr.to_int nbr) (aux e2)
    | Alg_expr.BIN_ALG_OP (Operator.MULT, e1, (Alg_expr.CONST nbr, _))
      when is_int nbr ->
      mul_scal (Nbr.to_int nbr) (aux e1)
    | Alg_expr.BIN_ALG_OP (Operator.DIV, e1, (Alg_expr.CONST nbr, _))
      when is_int nbr ->
      (match div_scal (aux e1) (Nbr.to_int nbr) with
      | None -> atom expr
      | Some a -> a)
    | Alg_expr.UN_ALG_OP (Operator.UMINUS, e) -> mul_scal (-1) (aux e)
    | Alg_expr.CONST nbr when is_int nbr ->
      {
        affine = Fractions.of_int (Nbr.to_int nbr);
        linear = AnonamisedExprMap.empty;
      }
    | Alg_expr.IF _ | Alg_expr.STATE_ALG_OP _ | Alg_expr.ALG_VAR _
    | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _
    | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
    | Alg_expr.UN_ALG_OP
        ( ( Operator.COSINUS | Operator.SINUS | Operator.LOG | Operator.SQRT
          | Operator.TAN | Operator.INT | Operator.EXP ),
          _ )
    | Alg_expr.CONST _
    | Alg_expr.BIN_ALG_OP
        ( ( Operator.MULT | Operator.MIN | Operator.MAX | Operator.DIV
          | Operator.POW | Operator.MODULO ),
          _,
          _ ) ->
      atom expr
  in
  aux expr
