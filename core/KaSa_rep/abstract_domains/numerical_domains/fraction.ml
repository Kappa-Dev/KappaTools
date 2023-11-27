type fraction = { num: int; den: int }
type ffraction = Frac of fraction | Infinity | Unknown | Minfinity

let trunc a =
  match a with
  | Frac { num = x; den = y } -> x / y
  | Infinity | Unknown | Minfinity -> raise Exit

let rec floor_int a =
  match a with
  | Frac { num = x; den = y } ->
    (match x >= 0, y > 0 with
    | true, true -> x / y
    | false, false -> floor_int (Frac { num = -x; den = -y })
    | true, false -> -cell_int (Frac { num = x; den = -y })
    | false, true -> -cell_int (Frac { num = -x; den = y }))
  | Infinity | Unknown | Minfinity -> raise Exit

and cell_int a =
  match a with
  | Frac { num = x; den = y } ->
    (match x >= 0, y > 0 with
    | true, true ->
      let q = x / y in
      if q * y = x then
        q
      else
        q + 1
    | false, false -> cell_int (Frac { num = -x; den = -y })
    | true, false -> -floor_int (Frac { num = x; den = -y })
    | false, true -> -floor_int (Frac { num = -x; den = y }))
  | Infinity | Unknown | Minfinity -> raise Exit

let zero = { num = 0; den = 1 }

let pgcd a b =
  let rec aux a b =
    if b = 0 then
      a
    else
      aux b (a mod b)
  in
  let a, b = abs a, abs b in
  if a < b then
    aux b a
  else
    aux a b

let reduit { num = n; den = d } =
  if n = 0 then
    { num = 0; den = 1 }
  else (
    let e = pgcd n d in
    let n, d = n / e, d / e in
    if d < 0 then
      { num = -n; den = -d }
    else
      { num = n; den = d }
  )

let finf a b = a.num * b.den < a.den * b.num
let finfeq a b = a.num * b.den <= a.den * b.num

let fplus a b =
  reduit { num = (a.num * b.den) + (a.den * b.num); den = a.den * b.den }

let fmoins a b =
  reduit { num = (a.num * b.den) - (a.den * b.num); den = a.den * b.den }

let ffois a b = reduit { num = a.num * b.num; den = a.den * b.den }
let fdiv a b = reduit { num = a.num * b.den; den = a.den * b.num }

let ffdiv a b =
  match a, b with
  | Frac a, Frac b -> Frac (fdiv a b)
  | Infinity, Frac a when a.num > 0 -> Infinity
  | Infinity, Frac a when a.num < 0 -> Minfinity
  | Minfinity, Frac a when a.num < 0 -> Infinity
  | Minfinity, Frac a when a.num > 0 -> Minfinity
  | Frac _, Infinity -> Frac { num = 0; den = 1 }
  | Frac _, Minfinity -> Frac { num = 0; den = 1 }
  | Unknown, _
  | _, Unknown
  | (Infinity | Minfinity), (Infinity | Minfinity | Frac _) ->
    Unknown

let ffplus a i b =
  if i.num = 0 then
    a
  else (
    let c =
      match b with
      | Unknown -> Unknown
      | Infinity when i.num > 0 -> Infinity
      | Minfinity when i.num > 0 -> Minfinity
      | Infinity -> Minfinity
      | Minfinity -> Infinity
      | Frac b -> Frac (ffois i b)
    in
    match a, c with
    | _, Frac c when c.num = 0 -> a
    | Unknown, _ -> Unknown
    | _, Unknown -> Unknown
    | Infinity, Minfinity -> Unknown
    | Infinity, _ -> Infinity
    | Minfinity, Infinity -> Unknown
    | _, Infinity -> Infinity
    | Minfinity, _ -> Minfinity
    | _, Minfinity -> Minfinity
    | Frac a, Frac b -> Frac (fplus a b)
  )

let ffneg a = ffplus (Frac zero) { num = -1; den = 1 } a

let ffmin a b =
  match a, b with
  | Unknown, _ | _, Unknown -> raise Exit
  | Minfinity, _ | _, Infinity -> a
  | Frac x, Frac y when (fmoins x y).num < 0 -> a
  | (Infinity | Frac _), b -> b

let ffinf a b =
  match a, b with
  | Unknown, _ | _, Unknown -> raise Exit
  | Minfinity, _ | _, Infinity -> true
  | Frac x, Frac y when (fmoins x y).num < 0 -> true
  | (Infinity | Frac _), _ -> false

let ffmax a b =
  match a, b with
  | Unknown, _ | _, Unknown -> raise Exit
  | Minfinity, _ | _, Infinity -> b
  | Frac x, Frac y when (fmoins x y).num < 0 -> b
  | a, (Minfinity | Frac _) -> a

let fsup a b =
  if finf a b then
    b
  else
    a

let string_of a =
  if a.den = 1 then
    string_of_int a.num
  else
    "(" ^ string_of_int a.num ^ "/" ^ string_of_int a.den ^ ")"
