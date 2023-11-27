type t = { num: int; den: int }

let sign f =
  if f.num = 0 then
    { num = 0; den = 1 }
  else if f.den < 0 then
    { num = -f.num; den = -f.den }
  else
    f

let simplify f =
  let gcd = Tools.gcd_2 f.num f.den in
  sign { num = f.num / gcd; den = f.den / gcd }

let add a b =
  simplify { num = (a.num * b.den) + (b.num * a.den); den = b.den * a.den }

let op f = { f with num = -f.num }
let sub a b = add a (op b)
let mult a b = simplify { num = a.num * b.num; den = a.den * b.den }

let inv a =
  if a.num = 0 then
    None
  else
    Some { num = a.den; den = a.num }

let div a b =
  match inv b with
  | None -> None
  | Some b_inv -> Some (mult a b_inv)

let zero = { num = 0; den = 1 }
let is_equal a b = a = b
let of_int i = simplify { num = i; den = 1 }
let is_zero a = a.num = 0
let one = of_int 1
