type 'a bot_or_not =
  | Bot
  | Not_bot of 'a

type maybe_bool =
  | Sure_value of bool
  | Maybe

type 'a top_or_not =
  | Top
  | Not_top of 'a

type 'a flat_lattice =
  | Val of 'a
  | Any
  | Undefined

let lub a b =
  match
    a,b
  with
  | Undefined,_ -> b
  | _,Undefined -> a
  | Any, _ | _,Any -> Any
  | Val x,Val y when x=y -> a
  | Val x,Val y -> Any
