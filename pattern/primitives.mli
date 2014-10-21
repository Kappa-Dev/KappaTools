type 'a variable =
    CONST of 'a
  | VAR of
      ((int -> Nbr.t) -> (int -> Nbr.t) -> float ->
       int -> int -> float -> (int -> Nbr.t) -> 'a)
