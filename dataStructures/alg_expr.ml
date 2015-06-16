type t =
    BIN_ALG_OP of Term.bin_alg_op * t Term.with_pos * t Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * t Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t
