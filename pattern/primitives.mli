(** binding or modifying a port that has been added or kept from the lhs *)
type id = FRESH of int | KEPT of int
type port = id * int
type action =
    BND of (port * port)
  | FREE of (port * bool) (** FREE(p,is_side_effect_free) *)
  | MOD of (port * int)
  | DEL of int
  | ADD of (int * int) (**(id in mixture, name_id)*)

module IdMap : MapExt.S with type key = id
module PortMap : MapExt.S with type key = port
module ActionSet : Set.S with type elt = action

module Causality :
sig
  type t
  val is_link_tested : t -> bool
  val is_link_modif : t -> bool
  val is_link_something : t -> bool
  val is_internal_tested : t -> bool
  val is_internal_modif : t -> bool
  val is_internal_something : t -> bool

  (** [create internal_tested link_tested] *)
  val create : bool -> bool -> t
  val add_internal_modif : t -> t
  val add_link_modif : t -> t

  val to_int : t -> int
end

type rule = {
  k_def : Expr.alg_expr; (** standard kinetic constant *)
  k_alt : Expr.alg_expr option * Expr.alg_expr option;
  (** Possible unary kinetic rate *)
  over_sampling : float option;
  (** Boosted kinetic rate for Bologna technique *)
  script : action list;
  balance : (int * int * int);	(** #deleted,#preserved,#removed *)
  lhs : Mixture.t;
  rhs : Mixture.t;
  r_id : int;
  added : Mods.IntSet.t;
  modif_sites : Mods.Int2Set.t IdMap.t;
  pre_causal : Causality.t PortMap.t;
  is_pert: bool;
  cc_impact :
    (Mods.IntSet.t Mods.IntMap.t * Mods.IntSet.t Mods.IntMap.t *
       Mods.IntSet.t Mods.IntMap.t) option;
  add_token : (Expr.alg_expr * int) list;
  rm_token : (Expr.alg_expr * int) list
}

type modification =
    ITER_RULE of Expr.alg_expr Term.with_pos * rule
  | UPDATE of Term.dep_type * Expr.alg_expr Term.with_pos
  | SNAPSHOT of Expr.alg_expr Ast.print_expr Term.with_pos list
  | STOP of Expr.alg_expr Ast.print_expr Term.with_pos list
  | CFLOW of int
  | FLUX of Expr.alg_expr Ast.print_expr Term.with_pos list
  | FLUXOFF of Expr.alg_expr Ast.print_expr Term.with_pos list
  | CFLOWOFF of int
  | PLOTENTRY
  | PRINT of
      (Expr.alg_expr Ast.print_expr Term.with_pos list *
	 Expr.alg_expr Ast.print_expr Term.with_pos list)

type perturbation =
    { precondition: Expr.alg_expr Ast.bool_expr;
      effect : modification list;
      abort : Expr.alg_expr Ast.bool_expr option;
      stopping_time : Nbr.t option
    }
