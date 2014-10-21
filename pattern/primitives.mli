type 'a variable =
    CONST of 'a
  | VAR of
      ((int -> Nbr.t) -> (int -> Nbr.t) -> float ->
       int -> int -> float -> (int -> Nbr.t) -> 'a)

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
  k_def : Nbr.t variable; (** standard kinetic constant *)
  k_alt : Nbr.t variable option * Nbr.t variable option;
  (** Possible unary kinetic rate *)
  over_sampling : float option;
  (** Boosted kinetic rate for Bologna technique *)
  script : action list;
  balance : (int * int * int);	(** #deleted,#preserved,#removed *)
  kappa: string;
  lhs : Mixture.t;
  rhs : Mixture.t;
  refines: int option; (** mixture id that is refined by lhs *)
  r_id : int;
  added : Mods.IntSet.t;
  modif_sites : Mods.Int2Set.t IdMap.t;
  pre_causal : Causality.t PortMap.t;
  is_pert: bool;
  cc_impact :
    (Mods.IntSet.t Mods.IntMap.t * Mods.IntSet.t Mods.IntMap.t *
       Mods.IntSet.t Mods.IntMap.t) option;
  add_token : (Nbr.t variable * int) list;
  rm_token : (Nbr.t variable * int) list
}

type modification =
    INTRO of Nbr.t variable * Mixture.t
  | DELETE of Nbr.t variable * Mixture.t
  | UPDATE_RULE of int * Nbr.t variable
  | UPDATE_VAR of int * Nbr.t variable
  | UPDATE_TOK of int * Nbr.t variable
  | SNAPSHOT of Ast.mixture Ast.print_expr Term.with_pos list
  | STOP of Ast.mixture Ast.print_expr Term.with_pos list
  | CFLOW of int
  | FLUX of Ast.mixture Ast.print_expr Term.with_pos list
  | FLUXOFF of Ast.mixture Ast.print_expr Term.with_pos list
  | CFLOWOFF of int
  | PRINT of
      (Ast.mixture Ast.print_expr Term.with_pos list *
	 Ast.mixture Ast.print_expr Term.with_pos list)

type perturbation =
    { precondition: bool variable;
      effect : (rule option * modification) list;
      abort : bool variable option;
      flag : string;
      stopping_time : Nbr.t option
    }
