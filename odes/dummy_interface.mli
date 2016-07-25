(** Network/ODE generation
  * Creation: 20/07/2016
  * Last modification: Time-stamp: <Jul 25 2016>
 *)

module Interface: Ode_interface.Interface with type compil = (Ast.agent, Ast.mixture, string, Ast.rule) Ast.compil
