(**
  * dag.mli
  *
  * Dag computation and canonical form
  *
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Diderot, CNRS
  *
  * Creation: 02/12/2025
  * Last modification: 02/12/2015
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique
  * et en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type canonical_form
type graph

val compare_canonic : canonical_form -> canonical_form -> int

val graph_of_grid :
  ( Causal.grid,
    graph )
  Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.unary

val canonicalize :
  ( graph,
    canonical_form )
  Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.unary

module type StoryTable = sig
  type table

  val fold_table :
    ( ( Trace.t,
        StoryProfiling.StoryStats.log_info Trace.Simulation_info.t list,
        'a,
        'a )
      Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.ternary,
      table,
      'a,
      'a )
    Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.ternary

  val init_table :
    table Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.zeroary

  val count_stories : table -> int

  val add_story :
    ( Causal.grid,
      Trace.t,
      StoryProfiling.StoryStats.log_info Trace.Simulation_info.t list,
      table,
      table )
    Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.quaternary

  val hash_list :
    (table, table) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.unary

  val sort_list :
    ( table,
      (Trace.t
      * Causal.grid
      * StoryProfiling.StoryStats.log_info Trace.Simulation_info.t list)
      list )
    Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.unary
end

module StoryTable : StoryTable
