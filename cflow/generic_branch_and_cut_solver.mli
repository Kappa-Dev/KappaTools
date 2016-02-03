module type Solver =
  (sig
    module PH:Propagation_heuristics.Blackboard_with_heuristic

    val compress: (PH.B.blackboard,PH.update_order list,PH.B.blackboard * PH.B.assign_result  * PH.B.result list) PH.B.PB.CI.Po.K.H.binary
    val detect_independent_events: (PH.B.blackboard,PH.B.PB.step_id list,PH.B.PB.step_id list) PH.B.PB.CI.Po.K.H.binary
    val filter: (PH.B.blackboard,PH.B.PB.step_id list,PH.B.blackboard) PH.B.PB.CI.Po.K.H.binary

    val sub: (PH.B.PB.CI.Po.K.refined_step list,PH.B.blackboard) PH.B.PB.CI.Po.K.H.unary

    val clean: (PH.B.blackboard,PH.B.blackboard) PH.B.PB.CI.Po.K.H.unary

    val translate: (PH.B.blackboard,PH.B.PB.step_id list,PH.B.PB.CI.Po.K.refined_step list * PH.B.result) PH.B.PB.CI.Po.K.H.binary
    val translate_result: PH.B.result -> PH.B.PB.CI.Po.K.refined_step list

    end)

module Solver:Solver

