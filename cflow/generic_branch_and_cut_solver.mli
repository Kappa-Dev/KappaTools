module type Solver = 
  (sig 
    module PH:Propagation_heuristics.Blackboard_with_heuristic

    val compress: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.update_order list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard * PH.B.assign_result  * PH.B.result list) PH.B.PB.CI.Po.K.H.with_handler
      
    val detect_independent_events: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.PB.step_id list) PH.B.PB.CI.Po.K.H.with_handler

    val filter: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val sub: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.CI.Po.K.refined_step list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val clean: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val translate: (PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.refined_step list * PH.B.result) PH.B.PB.CI.Po.K.H.with_handler 
    val translate_result: PH.B.result -> PH.B.PB.CI.Po.K.refined_step list 

    end)

module Solver:Solver
		
