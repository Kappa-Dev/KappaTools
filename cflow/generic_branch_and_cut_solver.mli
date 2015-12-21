module type Solver = 
  (sig 
    module PH:Propagation_heuristics.Blackboard_with_heuristic

    val compress: (StoryProfiling.StoryStats.log_info -> PH.B.blackboard -> PH.update_order list -> Exception.method_handler * StoryProfiling.StoryStats.log_info * PH.B.blackboard * PH.B.assign_result  * PH.B.result list) PH.B.PB.CI.Po.K.H.with_handler
      
    val detect_independent_events: (StoryProfiling.StoryStats.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> Exception.method_handler * StoryProfiling.StoryStats.log_info * PH.B.PB.step_id list) PH.B.PB.CI.Po.K.H.with_handler

    val filter: (StoryProfiling.StoryStats.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> Exception.method_handler * StoryProfiling.StoryStats.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val sub: (StoryProfiling.StoryStats.log_info -> (*PH.B.blackboard ->*) PH.B.PB.CI.Po.K.refined_step list -> Exception.method_handler * StoryProfiling.StoryStats.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val clean: (StoryProfiling.StoryStats.log_info -> PH.B.blackboard -> Exception.method_handler * StoryProfiling.StoryStats.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val translate: (PH.B.blackboard -> PH.B.PB.step_id list -> Exception.method_handler * PH.B.PB.CI.Po.K.refined_step list * PH.B.result) PH.B.PB.CI.Po.K.H.with_handler 
    val translate_result: PH.B.result -> PH.B.PB.CI.Po.K.refined_step list 

    end)

module Solver:Solver
		
