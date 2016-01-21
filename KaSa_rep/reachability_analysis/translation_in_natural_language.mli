type token
type rename_sites =   (Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.site_name -> Exception.method_handler * Cckappa_sig.site_name) 
       
val translate: Remanent_parameters_sig.parameters ->
	       Mvbdu_wrapper.Mvbdu.handler ->
	       Exception.method_handler ->
	       rename_sites -> 
	       Mvbdu_wrapper.Mvbdu.mvbdu -> Exception.method_handler * (Mvbdu_wrapper.Mvbdu.handler * token)
val print: 
  ?beginning_of_sentence:bool ->
  ?prompt_agent_type:bool ->
  ?html_mode:bool ->
  show_dep_with_dimmension_higher_than:int
  -> Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.method_handler ->
  string ->
  Cckappa_sig.agent_name ->
  token ->
  Exception.method_handler
	     
