module KaSaUtil : functor (E : Export_to_KaSa.Type) -> sig
  val print_analysis_result : float -> E.state -> E.state

  val print_backdoor_timing :
    Remanent_parameters_sig.parameters -> float -> unit
end
