module KaSaUtil : functor (E : Export_to_KaSa.Type) -> sig
  val print_analysis_result : float -> E.state -> E.state
end
