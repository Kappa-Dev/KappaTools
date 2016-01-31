module Product:
functor
  (New_domain:Analyzer_domain_sig.Domain) ->
functor
  (Underlying_domain:Analyzer_domain_sig.Domain) ->
Analyzer_domain_sig.Domain
