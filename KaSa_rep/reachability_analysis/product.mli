(** Functor to combine several abstract domains (with explicit communiaction among them) *)

module Product:
functor
  (New_domain:Analyzer_domain_sig.Domain) ->
functor
  (Underlying_domain:Analyzer_domain_sig.Domain) ->
Analyzer_domain_sig.Domain

(** The functor is almost symmetric, but better time performances will be 
obtained by using New_domain as a single domain and UNderlying_domain as a 
list of domains (obtained as the result of the product domain.
That is to say that the time performance will be better with right linear 
tree *)
  
