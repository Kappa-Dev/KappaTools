val local_trace : bool

val translate_compil :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  (Ast.agent, Ast.mixture, Ast.agent list list, string, Ast.rule) Ast.compil ->
  Exception_without_parameter.method_handler
  * ( Ckappa_sig.agent,
      Ckappa_sig.mixture,
      Ckappa_sig.mixture,
      string,
      Ckappa_sig.mixture Ckappa_sig.rule )
    Ast.compil

val modif_map :
  ('a -> 'b Locality.annot -> 'a * 'c Locality.annot) ->
  ('a -> 'd -> 'a * 'e) ->
  'a ->
  ('d, 'f, 'g, 'b) Ast.modif_expr ->
  'a * ('e, 'h, 'g, 'c) Ast.modif_expr

val rev_ast : 'a list -> 'a list

val add_entry :
  Remanent_parameters_sig.parameters ->
  Ckappa_sig.c_agent_id ->
  'a ->
  'b ->
  'c ->
  Exception_without_parameter.method_handler
  * ('a * 'b * 'c) list Ckappa_sig.Agent_id_map_and_set.Map.t ->
  Exception_without_parameter.method_handler
  * ('a * 'b * 'c) list Ckappa_sig.Agent_id_map_and_set.Map.t

val map_with_pos :
  ('a -> 'b -> 'c -> Exception_without_parameter.method_handler * 'e) ->
  'a ->
  'b ->
  'c * 'f ->
  Exception_without_parameter.method_handler * ('e * 'f)

val alg_map :
  (Exception_without_parameter.method_handler ->
  'b ->
  Exception_without_parameter.method_handler * 'c) ->
  Exception_without_parameter.method_handler ->
  ('b, 'd) Alg_expr.e ->
  Exception_without_parameter.method_handler * ('c, 'd) Alg_expr.e

val bool_map :
  (Exception_without_parameter.method_handler ->
  'b ->
  Exception_without_parameter.method_handler * 'c) ->
  Exception_without_parameter.method_handler ->
  ('b, 'd) Alg_expr.bool ->
  Exception_without_parameter.method_handler * ('c, 'd) Alg_expr.bool

val with_option_map :
  ('a ->
  Exception_without_parameter.method_handler ->
  'c ->
  Exception_without_parameter.method_handler * 'd) ->
  'a ->
  Exception_without_parameter.method_handler ->
  'c option ->
  Exception_without_parameter.method_handler * 'd option
