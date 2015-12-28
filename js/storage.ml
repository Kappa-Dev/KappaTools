let model_nb_plot, set_model_nb_plot = React.S.create 0
let model_max_events, set_model_max_events = React.S.create None
let model_max_time, set_model_max_time = React.S.create None
let model_text, set_model_text = React.S.create ""

let opened_filename, set_opened_filename = React.S.create "model.ka"

let fake_env =
  Environment.init
    (Signature.create []) (NamedDecls.create [||]) (NamedDecls.create [||])
    (Operator.DepSet.empty,Operator.DepSet.empty,[||],[||])
    ([||],[||],Connected_component.Set.empty) [||] [||]

let model_env, set_model_env = React.S.create fake_env
let model_domain, set_model_domain =
  React.S.create Connected_component.Env.empty
let model_counter =
  React.S.l5 Counter.create
	     model_nb_plot  (React.S.const 0.) (React.S.const 0)
	     model_max_time model_max_events

let model_syntax_error, set_model_syntax_error = React.S.create None
let parse text =
  try
    let () = set_model_syntax_error None in
    Some (KappaParser.start_rule KappaLexer.token text Ast.empty_compil)
  with ExceptionDefn.Syntax_Error e ->
    let () = set_model_syntax_error (Some e) in None

let model_ast =
  React.S.fmap parse Ast.empty_compil (React.S.l1 Lexing.from_string model_text)
