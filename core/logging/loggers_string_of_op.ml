let string_of_un_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.UMINUS ->
    (match format with
    | Loggers.SBML -> "<minus/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "-"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.LOG ->
    (match format with
    | Loggers.SBML -> "<log/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "log"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.SQRT ->
    (match format with
    | Loggers.SBML -> "<root/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "sqrt"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.EXP ->
    (match format with
    | Loggers.SBML -> "<exp/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "exp"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.SINUS ->
    (match format with
    | Loggers.SBML -> "<sin/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "sin"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.COSINUS ->
    (match format with
    | Loggers.SBML -> "<cos/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "cos"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.TAN ->
    (match format with
    | Loggers.SBML -> "<tan/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "tan"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.INT ->
    (match format with
    | Loggers.SBML -> "<floor/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "floor"
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")

let string_of_compare_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.EQUAL ->
    (match format with
    | Loggers.SBML -> "<eq/>"
    | Loggers.Octave | Loggers.Matlab -> "=="
    | Loggers.DOTNET | Loggers.Maple | Loggers.Mathematica -> "="
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.DIFF ->
    (match format with
    | Loggers.SBML -> "<neq/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "!="
    | Loggers.Json | Loggers.DOT | Loggers.GEPHI | Loggers.Matrix
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.SMALLER ->
    (match format with
    | Loggers.SBML -> "<lt/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "<"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.GREATER ->
    (match format with
    | Loggers.SBML -> "<gt/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      ">"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")

let string_of_bin_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.MODULO ->
    (match format with
    | Loggers.SBML -> "<rem/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "mod"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.MAX ->
    (match format with
    | Loggers.SBML -> "<max/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "max"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.MIN ->
    (match format with
    | Loggers.SBML -> "<min/>"
    | Loggers.DOTNET | Loggers.Octave | Loggers.Matlab | Loggers.Maple
    | Loggers.Mathematica ->
      "min"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.DIV ->
    (match format with
    | Loggers.SBML -> "<divide/>"
    | Loggers.DOTNET | Loggers.Maple | Loggers.Mathematica | Loggers.Octave
    | Loggers.Matlab ->
      "/"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.SUM ->
    (match format with
    | Loggers.SBML -> "<plus/>"
    | Loggers.Maple | Loggers.Mathematica | Loggers.DOTNET | Loggers.Octave
    | Loggers.Matlab ->
      "+"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.MINUS ->
    (match format with
    | Loggers.SBML -> "<minus/>"
    | Loggers.Maple | Loggers.Mathematica | Loggers.DOTNET | Loggers.Octave
    | Loggers.Matlab ->
      "-"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.MULT ->
    (match format with
    | Loggers.SBML -> "<times/>"
    | Loggers.Maple | Loggers.Mathematica | Loggers.DOTNET | Loggers.Octave
    | Loggers.Matlab ->
      "*"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.POW ->
    (match format with
    | Loggers.SBML -> "<power/>"
    | Loggers.Maple | Loggers.Mathematica | Loggers.DOTNET | Loggers.Octave
    | Loggers.Matlab ->
      "**"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.HTML_Graph
    | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.GEPHI
    | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")

let string_of_bin_bool_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.AND ->
    (match format with
    | Loggers.SBML -> "<and/>"
    | Loggers.DOTNET | Loggers.Maple | Loggers.Mathematica | Loggers.Octave
    | Loggers.Matlab ->
      "&"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.HTML_Graph
    | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.GEPHI
    | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
  | Operator.OR ->
    (match format with
    | Loggers.SBML -> "<or/>"
    | Loggers.DOTNET | Loggers.Maple | Loggers.Mathematica | Loggers.Octave
    | Loggers.Matlab ->
      "|"
    | Loggers.Matrix | Loggers.Json | Loggers.DOT | Loggers.GEPHI
    | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
    | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")

let string_of_un_bool_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.NOT ->
    (match format with
    | Loggers.SBML -> "<not/>"
    | Loggers.DOTNET | Loggers.Maple | Loggers.Mathematica | Loggers.Octave
    | Loggers.Matlab ->
      "!"
    | Loggers.Json | Loggers.Matrix | Loggers.DOT | Loggers.HTML_Graph
    | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.GEPHI
    | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS ->
      "")
